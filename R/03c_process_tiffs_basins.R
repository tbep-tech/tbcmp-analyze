# ---------------------------------------------------------------------------
# 03c_process_tiffs_basins.R
#
# Summarize HEM GeoTIFF outputs by SWFWMD/FDEP drainage basin.
#
# Companion to:
#   03_process_tiffs.R                (summaries by county)
#   03b_process_tiffs_huc12.R         (summaries by NHDPlus HUC12)
#   04b_HEM_Change_Summary_by_Basin.R (consumes the CSVs written here)
#
# Basins are read from a local shapefile
# (./data-raw/swfwmd/Drainage_Basin_Boundaries.shp); no network call needed.
#
# REVISION 1 - dissolve includes HUC8
# -----------------------------------
# EXTHUC is unique only WITHIN a HUC8. Dissolving on EXTHUC + BASIN + FEATURE
# merged basins across unrelated watersheds: EXTHUC 99990000 /
# "DIRECT RUNOFF TO BAY" occurs 51 times across five HUC8s.
#
# REVISION 2 - FEATURE removed from the dissolve key
# --------------------------------------------------
# Grouping on FEATURE while building basin_id from HUC8 + EXTHUC + BASIN
# produced two rows with one id (03100206_99990000_DIR_RUNOFF_TO_BAY). FEATURE
# is now aggregated, not grouped on. Unit counts from 1,166 source records:
#   EXTHUC + BASIN + FEATURE          1,061  (original, incorrect)
#   HUC8 + EXTHUC + BASIN + FEATURE   1,070  (id collision)
#   HUC8 + EXTHUC + BASIN             1,069
#   ... with name normalization       1,068  (default)
#
# REVISION 3 - memory
# -------------------
# Earlier versions killed the R session rather than erroring. Causes:
#
#   1. exact_extract(fun = NULL) returns ONE ROW PER CELL. Tampa Bay is
#      ~259M cells at 2m, so ~4.1 GB for value + coverage_fraction. Then
#      data.frame(id, extracted) copied it, unnest() copied it again, and
#      group_by |> summarise() copied it again - ~16 GB live for one polygon.
#   2. max_cells_in_memory = 3e+08 is 10x the terra default, adding ~2.4 GB
#      of raster chunk on top.
#   3. crop() |> mask() allocated a masked raster that exact_extract does not
#      need, since it computes its own coverage fractions.
#
# Two extraction methods are now available:
#
#   method = "crosstab" (default) - rasterize the basins ONCE per raster grid,
#     then terra::crosstab() the zone raster against each HEM raster. terra
#     streams both by block, so peak R memory is a few hundred MB regardless
#     of raster size, and there is one pass per tiff instead of 1,068
#     crop/mask/extract cycles. Counts are whole-cell.
#
#   method = "exact" - exact_extract with partial-cell coverage fractions,
#     preserved for methodological parity with 03/03b, but tiled so the
#     per-cell data frame is bounded by tile size rather than basin size.
#
# The two differ only in how boundary cells are handled. At 2m resolution
# against basins of thousands of acres that is a rounding error, but "exact"
# is there if the county and HUC12 summaries need to be matched exactly.
#
# REVISION 4 - CRS bug in the crosstab path
# -----------------------------------------
# Revision 3 reported "No overlapping cells found" for every tiff. Cause:
# tbcmp_basins is transformed once to the CRS of tbcmp_base_raster_10m.tif
# (EPSG:3087), and the exact path re-projected per raster, but the crosstab
# path passed those 3087 basins straight into rasterize() against the HEM
# tiff. terra::rasterize does NOT reproject - a CRS mismatch yields an all-NA
# zone raster rather than an error, so crosstab returned zero rows and the
# script logged an empty result instead of failing.
#
# Basins are now aligned to each raster's CRS before rasterizing, extents are
# checked before the pass, the zone raster is sampled to confirm it carries
# values, and zero counts raise an error instead of being skipped. Untagged
# rasters are handled explicitly rather than defaulting to "no overlap".
#
# Delete data/zones/ before rerunning - any zone raster cached by revision 3
# is all NA. The script now detects and rebuilds these, but removing them is
# faster than sampling each one.
#
# REVISION 5 - categorical rasters returned labels, not codes
# -----------------------------------------------------------
# After Revision 4 the zone raster was populated and extents overlapped, but
# every tiff still produced zero rows, with:
#
#   Warning: In argument `value = as.numeric(as.character(value))`
#            NAs introduced by coercion
#
# The HEM tiffs carry a raster attribute table, so crosstab() reported the
# category LABELS ("Estuarine Open Water") rather than the numeric codes
# (5400). Coercion turned every label into NA and filter(!is.na(value))
# discarded the whole result.
#
# strip_categories() now detaches the RAT before counting so the underlying
# integer codes come through, with to_hem_codes() falling back to a RAT
# lookup and erroring - rather than returning NA - if a value still will not
# resolve. Both the crosstab and exact paths are covered, and the attribute
# table is archived once to data/output/basin/_hem_raster_attribute_table.csv.
#
# Tampa Bay Estuary Program / Tampa Bay Coastal Master Plan
# ---------------------------------------------------------------------------

library(terra)
library(exactextractr)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(here)

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
geotiff_dir <- here("data/output/raster/")
output_dir  <- here("data/output/basin/")
basin_shp   <- here("data-raw/swfwmd/Drainage_Basin_Boundaries.shp")
zone_dir    <- here("data/zones/")     # cached zone rasters
scratch_dir <- here("data/scratch/")   # terra temp files

# "crosstab" (streamed, whole-cell) or "exact" (tiled, partial-cell coverage)
method <- "crosstab"

# Tile size in metres, method = "exact" only. 5000m at 2m resolution is
# 6.25M cells per tile, roughly 100 MB per extract call.
tile_size_m <- 5000

# Skip tiffs whose CSV already exists. Leave TRUE so a crash does not discard
# completed work - rerun and it picks up where it stopped.
resume <- TRUE

# Ignore any cached zone raster and rebuild. Set TRUE once after upgrading
# from a version that cached an all-NA zone raster (see Revision 4).
force_zone_rebuild <- FALSE

# Some HEM/SLAMM exports ship without a CRS tag. If TRUE, an untagged raster
# is assumed to be on the same CRS as tbcmp_base_raster_10m.tif rather than
# failing. Set FALSE to make an untagged raster a hard error.
assume_crs_if_missing <- TRUE

# Normalize name variants ("DIR RUNOFF" -> "DIRECT RUNOFF", squish whitespace)
normalize_basin_names <- TRUE

# --- Memory settings -------------------------------------------------------
# On 32 GB, memfrac 0.4 caps terra at ~12.8 GB and memmax caps any single
# operation at 8 GB, leaving headroom for R's own copies and the OS. Raising
# these is what causes the OOM kill; they are deliberately conservative.
if (!dir.exists(scratch_dir)) dir.create(scratch_dir, recursive = TRUE)
terraOptions(
  memfrac  = 0.4,
  memmax   = 8,
  tempdir  = scratch_dir,
  progress = 0
)

# GDAL's block cache is separate from terra's budget and defaults to a share
# of total RAM. Cap it so the two cannot sum past what is available.
Sys.setenv(GDAL_CACHEMAX = "1024")   # MB

for (d in c(output_dir, zone_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

geotiff_files <- list.files(geotiff_dir, pattern = "\\.tif$|\\.tiff$",
                            full.names = TRUE)

if (length(geotiff_files) == 0) {
  stop("No GeoTIFFs found in ", geotiff_dir)
}

# ---------------------------------------------------------------------------
# Load and dissolve drainage basins
# ---------------------------------------------------------------------------
if (!file.exists(basin_shp)) {
  stop("Drainage basin shapefile not found at: ", basin_shp,
       "\nDownload from https://data-swfwmd.opendata.arcgis.com/datasets/drainage-basin-boundaries")
}

base_raster <- rast(here("data/tbcmp_base_raster_10m.tif"))
prj         <- crs(base_raster)

study_bbox <- st_bbox(base_raster) |>
  st_as_sfc() |>
  st_as_sf() |>
  st_set_crs(prj)

rm(base_raster)
invisible(gc())

cat("Reading drainage basins from:", basin_shp, "\n")

# Source layer is NAD83(HARN) / StatePlane Florida West ftUS
tbcmp_basins <- st_read(basin_shp, quiet = TRUE) |>
  st_transform(prj) |>
  st_make_valid()

cat("  Read", nrow(tbcmp_basins), "polygon records\n")

# --- Resolve attribute names defensively -----------------------------------
# Expected SWFWMD/FDEP schema:
#   OBJECTID, HUC, EXTHUC, BASIN, FEATURE, SQ_MILES, DATESTAMP
nms        <- names(tbcmp_basins)
name_fld   <- nms[toupper(nms) %in% c("BASIN", "BASIN_NAME", "NAME")][1]
huc8_fld   <- nms[toupper(nms) %in% c("HUC", "HUC8")][1]
exthuc_fld <- nms[toupper(nms) %in% c("EXTHUC", "EXT_HUC")][1]
feat_fld   <- nms[toupper(nms) %in% c("FEATURE", "FEAT_TYPE")][1]

if (is.na(name_fld)) {
  stop("Could not find a basin name field. Fields present: ",
       paste(nms, collapse = ", "))
}
if (is.na(huc8_fld)) {
  warning("No HUC8 field found. Basins sharing an EXTHUC across watersheds ",
          "will be merged. Fields present: ", paste(nms, collapse = ", "))
}

tbcmp_basins <- tbcmp_basins |>
  mutate(
    basin_name = as.character(.data[[name_fld]]),
    basin_huc8 = if (!is.na(huc8_fld))   as.character(.data[[huc8_fld]])   else NA_character_,
    basin_huc  = if (!is.na(exthuc_fld)) as.character(.data[[exthuc_fld]]) else NA_character_,
    basin_feat = if (!is.na(feat_fld))   as.character(.data[[feat_fld]])   else NA_character_
  ) |>
  select(basin_name, basin_huc8, basin_huc, basin_feat)   # drop unused columns

# --- Normalize basin names --------------------------------------------------
# The layer carries abbreviation variants that split one unit:
# "DIR RUNOFF TO BAY" (5 records) alongside "DIRECT RUNOFF TO BAY" (52), same
# HUC8 and EXTHUC. One name has a doubled internal space ("LEMMON STREET
# DITCH"). Merges one pair of units in HUC8 03100206 (1,069 -> 1,068).
if (normalize_basin_names) {
  n_before <- tbcmp_basins |>
    st_drop_geometry() |>
    distinct(basin_huc8, basin_huc, basin_name) |>
    nrow()

  tbcmp_basins <- tbcmp_basins |>
    mutate(
      basin_name = str_squish(basin_name),
      basin_name = str_replace(basin_name, "^DIR\\s+RUNOFF", "DIRECT RUNOFF")
    )

  n_after <- tbcmp_basins |>
    st_drop_geometry() |>
    distinct(basin_huc8, basin_huc, basin_name) |>
    nrow()

  cat("  Name normalization merged", n_before - n_after, "unit(s)\n")
}

# --- Dissolve ---------------------------------------------------------------
# FEATURE is deliberately NOT a grouping key - it describes the hydrologic
# feature type, not basin identity. Grouping on it while building basin_id
# from HUC8 + EXTHUC + BASIN produced a duplicate id (see Revision 2). Where a
# unit spans multiple FEATURE values they are concatenated.
id_keys <- c("basin_huc8", "basin_huc", "basin_name")

tbcmp_basins <- tbcmp_basins |>
  group_by(across(all_of(id_keys))) |>
  summarise(
    basin_feat = paste(sort(unique(basin_feat)), collapse = "/"),
    n_parts    = n(),
    .groups    = "drop"
  ) |>
  st_make_valid() |>
  mutate(
    basin_id = paste0(
      coalesce(basin_huc8, "NA"), "_",
      coalesce(basin_huc, "NA"), "_",
      str_replace_all(basin_name, "\\s+", "_")
    )
  )

cat("  Dissolved to", nrow(tbcmp_basins), "unique basin units\n")

# basin_id must be unique. If this fires, the columns used to build basin_id
# have drifted from id_keys - reconcile the two rather than lengthening the id.
if (any(duplicated(tbcmp_basins$basin_id))) {
  dupes <- tbcmp_basins |>
    st_drop_geometry() |>
    filter(basin_id %in% basin_id[duplicated(basin_id)]) |>
    select(basin_id, all_of(id_keys), basin_feat, n_parts) |>
    arrange(basin_id)
  print(as.data.frame(dupes), row.names = FALSE)
  stop("Duplicate basin_id values after dissolve (", nrow(dupes), " rows). ",
       "basin_id must be built from exactly the columns in id_keys.")
}

tbcmp_basins <- st_filter(tbcmp_basins, study_bbox)

cat("Found", nrow(tbcmp_basins), "drainage basins in study area\n")

if (nrow(tbcmp_basins) == 0) {
  stop("No drainage basins intersect the project area. Check the shapefile CRS.")
}

# Integer index for the zone raster (INT2U tops out at 65,535; ~1,068 needed)
tbcmp_basins <- tbcmp_basins |>
  mutate(zone_idx = row_number())

if (nrow(tbcmp_basins) > 65535) {
  stop("More basins than INT2U can hold; change the rasterize datatype to INT4U.")
}

# Attribute lookup, geometry dropped - joined back to counts later
basin_lut <- tbcmp_basins |>
  st_drop_geometry() |>
  select(zone_idx, basin_id, basin_name, basin_huc8, basin_huc, basin_feat)

save(tbcmp_basins, file = here("data/tbcmp_basins.Rdata"))

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Signature identifying a raster grid, so the zone raster is rebuilt only when
# a tiff arrives on a different grid rather than once per tiff.
grid_signature <- function(r) {
  e <- as.vector(ext(r))
  paste0(
    paste(round(e, 3), collapse = "_"), "__",
    paste(round(res(r), 6), collapse = "x"), "__",
    substr(digest_crs(r), 1, 8)
  )
}

digest_crs <- function(r) {
  a <- crs(r, describe = TRUE)
  code <- if (!is.na(a$code)) paste0(a$authority, a$code) else "unknown"
  gsub("[^A-Za-z0-9]", "", code)
}

# Acres per cell, derived from the raster rather than hard-coded. The old
# literal 0.000988422 assumes 2x2m; if a tiff is delivered at another
# resolution a fixed constant silently scales every acreage in the report.
acres_per_cell <- function(r) {
  rr <- res(r)
  prod(rr) * 0.000247105381   # m^2 -> acres
}

# Put the basins on the raster's CRS. terra::rasterize does NOT reproject: a
# CRS mismatch yields an all-NA zone raster instead of an error, which is what
# produced "No overlapping cells found" on every tiff.
align_basins <- function(basins, r) {
  if (st_crs(basins) == st_crs(crs(r))) return(basins)
  cat("  Reprojecting basins:", crs_label(st_crs(basins)$wkt),
      "->", crs_label(crs(r)), "\n")
  st_transform(basins, crs(r))
}

crs_label <- function(x) {
  a <- try(st_crs(x), silent = TRUE)
  if (inherits(a, "try-error") || is.na(a$input)) return("undefined")
  if (!is.na(a$epsg)) paste0("EPSG:", a$epsg) else substr(a$input, 1, 40)
}

# Confirm the aligned basins actually fall on the raster before rasterizing.
# Catches both a genuine footprint mismatch and a CRS that was reprojected to
# the wrong thing.
check_overlap <- function(basins, r, label = "") {
  bb <- st_bbox(basins)
  re <- as.vector(ext(r))

  overlaps <- bb[["xmin"]] < re[2] && bb[["xmax"]] > re[1] &&
              bb[["ymin"]] < re[4] && bb[["ymax"]] > re[3]

  if (!overlaps) {
    re_v <- as.vector(ext(r))
    cat("\n  EXTENT MISMATCH", label, "\n")
    cat("    raster CRS   :", crs_label(crs(r)), "\n")
    cat("    basins CRS   :", crs_label(st_crs(basins)$wkt), "\n")
    cat(sprintf("    raster extent: xmin %.1f  ymin %.1f  xmax %.1f  ymax %.1f\n",
                re_v[1], re_v[3], re_v[2], re_v[4]))
    cat(sprintf("    basins extent: xmin %.1f  ymin %.1f  xmax %.1f  ymax %.1f\n",
                bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]))
  }

  overlaps
}

# Cheap check that a zone raster actually carries values. A full pass over a
# multi-billion-cell grid is not worth it; a stratified sample is enough to
# distinguish "all NA" from "populated".
zone_has_values <- function(z, n = 20000) {
  s <- try(spatSample(z, size = n, method = "regular", na.rm = TRUE,
                      values = TRUE, warn = FALSE), silent = TRUE)
  if (inherits(s, "try-error")) return(NA)
  nrow(s) > 0
}

# Build (or reuse) the zone raster aligned to a given HEM raster
get_zone_raster <- function(r, basins) {

  # Reproject FIRST - everything downstream depends on this
  basins <- align_basins(basins, r)

  if (!check_overlap(basins, r, "(basins vs raster)")) {
    stop("Basins do not overlap ", basename(sources(r)), ". ",
         "Check that the HEM outputs and tbcmp_base_raster_10m.tif are on ",
         "the same CRS, or set assume_crs_if_missing if the tiffs are untagged.")
  }

  sig      <- grid_signature(r)
  zone_tif <- file.path(zone_dir, paste0("zones_", sig, ".tif"))

  if (file.exists(zone_tif) && !force_zone_rebuild) {
    z <- rast(zone_tif)
    if (compareGeom(z, r, stopOnError = FALSE)) {
      ok <- zone_has_values(z)
      if (isTRUE(ok)) {
        cat("  Reusing cached zone raster\n")
        return(z)
      }
      cat("  Cached zone raster is empty (built before the CRS fix); rebuilding\n")
    } else {
      cat("  Cached zone raster does not align; rebuilding\n")
    }
    rm(z)
  }

  cat("  Rasterizing", nrow(basins), "basins to the raster grid (one time)\n")

  # filename= streams to disk instead of building the whole grid in memory
  z <- rasterize(
    vect(basins), r, field = "zone_idx",
    filename  = zone_tif,
    overwrite = TRUE,
    datatype  = "INT2U",
    gdal      = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES")
  )

  # Fail here rather than silently writing empty CSVs for every scenario
  if (isFALSE(zone_has_values(z))) {
    stop("Zone raster came back empty after rasterize despite overlapping ",
         "extents. Inspect ", zone_tif, " in GIS before rerunning.")
  }

  z
}

# HEM tiffs ship with a raster attribute table. Any operation that reports
# cell values then returns the category LABELS ("Estuarine Open Water")
# instead of the numeric codes (5400). Detaching the RAT exposes the
# underlying integer codes; this is a metadata edit on the in-memory
# SpatRaster and does not rewrite the file on disk.
strip_categories <- function(r) {
  if (!any(is.factor(r))) return(list(r = r, rat = NULL))

  rat <- try(cats(r)[[1]], silent = TRUE)
  if (inherits(rat, "try-error")) rat <- NULL

  levels(r) <- NULL

  list(r = r, rat = rat)
}

# Recover numeric codes from label strings using the RAT, for the case where
# a value still is not numeric after the RAT has been detached.
codes_from_rat <- function(raw, rat) {
  if (is.null(rat) || ncol(rat) < 2) return(rep(NA_real_, length(raw)))
  lut <- suppressWarnings(as.numeric(rat[[1]]))
  names(lut) <- as.character(rat[[2]])
  unname(lut[raw])
}

# Coerce the crosstab value column to numeric HEM codes, failing loudly rather
# than silently NA-ing rows out of the result.
to_hem_codes <- function(raw, rat, context = "") {
  raw <- as.character(raw)
  num <- suppressWarnings(as.numeric(raw))

  if (anyNA(num)) {
    num[is.na(num)] <- codes_from_rat(raw[is.na(num)], rat)
  }

  if (anyNA(num)) {
    bad <- unique(raw[is.na(num)])
    stop("Could not resolve ", length(bad), " raster value(s) to numeric HEM ",
         "codes", context, ": ",
         paste(utils::head(bad, 8), collapse = ", "),
         if (length(bad) > 8) ", ..." else "",
         "\nThe raster attribute table may use a label column that does not ",
         "map to codes. Inspect cats(rast(<tiff>)) and adjust codes_from_rat().")
  }

  num
}

# Streamed zonal counts via crosstab - no per-cell data enters R
counts_crosstab <- function(r, z) {

  sc  <- strip_categories(r)
  r   <- sc$r
  rat <- sc$rat

  x <- c(z, r)
  names(x) <- c("zone", "hem")

  ct <- crosstab(x, long = TRUE, useNA = FALSE)

  if (ncol(ct) != 3) {
    stop("crosstab() returned ", ncol(ct), " columns, expected 3 ",
         "(zone, hem, Freq). Columns: ", paste(names(ct), collapse = ", "))
  }
  names(ct) <- c("zone_idx", "value", "count")

  ct |>
    mutate(
      zone_idx = as.integer(as.character(zone_idx)),
      value    = to_hem_codes(value, rat, " (crosstab)"),
      count    = as.numeric(count)
    ) |>
    filter(!is.na(zone_idx), !is.na(value))
}

# Tiled exact_extract - partial-cell coverage, bounded memory per tile
counts_exact <- function(r, basins) {

  # Same RAT problem as crosstab: a categorical raster yields labels
  sc  <- strip_categories(r)
  r   <- sc$r
  rat <- sc$rat

  out <- vector("list", nrow(basins))

  for (i in seq_len(nrow(basins))) {

    if (i %% 25 == 0) cat("    basin", i, "of", nrow(basins), "\n")

    poly <- basins[i, ]

    tiles <- st_make_grid(poly, cellsize = tile_size_m) |>
      st_as_sf() |>
      st_filter(poly) |>
      st_intersection(st_geometry(poly)) |>
      st_make_valid()

    tiles <- tiles[!st_is_empty(st_geometry(tiles)), ]
    if (nrow(tiles) == 0) next

    tile_out <- vector("list", nrow(tiles))

    for (j in seq_len(nrow(tiles))) {

      # No crop/mask: exact_extract computes coverage fractions itself.
      # max_cells_in_memory left at the terra default (3e7); raising it was
      # part of what pushed the session over.
      ex <- try(
        exact_extract(r, tiles[j, ], fun = NULL, force_df = TRUE,
                      max_cells_in_memory = 3e7, progress = FALSE),
        silent = TRUE
      )
      if (inherits(ex, "try-error")) next

      # Collapse immediately - the per-cell frame must not survive the
      # iteration, which is what made the old version fatal
      tile_out[[j]] <- bind_rows(ex) |>
        filter(!is.na(value)) |>
        group_by(value) |>
        summarise(count = sum(coverage_fraction, na.rm = TRUE), .groups = "drop")

      rm(ex)
    }

    tile_out <- tile_out[!vapply(tile_out, is.null, logical(1))]
    if (length(tile_out) == 0) next

    out[[i]] <- bind_rows(tile_out) |>
      group_by(value) |>
      summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
      mutate(
        value    = to_hem_codes(value, rat, " (exact_extract)"),
        zone_idx = poly$zone_idx
      )

    rm(tile_out)
    if (i %% 50 == 0) invisible(gc())
  }

  bind_rows(out)
}

# ---------------------------------------------------------------------------
# Process each GeoTIFF
# ---------------------------------------------------------------------------
cat("\nMethod:", method, "| rasters:", length(geotiff_files), "\n\n")

zone_r <- NULL

for (tiff_file in geotiff_files) {

  tiff_name   <- tools::file_path_sans_ext(basename(tiff_file))
  output_file <- file.path(output_dir, paste0(tiff_name, "_basin_summary.csv"))

  if (resume && file.exists(output_file)) {
    cat("Skipping (exists):", tiff_name, "\n")
    next
  }

  cat("Processing:", tiff_name, "\n")
  t0 <- Sys.time()

  parts        <- str_split(tiff_name, "_")[[1]]
  land_policy  <- if (length(parts) > 1) parts[2] else NA
  accretion    <- if (length(parts) > 2) parts[3] else NA
  slr_scenario <- if (length(parts) > 3) parts[4] else NA
  yr           <- if (length(parts) > 4) parts[5] else NA

  tbcmp_raster <- rast(tiff_file)

  # An untagged raster cannot be reprojected against, and silently defaults to
  # "no overlap". Tag it from the base raster or stop, per configuration.
  if (is.na(crs(tbcmp_raster)) || crs(tbcmp_raster) == "") {
    if (assume_crs_if_missing) {
      cat("  Raster has no CRS; assuming", crs_label(prj), "from base raster\n")
      crs(tbcmp_raster) <- prj
    } else {
      stop(tiff_name, " has no CRS defined. Tag it, or set ",
           "assume_crs_if_missing <- TRUE.")
    }
  }

  cell_acres <- acres_per_cell(tbcmp_raster)

  cat("  Grid:", paste(dim(tbcmp_raster)[1:2], collapse = " x "),
      "| res:", paste(res(tbcmp_raster), collapse = "x"),
      "| CRS:", crs_label(crs(tbcmp_raster)),
      "| acres/cell:", signif(cell_acres, 6), "\n")

  # Archive the raster attribute table once. Confirms the label -> code
  # mapping used to recover numeric HEM values, and is worth eyeballing
  # against data/hem_class_colors.csv.
  if (any(is.factor(tbcmp_raster))) {
    rat_file <- file.path(output_dir, "TBCMP_hem_raster_attribute_table.csv")
    if (!file.exists(rat_file)) {
      rat_out <- try(cats(tbcmp_raster)[[1]], silent = TRUE)
      if (!inherits(rat_out, "try-error") && !is.null(rat_out)) {
        write.csv(rat_out, rat_file, row.names = FALSE)
        cat("  Categorical raster;", nrow(rat_out),
            "categories written to", basename(rat_file), "\n")
      }
    } else {
      cat("  Categorical raster; codes recovered from attribute table\n")
    }
  }

  if (method == "crosstab") {

    if (is.null(zone_r) || !compareGeom(zone_r, tbcmp_raster, stopOnError = FALSE)) {
      zone_r <- get_zone_raster(tbcmp_raster, tbcmp_basins)
    }

    counts <- counts_crosstab(tbcmp_raster, zone_r)

  } else if (method == "exact") {

    basin_sub <- align_basins(tbcmp_basins, tbcmp_raster)

    if (!check_overlap(basin_sub, tbcmp_raster, "(basins vs raster)")) {
      stop("Basins do not overlap ", tiff_name, ".")
    }

    counts <- counts_exact(tbcmp_raster, basin_sub)
    rm(basin_sub)

  } else {
    stop("Unknown method: ", method, ". Use 'crosstab' or 'exact'.")
  }

  # Zero rows here is a configuration problem, not a legitimate empty result -
  # the overlap check above already passed. Stop rather than writing an empty
  # CSV that resume would then treat as complete on the next run.
  if (nrow(counts) == 0) {
    re_v <- as.vector(ext(tbcmp_raster))
    bb_v <- st_bbox(tbcmp_basins)
    cat("\n  DIAGNOSTIC\n")
    cat("    raster CRS   :", crs_label(crs(tbcmp_raster)), "\n")
    cat("    basins CRS   :", crs_label(st_crs(tbcmp_basins)$wkt), "\n")
    cat(sprintf("    raster extent: xmin %.1f  ymin %.1f  xmax %.1f  ymax %.1f\n",
                re_v[1], re_v[3], re_v[2], re_v[4]))
    cat(sprintf("    basins extent: xmin %.1f  ymin %.1f  xmax %.1f  ymax %.1f\n",
                bb_v[["xmin"]], bb_v[["ymin"]], bb_v[["xmax"]], bb_v[["ymax"]]))
    cat("    raster categorical:", any(is.factor(tbcmp_raster)), "\n")
    if (method == "crosstab") {
      cat("    zone raster populated:", zone_has_values(zone_r), "\n")
      cat("    zone raster file     :", sources(zone_r), "\n")
    }
    stop("No overlapping cells for ", tiff_name,
         " despite passing the extent check. See diagnostic above.")
  }

  basin_summary <- counts |>
    left_join(basin_lut, by = "zone_idx") |>
    mutate(
      acres        = count * cell_acres,
      filename     = tiff_name,
      land_policy  = land_policy,
      accretion    = accretion,
      slr_scenario = slr_scenario,
      yr           = yr
    ) |>
    rename(id = basin_id) |>
    select(id, value, count, basin_name, basin_huc8, basin_huc, basin_feat,
           filename, land_policy, accretion, slr_scenario, yr, acres) |>
    arrange(id, value)

  write.csv(basin_summary, output_file, row.names = FALSE)

  cat("  ", nrow(basin_summary), "rows |",
      round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min\n")
  cat("Saved results to:", output_file, "\n\n")

  # Release the raster, clear terra's scratch files, then collect. Skipping
  # the tmpFiles() call fills the scratch directory across a long run.
  rm(tbcmp_raster, counts, basin_summary)
  tmpFiles(remove = TRUE)
  invisible(gc())
}

cat("All files processed successfully!\n")
cat("Zone rasters cached in:", zone_dir,
    "- safe to delete once the run is complete.\n")
