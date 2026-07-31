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

# Build (or reuse) the zone raster aligned to a given HEM raster
get_zone_raster <- function(r, basins) {
  sig      <- grid_signature(r)
  zone_tif <- file.path(zone_dir, paste0("zones_", sig, ".tif"))

  if (file.exists(zone_tif)) {
    z <- rast(zone_tif)
    if (compareGeom(z, r, stopOnError = FALSE)) {
      cat("  Reusing cached zone raster\n")
      return(z)
    }
    cat("  Cached zone raster does not align; rebuilding\n")
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

  z
}

# Streamed zonal counts via crosstab - no per-cell data enters R
counts_crosstab <- function(r, z) {
  x <- c(z, r)
  names(x) <- c("zone", "hem")

  ct <- crosstab(x, long = TRUE, useNA = FALSE)
  names(ct) <- c("zone_idx", "value", "count")

  ct |>
    mutate(
      zone_idx = as.integer(as.character(zone_idx)),
      value    = as.numeric(as.character(value)),
      count    = as.numeric(count)
    ) |>
    filter(!is.na(zone_idx), !is.na(value))
}

# Tiled exact_extract - partial-cell coverage, bounded memory per tile
counts_exact <- function(r, basins) {

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
      mutate(zone_idx = poly$zone_idx)

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
  cell_acres   <- acres_per_cell(tbcmp_raster)

  cat("  Grid:", paste(dim(tbcmp_raster)[1:2], collapse = " x "),
      "| res:", paste(res(tbcmp_raster), collapse = "x"),
      "| acres/cell:", signif(cell_acres, 6), "\n")

  if (method == "crosstab") {

    if (is.null(zone_r) || !compareGeom(zone_r, tbcmp_raster, stopOnError = FALSE)) {
      zone_r <- get_zone_raster(tbcmp_raster, tbcmp_basins)
    }

    counts <- counts_crosstab(tbcmp_raster, zone_r)

  } else if (method == "exact") {

    basin_sub <- st_transform(tbcmp_basins, crs(tbcmp_raster))
    counts    <- counts_exact(tbcmp_raster, basin_sub)
    rm(basin_sub)

  } else {
    stop("Unknown method: ", method, ". Use 'crosstab' or 'exact'.")
  }

  if (nrow(counts) == 0) {
    cat("  No overlapping cells found, skipping.\n\n")
    rm(tbcmp_raster, counts)
    tmpFiles(remove = TRUE)
    invisible(gc())
    next
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
