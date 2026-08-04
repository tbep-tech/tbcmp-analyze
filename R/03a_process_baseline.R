# ---------------------------------------------------------------------------
# 03a_process_baseline.R
#
# Build the BASELINE basin summary from ./data/tbcmp_hem_filled.tif.
#
# Companion to:
#   03c_process_tiffs_basins.R        (per-scenario basin summaries)
#   04b_HEM_Change_Summary_by_Basin.R (consumes both)
#
# 04b derives its baseline by filtering the combined summaries for
# land_policy == "baseline" & yr == 2025. That row set has to come from
# somewhere, and the scenario rasters in data/output/raster/ do not contain
# it - the current condition lives in a single raster, data/tbcmp_hem_filled.tif.
# This script runs that one raster through the same zonal machinery as 03c and
# writes a CSV with matching schema and baseline metadata, so 04b picks it up
# with no changes.
#
# Basin geometry is NOT re-derived here
# -------------------------------------
# tbcmp_basins is loaded from data/tbcmp_basins.Rdata, written by 03c. Copying
# the dissolve into a second script would let the two drift, and a basin_id
# mismatch between baseline and scenarios produces a silent left_join failure
# in 04b rather than an error - every baseline_acres comes back NA and the
# deltas look like total habitat loss. Run 03c first; this script stops if the
# cache is absent.
#
# Output
# ------
#   data/output/basin/tbcmp_baseline_baseline_baseline_2025_basin_summary.csv
#
# The filename mirrors the county convention
# (tbcmp_baseline_baseline_baseline_2025_county_summary.csv) so that anything
# parsing name_policy_accretion_slr_year off the filename resolves correctly.
#
# Tampa Bay Estuary Program / Tampa Bay Coastal Master Plan
# ---------------------------------------------------------------------------

library(terra)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(here)

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
baseline_tif <- here("data/tbcmp_hem_filled.tif")
basins_rda   <- here("data/tbcmp_basins.Rdata")
coastal_rda  <- here("data/tbcmp_coastal.Rdata")
output_dir   <- here("data/output/basin/")
coast_csvdir <- here("data/output/coastal/")
zone_dir     <- here("data/zones/")
scratch_dir  <- here("data/scratch/")

# Metadata written into the CSV. 04b keys its baseline off land_policy and yr,
# so these strings matter more than the filename does.
baseline_tag <- list(
  filename     = "tbcmp_baseline_baseline_baseline_2025",
  land_policy  = "baseline",
  accretion    = "baseline",
  slr_scenario = "baseline",
  yr           = 2025
)

# Also write the baseline for the coastal residual, if 04b Part A has already
# built data/tbcmp_coastal.Rdata. Without this, 04b has no Coastal Waters
# baseline row and every coastal delta joins to NA. Skipped silently when the
# geometry does not exist yet - see the note at the end of this script.
do_coastal <- TRUE

force_zone_rebuild    <- FALSE
assume_crs_if_missing <- TRUE
overwrite             <- FALSE   # TRUE to regenerate an existing baseline CSV

# --- Memory settings -------------------------------------------------------
# Matches 03c. On 32 GB, memfrac 0.4 caps terra at ~12.8 GB with any single
# operation at 8 GB; GDAL's block cache is budgeted separately and is capped
# so the two cannot sum past available RAM.
if (!dir.exists(scratch_dir)) dir.create(scratch_dir, recursive = TRUE)
terraOptions(memfrac = 0.4, memmax = 8, tempdir = scratch_dir, progress = 0)
Sys.setenv(GDAL_CACHEMAX = "1024")

for (d in c(output_dir, zone_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# ---------------------------------------------------------------------------
# Helpers
#
# Copied from 03c_process_tiffs_basins.R. KEEP IN SYNC - if these diverge, the
# baseline and the scenarios stop being comparable. Worth extracting into a
# shared R/00_basin_utils.R that both scripts source.
# ---------------------------------------------------------------------------

crs_label <- function(x) {
  a <- try(st_crs(x), silent = TRUE)
  if (inherits(a, "try-error") || is.na(a$input)) return("undefined")
  if (!is.na(a$epsg)) paste0("EPSG:", a$epsg) else substr(a$input, 1, 40)
}

digest_crs <- function(r) {
  a <- crs(r, describe = TRUE)
  code <- if (!is.na(a$code)) paste0(a$authority, a$code) else "unknown"
  gsub("[^A-Za-z0-9]", "", code)
}

grid_signature <- function(r) {
  e <- as.vector(ext(r))
  paste0(
    paste(round(e, 3), collapse = "_"), "__",
    paste(round(res(r), 6), collapse = "x"), "__",
    substr(digest_crs(r), 1, 8)
  )
}

# Derived from the raster, never hard-coded: a fixed 0.000988422 silently
# rescales every acreage if a raster arrives at another resolution.
acres_per_cell <- function(r) {
  prod(res(r)) * 0.000247105381   # m^2 -> acres
}

# terra::rasterize does NOT reproject; a CRS mismatch yields an all-NA zone
# raster rather than an error.
align_units <- function(x, r) {
  if (st_crs(x) == st_crs(crs(r))) return(x)
  cat("  Reprojecting units:", crs_label(st_crs(x)$wkt),
      "->", crs_label(crs(r)), "\n")
  st_transform(x, crs(r))
}

check_overlap <- function(x, r, label = "") {
  bb <- st_bbox(x)
  re <- as.vector(ext(r))

  overlaps <- bb[["xmin"]] < re[2] && bb[["xmax"]] > re[1] &&
    bb[["ymin"]] < re[4] && bb[["ymax"]] > re[3]

  if (!overlaps) {
    cat("\n  EXTENT MISMATCH", label, "\n")
    cat("    raster CRS   :", crs_label(crs(r)), "\n")
    cat("    units CRS    :", crs_label(st_crs(x)$wkt), "\n")
    cat(sprintf("    raster extent: xmin %.1f  ymin %.1f  xmax %.1f  ymax %.1f\n",
                re[1], re[3], re[2], re[4]))
    cat(sprintf("    units extent : xmin %.1f  ymin %.1f  xmax %.1f  ymax %.1f\n",
                bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]))
  }

  overlaps
}

zone_has_values <- function(z, n = 20000) {
  s <- try(spatSample(z, size = n, method = "regular", na.rm = TRUE,
                      values = TRUE, warn = FALSE), silent = TRUE)
  if (inherits(s, "try-error")) return(NA)
  nrow(s) > 0
}

# HEM tiffs carry a raster attribute table, so anything reporting cell values
# returns the labels ("Estuarine Open Water") rather than the codes (5400).
strip_categories <- function(r) {
  if (!any(is.factor(r))) return(list(r = r, rat = NULL))
  rat <- try(cats(r)[[1]], silent = TRUE)
  if (inherits(rat, "try-error")) rat <- NULL
  levels(r) <- NULL
  list(r = r, rat = rat)
}

codes_from_rat <- function(raw, rat) {
  if (is.null(rat) || ncol(rat) < 2) return(rep(NA_real_, length(raw)))
  lut <- suppressWarnings(as.numeric(rat[[1]]))
  names(lut) <- as.character(rat[[2]])
  unname(lut[raw])
}

to_hem_codes <- function(raw, rat, context = "") {
  raw <- as.character(raw)
  num <- suppressWarnings(as.numeric(raw))

  if (anyNA(num)) num[is.na(num)] <- codes_from_rat(raw[is.na(num)], rat)

  if (anyNA(num)) {
    bad <- unique(raw[is.na(num)])
    stop("Could not resolve ", length(bad), " raster value(s) to numeric HEM ",
         "codes", context, ": ", paste(utils::head(bad, 8), collapse = ", "),
         if (length(bad) > 8) ", ..." else "")
  }

  num
}

# Zone raster keyed on grid signature, shared with 03c. If the baseline raster
# sits on the same grid as the scenario rasters this is a straight cache hit
# and no rasterizing happens at all.
get_zone_raster <- function(r, units, idx_field, prefix) {

  units <- align_units(units, r)

  if (!check_overlap(units, r, paste0("(", prefix, " vs raster)"))) {
    stop(prefix, " units do not overlap ", basename(sources(r)), ".")
  }

  zone_tif <- file.path(zone_dir,
                        paste0(prefix, "_", grid_signature(r), ".tif"))

  if (file.exists(zone_tif) && !force_zone_rebuild) {
    z <- rast(zone_tif)
    if (compareGeom(z, r, stopOnError = FALSE) && isTRUE(zone_has_values(z))) {
      cat("  Reusing cached zone raster:", basename(zone_tif), "\n")
      return(z)
    }
    cat("  Cached zone raster unusable; rebuilding\n")
    rm(z)
  }

  cat("  Rasterizing", nrow(units), prefix, "units (one time)\n")

  z <- rasterize(
    vect(units), r, field = idx_field,
    filename  = zone_tif,
    overwrite = TRUE,
    datatype  = "INT2U",
    gdal      = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES")
  )

  if (isFALSE(zone_has_values(z))) {
    stop("Zone raster empty after rasterize despite overlapping extents. ",
         "Inspect ", zone_tif, " in GIS.")
  }

  z
}

counts_crosstab <- function(r, z) {

  sc  <- strip_categories(r)
  r   <- sc$r
  rat <- sc$rat

  x <- c(z, r)
  names(x) <- c("zone", "hem")

  ct <- crosstab(x, long = TRUE, useNA = FALSE)

  if (ncol(ct) != 3) {
    stop("crosstab() returned ", ncol(ct), " columns, expected 3. Columns: ",
         paste(names(ct), collapse = ", "))
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

# ---------------------------------------------------------------------------
# Load the baseline raster
# ---------------------------------------------------------------------------
if (!file.exists(baseline_tif)) {
  stop("Baseline raster not found at: ", baseline_tif)
}

output_file <- file.path(output_dir,
                         paste0(baseline_tag$filename, "_basin_summary.csv"))

if (file.exists(output_file) && !overwrite) {
  stop("Baseline CSV already exists: ", output_file,
       "\nSet overwrite <- TRUE to regenerate.")
}

cat("=== Baseline:", basename(baseline_tif), "===\n")

hem <- rast(baseline_tif)

if (is.na(crs(hem)) || crs(hem) == "") {
  if (assume_crs_if_missing) {
    prj <- crs(rast(here("data/tbcmp_base_raster_10m.tif")))
    cat("  Raster has no CRS; assuming", crs_label(prj), "from base raster\n")
    crs(hem) <- prj
  } else {
    stop(basename(baseline_tif), " has no CRS defined.")
  }
}

cell_acres <- acres_per_cell(hem)

cat("  Grid:", paste(dim(hem)[1:2], collapse = " x "),
    "| res:", paste(res(hem), collapse = "x"),
    "| CRS:", crs_label(crs(hem)),
    "| acres/cell:", signif(cell_acres, 6), "\n")

# The baseline must be on the same grid as the scenarios or the acreages are
# not comparable. Warn rather than stop, since a differing grid is legitimate
# if the HEM outputs were exported separately - but it needs a decision.
scenario_tifs <- list.files(here("data/output/raster/"),
                            pattern = "\\.tif$|\\.tiff$", full.names = TRUE)
if (length(scenario_tifs) > 0) {
  ref <- rast(scenario_tifs[1])
  if (!compareGeom(hem, ref, stopOnError = FALSE)) {
    warning("Baseline raster is on a DIFFERENT grid to ",
            basename(scenario_tifs[1]),
            ". Acreages remain valid per raster but the zone raster cannot be ",
            "shared, and baseline/scenario differences will carry a ",
            "resampling artefact. Verify this is intended.")
  } else {
    cat("  Grid matches scenario rasters; zone raster cache is shared\n")
  }
  rm(ref)
}

if (any(is.factor(hem))) {
  rat_out <- try(cats(hem)[[1]], silent = TRUE)
  if (!inherits(rat_out, "try-error") && !is.null(rat_out)) {
    cat("  Categorical raster;", nrow(rat_out), "categories in attribute table\n")
  }
}

# ---------------------------------------------------------------------------
# Basin zonal counts
# ---------------------------------------------------------------------------
if (!file.exists(basins_rda)) {
  stop("Basin geometry cache not found at: ", basins_rda,
       "\nRun 03c_process_tiffs_basins.R first - it writes this file after ",
       "the dissolve. Re-deriving the dissolve here would risk basin_id ",
       "drift between baseline and scenarios.")
}

load(basins_rda)   # tbcmp_basins, with zone_idx and basin_id

if (!all(c("zone_idx", "basin_id") %in% names(tbcmp_basins))) {
  stop("tbcmp_basins is missing zone_idx or basin_id. Regenerate it by ",
       "rerunning 03c_process_tiffs_basins.R.")
}

cat("  Basin units:", nrow(tbcmp_basins), "\n")

basin_lut <- tbcmp_basins |>
  st_drop_geometry() |>
  select(zone_idx, basin_id, basin_name, basin_huc8, basin_huc, basin_feat)

zone_r <- get_zone_raster(hem, tbcmp_basins, "zone_idx", "zones")

t0     <- Sys.time()
counts <- counts_crosstab(hem, zone_r)

if (nrow(counts) == 0) {
  stop("No overlapping cells between the baseline raster and the basins.")
}

baseline_summary <- counts |>
  left_join(basin_lut, by = "zone_idx") |>
  mutate(
    acres        = count * cell_acres,
    filename     = baseline_tag$filename,
    land_policy  = baseline_tag$land_policy,
    accretion    = baseline_tag$accretion,
    slr_scenario = baseline_tag$slr_scenario,
    yr           = baseline_tag$yr
  ) |>
  rename(id = basin_id) |>
  select(id, value, count, basin_name, basin_huc8, basin_huc, basin_feat,
         filename, land_policy, accretion, slr_scenario, yr, acres) |>
  arrange(id, value)

write.csv(baseline_summary, output_file, row.names = FALSE)

cat("  ", nrow(baseline_summary), "rows |",
    round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min\n")
cat("Saved:", output_file, "\n\n")

# --- Checks -----------------------------------------------------------------
n_basins_hit <- n_distinct(baseline_summary$id)
cat("  Basins represented:", n_basins_hit, "of", nrow(tbcmp_basins), "\n")
if (n_basins_hit < nrow(tbcmp_basins)) {
  cat("  ", nrow(tbcmp_basins) - n_basins_hit,
      "basin(s) returned no cells - expected only for units smaller than a ",
      "cell or lying outside the raster footprint\n")
}

cat("\n  Baseline acres by HEM code:\n")
baseline_summary |>
  summarize(acres = sum(acres), .by = value) |>
  arrange(desc(acres)) |>
  mutate(acres = format(round(acres), big.mark = ",")) |>
  as.data.frame() |>
  print(row.names = FALSE)

cat("\n  Total baseline acres in basins:",
    format(round(sum(baseline_summary$acres)), big.mark = ","), "\n\n")

rm(counts)
tmpFiles(remove = TRUE)
invisible(gc())

# ---------------------------------------------------------------------------
# Coastal residual baseline (optional)
#
# 04b filters its baseline out of the combined basin + coastal summaries. With
# no coastal baseline row, every Coastal Waters delta in domain_change joins to
# NA and reads as if the residual had no starting condition. Runs only once
# 04b Part A has written data/tbcmp_coastal.Rdata.
# ---------------------------------------------------------------------------
if (do_coastal && file.exists(coastal_rda)) {

  cat("=== Coastal residual baseline ===\n")

  if (!dir.exists(coast_csvdir)) dir.create(coast_csvdir, recursive = TRUE)

  coast_file <- file.path(coast_csvdir,
                          paste0(baseline_tag$filename, "_coastal_summary.csv"))

  if (file.exists(coast_file) && !overwrite) {

    cat("  Exists, skipping:", basename(coast_file), "\n")

  } else {

    load(coastal_rda)   # tbcmp_coastal

    tbcmp_coastal <- tbcmp_coastal |>
      mutate(zone_idx = row_number())

    coast_lut <- tbcmp_coastal |>
      st_drop_geometry() |>
      select(zone_idx, unit_id, unit_name, unit_huc, unit_feat)

    cat("  Coastal units:", nrow(tbcmp_coastal), "\n")

    zone_c <- get_zone_raster(hem, tbcmp_coastal, "zone_idx", "zones_coastal")

    counts_c <- counts_crosstab(hem, zone_c)

    if (nrow(counts_c) == 0) {
      warning("No overlapping cells between the baseline raster and the ",
              "coastal residual; coastal baseline not written.")
    } else {

      coastal_summary <- counts_c |>
        left_join(coast_lut, by = "zone_idx") |>
        mutate(
          acres        = count * cell_acres,
          basin_huc8   = NA_character_,
          filename     = baseline_tag$filename,
          land_policy  = baseline_tag$land_policy,
          accretion    = baseline_tag$accretion,
          slr_scenario = baseline_tag$slr_scenario,
          yr           = baseline_tag$yr
        ) |>
        rename(id = unit_id, basin_name = unit_name,
               basin_huc = unit_huc, basin_feat = unit_feat) |>
        select(id, value, count, basin_name, basin_huc8, basin_huc, basin_feat,
               filename, land_policy, accretion, slr_scenario, yr, acres) |>
        arrange(id, value)

      write.csv(coastal_summary, coast_file, row.names = FALSE)
      cat("  ", nrow(coastal_summary), "rows saved:", coast_file, "\n")

      cat("\n  Reconciliation:\n")
      cat("    basin acres  :",
          format(round(sum(baseline_summary$acres)), big.mark = ","), "\n")
      cat("    coastal acres:",
          format(round(sum(coastal_summary$acres)), big.mark = ","), "\n")
      cat("    combined     :",
          format(round(sum(baseline_summary$acres) +
                         sum(coastal_summary$acres)), big.mark = ","), "\n")
      cat("    raster cells :",
          format(round(ncell(hem) * cell_acres), big.mark = ","),
          "(includes NoData)\n")
    }

    rm(counts_c)
    tmpFiles(remove = TRUE)
    invisible(gc())
  }

} else if (do_coastal) {
  cat("=== Coastal residual baseline skipped ===\n")
  cat("  ", coastal_rda, " not found. Run 04b Part A to build the coastal\n",
      "  geometry, then rerun this script to add the coastal baseline row.\n",
      sep = "")
}

cat("\nDone.\n")
