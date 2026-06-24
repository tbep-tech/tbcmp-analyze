################################################################################
# Batch sabre vmeasure_calc Analysis
# This script loops through all raster files in ./data/output/raster and
# calculates sabre::vmeasure_calc between each one and the baseline raster
# (./data/tbcmp_hem_filled.tif), writing hot spot output rasters whose
# names are derived from the comparison raster filename.
################################################################################

# Load libraries
library(here)
library(terra)
library(sf)
library(tidyverse)
library(sabre)

################################################################################
# SECTION 1: Load Shared Reference Data
################################################################################

# Baseline raster (time 1 — 2025 existing conditions)
raster_baseline <- rast("./data/tbcmp_hem_filled.tif")

# Base raster used to rescale inputs to a common 10 m grid
scaled_raster <- rast("./data/tbcmp_base_raster_10m.tif")

# HEM class lookup and reclassification table
hem_class  <- read_csv(file = here("data/hem_class_colors.csv")) |>
  select(Value, ClassName)
hem_recode <- read_csv(file = here("data/hem_recode.csv"), col_names = FALSE)

# Resample / reclassify the baseline once so it does not repeat inside the loop
if (!compareGeom(raster_baseline, scaled_raster, stopOnError = FALSE)) {
  cat("Resampling baseline raster to match scaled_raster...\n")
  raster_baseline <- resample(raster_baseline, scaled_raster, method = "mode")
  raster_baseline <- classify(raster_baseline, hem_recode)
  levels(raster_baseline) <- hem_class
}

# Ensure output directory exists
dir.create("./data/output/hot_spot", recursive = TRUE, showWarnings = FALSE)

################################################################################
# SECTION 2: Enumerate Comparison Rasters
################################################################################

comparison_files <- list.files(
  path       = "./data/output/raster",
  pattern    = "\\.tif$",
  full.names = TRUE
)

if (length(comparison_files) == 0) {
  stop("No .tif files found in ./data/output/raster. Check the directory path.")
}

cat(sprintf("Found %d comparison raster(s) to process.\n\n", length(comparison_files)))

################################################################################
# SECTION 3: Loop — vmeasure_calc for Each Comparison Raster
################################################################################

for (comp_path in comparison_files) {

  # Derive a clean stem from the comparison raster filename, e.g.
  # "tbcmp_PD_LA_IntHi_2080" from "tbcmp_PD_LA_IntHi_2080.tif"
  comp_stem <- tools::file_path_sans_ext(basename(comp_path))

  cat(sprintf("Processing: %s\n", comp_stem))

  # --- Load and prepare comparison raster -----------------------------------
  raster_comp <- tryCatch(
    rast(comp_path),
    error = function(e) {
      message(sprintf("  ERROR loading %s: %s — skipping.\n", comp_path, e$message))
      return(NULL)
    }
  )
  if (is.null(raster_comp)) next

  if (!compareGeom(raster_comp, scaled_raster, stopOnError = FALSE)) {
    cat("  Resampling comparison raster to match scaled_raster...\n")
    raster_comp <- resample(raster_comp, scaled_raster, method = "mode")
    raster_comp <- classify(raster_comp, hem_recode)
    levels(raster_comp) <- hem_class
  }

  # --- vmeasure_calc ---------------------------------------------------------
  cat("  Running vmeasure_calc...\n")
  lc_sabre <- tryCatch(
    vmeasure_calc(raster_baseline, raster_comp),
    error = function(e) {
      message(sprintf("  ERROR in vmeasure_calc for %s: %s — skipping.\n",
                      comp_stem, e$message))
      return(NULL)
    }
  )
  if (is.null(lc_sabre)) next

  cat(sprintf("  V-measure: %.4f  |  homogeneity: %.4f  |  completeness: %.4f\n",
              lc_sabre$V_measure, lc_sabre$homogeneity, lc_sabre$completeness))

  # --- Write map1 (Rih from baseline perspective) ---------------------------
  # Naming mirrors the comparison raster stem, prefixed to indicate direction:
  #   map1 → existing conditions vulnerability relative to <comp_stem>
  #   map2 → future difference of <comp_stem> relative to baseline
  out_map1 <- file.path(
    "./data/output/hot_spot",
    paste0(comp_stem, "_map1_exist_vuln.tif")
  )

  writeRaster(
    lc_sabre$map1[[2]],
    filename = out_map1,
    filetype = "GTiff",
    overwrite = TRUE,
    datatype  = "FLT4S",
    wopt      = list(gdal = c("COMPRESS=LZW", "PREDICTOR=2"))
  )
  cat(sprintf("  Saved map1 → %s\n", out_map1))

  # --- Write map2 (Rih from comparison raster perspective) ------------------
  out_map2 <- file.path(
    "./data/output/hot_spot",
    paste0(comp_stem, "_map2_future_diff.tif")
  )

  writeRaster(
    lc_sabre$map2[[2]],
    filename = out_map2,
    filetype = "GTiff",
    overwrite = TRUE,
    datatype  = "FLT4S",
    wopt      = list(gdal = c("COMPRESS=LZW", "PREDICTOR=2"))
  )
  cat(sprintf("  Saved map2 → %s\n\n", out_map2))

  # Free memory before next iteration
  rm(raster_comp, lc_sabre)
  gc()
}

cat("=== Batch vmeasure_calc complete ===\n")
