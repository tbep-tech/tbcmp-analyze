################################################################################
# Synthesize Hot Spot "map1" Rasters
# This script averages the 36 map1_exist_vuln.tif rasters in
# ./data/output/hot_spot (one per DEV x HA/LA x scenario x horizon-year
# combination) into a single mean raster and writes it back out.
################################################################################

# Load libraries
library(here)
library(terra)

################################################################################
# SECTION 1: Enumerate map1 Rasters
################################################################################

hot_spot_dir <- here("data/output/hot_spot")

map1_files <- list.files(
  path       = hot_spot_dir,
  pattern    = "_map1_exist_vuln\\.tif$",
  full.names = TRUE
)

if (length(map1_files) == 0) {
  stop("No map1 .tif files found in ", hot_spot_dir, ". Check the directory path.")
}

cat(sprintf("Found %d map1 raster(s) to synthesize.\n", length(map1_files)))

if (length(map1_files) != 36) {
  warning(sprintf(
    "Expected 36 map1 rasters but found %d — proceeding anyway.",
    length(map1_files)
  ))
}

################################################################################
# SECTION 2: Load and Stack Rasters
################################################################################

# Load first raster as the geometry reference
ref_raster <- rast(map1_files[1])

map1_stack <- rast(map1_files[1])

for (f in map1_files[-1]) {

  r <- tryCatch(
    rast(f),
    error = function(e) {
      message(sprintf("  ERROR loading %s: %s — skipping.", f, e$message))
      return(NULL)
    }
  )
  if (is.null(r)) next

  if (!compareGeom(ref_raster, r, stopOnError = FALSE)) {
    cat(sprintf("  Resampling %s to match reference geometry...\n", basename(f)))
    r <- resample(r, ref_raster, method = "bilinear")
  }

  map1_stack <- c(map1_stack, r)
}

cat(sprintf("Stacked %d layer(s) for identifying median.\n", nlyr(map1_stack)))

################################################################################
# SECTION 3: Max Across Layers
################################################################################

# na.rm = TRUE so that cells missing in a subset of scenarios/years still
# get a median from the layers that do have data
map1_median <- app(map1_stack, fun = median, na.rm = TRUE)

names(map1_median) <- "map1_exist_vuln_median"

################################################################################
# SECTION 4: Write Output
################################################################################

out_path <- file.path(hot_spot_dir, "tbcmp_hot_spot_map1_exist_vuln_median.tif")

writeRaster(
  map1_median,
  filename  = out_path,
  filetype  = "GTiff",
  overwrite = TRUE,
  datatype  = "FLT4S",
  wopt      = list(gdal = c("COMPRESS=LZW", "PREDICTOR=2"))
)

cat(sprintf("Saved median map1 raster -> %s\n", out_path))
cat("=== Hot spot map1 synthesis complete ===\n")
