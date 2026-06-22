# Load required libraries
library(terra)
library(exactextractr)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(nhdplusTools)
library(here)

# Define paths
geotiff_dir <- here("data/output/raster/")
output_dir  <- here("data/output/huc12/")

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Get list of all geoTIFF files
geotiff_files <- list.files(geotiff_dir,
                            pattern = "\\.tif$|\\.tiff$",
                            full.names = TRUE)

# ---------------------------------------------------------------------------
# Load HUC12 basins for the study area
# ---------------------------------------------------------------------------
# Derive study area extent from the base raster (used to spatially filter HUCs)
base_raster <- rast(here("data/tbcmp_base_raster_10m.tif"))
study_bbox  <- st_bbox(project(base_raster, "EPSG:4326")) |>
  st_as_sfc() |>
  st_as_sf() |>
  st_set_crs(4326)

cat("Fetching HUC12 basins from NHDPlus...\n")
tbcmp_huc12 <- get_huc(AOI = study_bbox, type = "huc12")

# Reproject HUC12 polygons to match raster CRS
tbcmp_huc12 <- st_transform(tbcmp_huc12, crs = crs(base_raster))

cat("Found", nrow(tbcmp_huc12), "HUC12 basins in study area\n\n")

rm(base_raster)
gc()

# ---------------------------------------------------------------------------
# Process each GeoTIFF
# ---------------------------------------------------------------------------
for (tiff_file in geotiff_files) {

  # Get filename for output naming
  tiff_name <- tools::file_path_sans_ext(basename(tiff_file))

  cat("Processing:", tiff_name, "\n")

  parts        <- str_split(tiff_name, "_")[[1]]
  name         <- parts[1]
  land_policy  <- if (length(parts) > 1) parts[2] else NA
  accretion    <- if (length(parts) > 2) parts[3] else NA
  slr_scenario <- if (length(parts) > 3) parts[4] else NA
  yr           <- if (length(parts) > 4) parts[5] else NA

  # Load the raster
  tbcmp_raster <- rast(tiff_file)

  # Filter HUC12 polygons to only those intersecting this raster's extent
  raster_ext_sf <- st_as_sf(as.polygons(ext(tbcmp_raster), crs = crs(tbcmp_raster)))
  huc12_sub <- st_filter(tbcmp_huc12, raster_ext_sf)

  if (nrow(huc12_sub) == 0) {
    cat("  No overlapping HUC12 basins found, skipping.\n\n")
    rm(tbcmp_raster); gc()
    next
  }

  # Initialize list for HUC12 results
  huc12_list <- list()

  # Loop through each HUC12 polygon
  for (i in seq_len(nrow(huc12_sub))) {

    # Print progress
    if (i %% 10 == 0) {
      cat("  Processing HUC12", i, "of", nrow(huc12_sub), "\n")
    }

    # Extract single polygon
    single_poly <- huc12_sub[i, ]

    # Crop and mask raster
    r_sub <- crop(tbcmp_raster, single_poly) |>
      mask(single_poly)

    # Extract values
    extracted <- exact_extract(r_sub, single_poly, fun = NULL, force_df = TRUE,
                               max_cells_in_memory = 3e+08)

    # Summarize data
    huc12_list[[i]] <- data.frame(id = single_poly$huc12, extracted) |>
      unnest(cols = everything()) |>
      group_by(id, value) |>
      summarise(count = sum(coverage_fraction, na.rm = TRUE),
                .groups = "drop") |>
      mutate(
        huc12_name   = single_poly$name,
        filename     = tiff_name,
        land_policy  = land_policy,
        accretion    = accretion,
        slr_scenario = slr_scenario,
        yr           = yr
      )

    # Clean up memory
    rm(extracted, r_sub)
    gc()
  }

  # Combine all HUC12 results
  huc12_summary <- do.call(rbind, huc12_list) |>
    mutate(acres = count * 0.000988422)   # 5x5m cells; use 0.000988422 for 2x2m outputs

  # Save results
  output_file <- file.path(output_dir, paste0(tiff_name, "_huc12_summary.csv"))
  write.csv(huc12_summary, output_file, row.names = FALSE)

  cat("Saved results to:", output_file, "\n\n")

  # Clean up raster from memory
  rm(tbcmp_raster)
  gc()
}

cat("All files processed successfully!\n")
