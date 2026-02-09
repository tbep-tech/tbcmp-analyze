# Load required libraries
library(terra)
library(exactextractr)
library(dplyr)
library(tidyr)
library(stringr)
library(here)

# Define paths
geotiff_dir <- "T:/05_GIS/TBEP/TBCMP/OUTPUTS/LOW_ACCRETION_SCENARIOS"
output_dir <- here("data/output/")

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Get list of all geoTIFF files
geotiff_files <- list.files(geotiff_dir,
                            pattern = "\\.tif$|\\.tiff$",
                            full.names = TRUE)

# Load your county polygons (assuming this is already loaded)
#tbcmp_cnt <- sf::st_read("path/to/county/shapefile.shp")

# Loop through each geoTIFF file
for (tiff_file in geotiff_files) {

  # Get filename for output naming
  tiff_name <- tools::file_path_sans_ext(basename(tiff_file))

  cat("Processing:", tiff_name, "\n")

  parts <- str_split(tiff_name, "_")[[1]]
  name <- parts[1]
  land_policy <- if(length(parts) > 1) parts[2] else NA   # Second part
  accretion <- if(length(parts) > 2) parts[3] else NA  # Third part
  slr_scenario <- if(length(parts) > 3) parts[4] else NA  # Fourth part
  yr <- if(length(parts) > 4) parts[5] else NA  # Fifth part

  # Load the raster
  tbcmp_raster <- rast(tiff_file)

  # Initialize list for county results
  county_list <- list()

  # Loop through each county polygon
  for (i in 1:nrow(tbcmp_cnt)) {

    # Print progress
    if (i %% 10 == 0) {
      cat("  Processing county", i, "of", nrow(tbcmp_cnt), "\n")
    }

    # Extract single polygon
    single_poly <- tbcmp_cnt[i, ]

    # Crop and mask raster
    r_sub <- crop(tbcmp_raster, single_poly) |>
      mask(single_poly)

    # Extract values
    extracted <- exact_extract(r_sub, single_poly, fun = NULL, force_df = TRUE,
                               max_cells_in_memory = 3e+08)

    # Summarize data
    county_list[[i]] <- data.frame(id = single_poly$county, extracted) |>
      unnest() |>
      group_by(id, value) |>
      summarise(count = sum(coverage_fraction, na.rm = TRUE),
                .groups = "drop") |>
      mutate(
        filename = tiff_name,
        land_policy = land_policy,
        accretion = accretion,
        slr_scenario = slr_scenario,
        yr = yr
      )

    # Clean up memory
    rm(extracted)
    rm(r_sub)
    gc()
  }

  # Combine all county results
  county_summary <- do.call(rbind, county_list) |>
    mutate(acres = count * 0.000988422)

  # Save results
  output_file <- file.path(output_dir, paste0(tiff_name, "_county_summary.csv"))
  write.csv(county_summary, output_file, row.names = FALSE)

  cat("Saved results to:", output_file, "\n\n")

  # Clean up raster from memory
  rm(tbcmp_raster)
  gc()
}

cat("All files processed successfully!\n")
