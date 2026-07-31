# ---------------------------------------------------------------------------
# 03c_process_tiffs_basins.R
#
# Summarize HEM GeoTIFF outputs by SWFWMD/FDEP drainage basin.
#
# Companion to:
#   03_process_tiffs.R        (summaries by county)
#   03b_process_tiffs_huc12.R (summaries by NHDPlus HUC12)
#
# Difference from 03b: basins are read from a local shapefile
# (./data-raw/swfwmd/Drainage_Basin_Boundaries.shp) rather than pulled from
# NHDPlus via nhdplusTools::get_huc(). No network call is required.
#
# Tampa Bay Estuary Program / Tampa Bay Coastal Master Plan
# ---------------------------------------------------------------------------

# Load required libraries
library(terra)
library(exactextractr)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(here)

# Define paths
geotiff_dir <- here("data/output/raster/")
output_dir  <- here("data/output/basin/")
basin_shp   <- here("data-raw/swfwmd/Drainage_Basin_Boundaries.shp")

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Get list of all geoTIFF files
geotiff_files <- list.files(geotiff_dir,
                            pattern = "\\.tif$|\\.tiff$",
                            full.names = TRUE)

# ---------------------------------------------------------------------------
# Load drainage basins for the study area
# ---------------------------------------------------------------------------
if (!file.exists(basin_shp)) {
  stop("Drainage basin shapefile not found at: ", basin_shp,
       "\nDownload from https://data-swfwmd.opendata.arcgis.com/datasets/drainage-basin-boundaries")
}

# Base raster defines the project area extent and CRS (EPSG:3087)
base_raster <- rast(here("data/tbcmp_base_raster_10m.tif"))
prj         <- crs(base_raster)

study_bbox <- st_bbox(base_raster) |>
  st_as_sfc() |>
  st_as_sf() |>
  st_set_crs(prj)

cat("Reading drainage basins from:", basin_shp, "\n")

tbcmp_basins <- st_read(basin_shp, quiet = TRUE) |>
  st_transform(prj) |>
  st_make_valid()

# --- Resolve attribute names defensively -----------------------------------
# Shapefile export can truncate/case-shift field names, so match rather than
# assume. Expected SWFWMD/FDEP schema: BASIN, HUC, EXTHUC, FEATURE, SQ_MILES.
nms       <- names(tbcmp_basins)
name_fld  <- nms[toupper(nms) %in% c("BASIN", "BASIN_NAME", "NAME")][1]
huc_fld   <- nms[toupper(nms) %in% c("EXTHUC", "HUC", "HUC8")][1]
feat_fld  <- nms[toupper(nms) %in% c("FEATURE", "FEAT_TYPE")][1]

if (is.na(name_fld)) {
  stop("Could not find a basin name field. Fields present: ",
       paste(nms, collapse = ", "))
}

tbcmp_basins <- tbcmp_basins |>
  mutate(
    basin_name = as.character(.data[[name_fld]]),
    basin_huc  = if (!is.na(huc_fld))  as.character(.data[[huc_fld]])  else NA_character_,
    basin_feat = if (!is.na(feat_fld)) as.character(.data[[feat_fld]]) else NA_character_
  )

# --- Dissolve multipart records so each basin appears once ------------------
# Mirrors the county dissolve in 01_data_prep.R. Prevents duplicate ids in the
# output when a single basin is stored as several polygon records.
tbcmp_basins <- tbcmp_basins |>
  group_by(basin_name, basin_huc, basin_feat) |>
  summarise(.groups = "drop") |>
  st_make_valid() |>
  mutate(basin_id = paste0(coalesce(basin_huc, "NA"), "_",
                           str_replace_all(basin_name, "\\s+", "_")))

# Keep only basins that intersect the project area
tbcmp_basins <- st_filter(tbcmp_basins, study_bbox)

cat("Found", nrow(tbcmp_basins), "drainage basins in study area\n\n")

if (nrow(tbcmp_basins) == 0) {
  stop("No drainage basins intersect the project area. Check the shapefile CRS.")
}

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

  # Filter basin polygons to only those intersecting this raster's extent
  raster_ext_sf <- st_as_sf(as.polygons(ext(tbcmp_raster), crs = crs(tbcmp_raster)))
  basin_sub <- st_filter(st_transform(tbcmp_basins, crs(tbcmp_raster)), raster_ext_sf)

  if (nrow(basin_sub) == 0) {
    cat("  No overlapping drainage basins found, skipping.\n\n")
    rm(tbcmp_raster); gc()
    next
  }

  # Initialize list for basin results
  basin_list <- list()

  # Loop through each basin polygon
  for (i in seq_len(nrow(basin_sub))) {

    # Print progress
    if (i %% 10 == 0) {
      cat("  Processing basin", i, "of", nrow(basin_sub), "\n")
    }

    # Extract single polygon
    single_poly <- basin_sub[i, ]

    # Crop and mask raster
    r_sub <- crop(tbcmp_raster, single_poly) |>
      mask(single_poly)

    # Extract values
    extracted <- exact_extract(r_sub, single_poly, fun = NULL, force_df = TRUE,
                               max_cells_in_memory = 3e+08)

    # Summarize data
    basin_list[[i]] <- data.frame(id = single_poly$basin_id, extracted) |>
      unnest(cols = everything()) |>
      group_by(id, value) |>
      summarise(count = sum(coverage_fraction, na.rm = TRUE),
                .groups = "drop") |>
      mutate(
        basin_name   = single_poly$basin_name,
        basin_huc    = single_poly$basin_huc,
        basin_feat   = single_poly$basin_feat,
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

  # Combine all basin results
  basin_summary <- do.call(rbind, basin_list) |>
    mutate(acres = count * 0.000988422)   # 2x2m cells; adjust for other resolutions

  # Save results
  output_file <- file.path(output_dir, paste0(tiff_name, "_basin_summary.csv"))
  write.csv(basin_summary, output_file, row.names = FALSE)

  cat("Saved results to:", output_file, "\n\n")

  # Clean up raster from memory
  rm(tbcmp_raster)
  gc()
}

cat("All files processed successfully!\n")
