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
# Difference from 03b: basins are read from a local shapefile
# (./data-raw/swfwmd/Drainage_Basin_Boundaries.shp) rather than pulled from
# NHDPlus via nhdplusTools::get_huc(). No network call is required.
#
# REVISION 1 - dissolve now includes HUC8
# ---------------------------------------
# The original version dissolved on EXTHUC + BASIN + FEATURE only. EXTHUC is
# unique only WITHIN a HUC8, so that grouping merged basins across unrelated
# watersheds: EXTHUC 99990000 / "DIRECT RUNOFF TO BAY" occurs 51 times across
# five HUC8s, and 99999900 / "DIRECT RUNOFF TO GULF" 23 times, all collapsing
# into single multipart units.
#
# REVISION 2 - FEATURE removed from the dissolve key
# --------------------------------------------------
# Revision 1 left FEATURE in the grouping while building basin_id from
# HUC8 + EXTHUC + BASIN, so a group splitting on FEATURE alone produced two
# rows with the same id. This raised:
#
#   Error: Duplicate basin_id values after dissolve:
#          03100206_99990000_DIR_RUNOFF_TO_BAY
#
# FEATURE is now aggregated rather than grouped on, and basin_id is built from
# exactly the columns in id_keys. Names are also normalized ("DIR RUNOFF" ->
# "DIRECT RUNOFF", whitespace squished), which merges one further pair.
# Resulting counts from 1,166 source records:
#
#   EXTHUC + BASIN + FEATURE          1,061 units  (original, incorrect)
#   HUC8 + EXTHUC + BASIN + FEATURE   1,070 units  (revision 1, id collision)
#   HUC8 + EXTHUC + BASIN             1,069 units  (revision 2)
#   ... with name normalization       1,068 units  (default)
#
# basin_id values have changed (now HUC8_EXTHUC_BASIN_NAME). Any CSVs written
# by an earlier version must be regenerated - they are not joinable with
# output from this version.
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

# Acres per raster cell. 0.000988422 = 4 m^2 (2x2m HEM outputs).
# Use 0.00617764 for 5x5m outputs, 0.02471054 for 10x10m.
# Must match the value used in 04b_HEM_Change_Summary_by_Basin.R.
cell_acres <- 0.000988422

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

# Source layer is NAD83(HARN) / StatePlane Florida West ftUS; st_transform
# handles the conversion to the project CRS.
tbcmp_basins <- st_read(basin_shp, quiet = TRUE) |>
  st_transform(prj) |>
  st_make_valid()

cat("  Read", nrow(tbcmp_basins), "polygon records\n")

# --- Resolve attribute names defensively -----------------------------------
# Shapefile export can truncate/case-shift field names, so match rather than
# assume. Expected SWFWMD/FDEP schema:
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
  )

# --- Normalize basin names --------------------------------------------------
# The source layer carries abbreviation variants that split what is one unit:
# "DIR RUNOFF TO BAY" (5 records) alongside "DIRECT RUNOFF TO BAY" (52), same
# HUC8 and same EXTHUC. One name also has a doubled internal space
# ("LEMMON STREET  DITCH"). Normalizing merges one pair of units in HUC8
# 03100206 (1,069 -> 1,068). Set to FALSE to keep the source names verbatim.
normalize_basin_names <- TRUE

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

# --- Dissolve multipart records so each basin appears once ------------------
# Mirrors the county dissolve in 01_data_prep.R. Prevents duplicate ids in the
# output when a single basin is stored as several polygon records. HUC8 is
# included because EXTHUC is only unique within a HUC8 - see the revision note
# in the header.
#
# FEATURE is deliberately NOT a grouping key. It describes the hydrologic
# feature type, not basin identity, and grouping on it while building basin_id
# from HUC8 + EXTHUC + BASIN produces two rows sharing one id. That happens
# once in the layer: HUC8 03100206 / EXTHUC 99990000 / "DIR RUNOFF TO BAY" has
# four records tagged RUNOFF and one tagged BAY. The BAY tag is a mis-entry -
# the actual bay is the separate "TAMPA BAY" record with the same EXTHUC - so
# splitting on it would manufacture a spurious unit. Where a unit does span
# multiple FEATURE values they are concatenated, keeping the information
# visible in the output without fragmenting the unit.
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

# Sanity check: basin_id must be unique after the dissolve. If this fires, the
# columns used to build basin_id have drifted from id_keys - reconcile the two
# rather than making the id longer.
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

# Keep only basins that intersect the project area
tbcmp_basins <- st_filter(tbcmp_basins, study_bbox)

cat("Found", nrow(tbcmp_basins), "drainage basins in study area\n\n")

if (nrow(tbcmp_basins) == 0) {
  stop("No drainage basins intersect the project area. Check the shapefile CRS.")
}

# Cache the dissolved layer so 04b and any mapping scripts use identical ids
save(tbcmp_basins, file = here("data/tbcmp_basins.Rdata"), compress = "xz")

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
        basin_huc8   = single_poly$basin_huc8,
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
    mutate(acres = count * cell_acres)

  # Save results
  output_file <- file.path(output_dir, paste0(tiff_name, "_basin_summary.csv"))
  write.csv(basin_summary, output_file, row.names = FALSE)

  cat("Saved results to:", output_file, "\n\n")

  # Clean up raster from memory
  rm(tbcmp_raster)
  gc()
}

cat("All files processed successfully!\n")
