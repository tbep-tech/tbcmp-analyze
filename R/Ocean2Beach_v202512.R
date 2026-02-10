# ==============================================================================
# OCEAN TO BEACH v202512
# Converted from ArcGIS Python to R
# ==============================================================================
# Purpose: Identify ocean proximity for beach habitat conversion
# ==============================================================================

library(terra)
library(sf)

# ==============================================================================
# MAIN FUNCTION
# ==============================================================================

Ocean2Beach_v202512 <- function(
    habitat = NULL,
    output_path = NULL
) {

  # Load habitat raster if path provided
  if (is.character(habitat)) {
    habitat <- rast(habitat)
  }

  cat("Creating ocean proximity mask for beach identification...\n")

  # -------------------------------------------------------------------------
  # Step 1: Create blank habitat mask
  # -------------------------------------------------------------------------

  blankHab <- habitat == 0

  # -------------------------------------------------------------------------
  # Step 2: Convert habitat to polygon
  # -------------------------------------------------------------------------

  cat("  Converting habitat raster to polygons...\n")
  hab_polygon <- as.polygons(habitat)

  # -------------------------------------------------------------------------
  # Step 3: Select ocean and water bodies
  # -------------------------------------------------------------------------
  # Habitat codes: 5400 = Subtidal, 9113 = Submerged Aquatic Vegetation

  cat("  Selecting ocean extents...\n")
  ocean_extent <- hab_polygon[hab_polygon$Value %in% c(5400, 9113), ]

  # Alternative if column name is different
  if (nrow(ocean_extent) == 0) {
    # Try with generic gridcode column
    col_name <- names(hab_polygon)[1]
    ocean_extent <- hab_polygon[hab_polygon[[col_name]] %in% c(5400, 9113), ]
  }

  # -------------------------------------------------------------------------
  # Step 4: Buffer ocean extent by 500m
  # -------------------------------------------------------------------------

  cat("  Buffering ocean extent by 500m...\n")

  # Convert to sf for buffering
  ocean_sf <- st_as_sf(ocean_extent)
  ocean_500m_sf <- st_buffer(ocean_sf, dist = 500)

  # -------------------------------------------------------------------------
  # Step 5: Convert buffered polygon back to raster
  # -------------------------------------------------------------------------

  cat("  Converting buffer to raster...\n")
  ocean_500m_vect <- vect(ocean_500m_sf)

  # Rasterize using habitat as template
  ocean_500m_ras <- rasterize(
    ocean_500m_vect,
    habitat,
    field = 1,  # Use constant value of 1
    background = 0
  )

  # -------------------------------------------------------------------------
  # Step 6: Mosaic blank habitat with ocean buffer
  # -------------------------------------------------------------------------

  cat("  Creating final ocean check raster...\n")

  # Combine blankHab and ocean_500m_ras
  # Where ocean_500m_ras has values, use those; otherwise use blankHab
  oceancheck_temp <- ifel(is.na(ocean_500m_ras), blankHab, ocean_500m_ras)

  # -------------------------------------------------------------------------
  # Step 7: Extract by mask (clip to habitat extent)
  # -------------------------------------------------------------------------

  cat("  Masking to habitat extent...\n")

  # Create mask from habitat (where habitat is not NA)
  habitat_mask <- !is.na(habitat)

  # Apply mask
  oceancheck <- ifel(habitat_mask, oceancheck_temp, NA)

  # -------------------------------------------------------------------------
  # Save output if path provided
  # -------------------------------------------------------------------------

  if (!is.null(output_path)) {
    cat(sprintf("  Saving output to: %s\n", output_path))
    writeRaster(oceancheck, output_path, overwrite = TRUE)
  }

  cat("Ocean proximity check complete!\n")

  return(oceancheck)
}
