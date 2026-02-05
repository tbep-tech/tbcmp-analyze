# ==============================================================================
# PROCESS FRESHWATER v202512
# Converted from ArcGIS Python to R
# ==============================================================================
# Purpose: Create freshwater influence mask from NHD polygon data
# ==============================================================================

library(terra)
library(sf)

# ==============================================================================
# MAIN FUNCTION
# ==============================================================================

ProcessFreshwater_v202512 <- function(
    habitat = NULL,
    FW_Polygon = NULL,
    output_path = NULL
) {
  
  # Load habitat raster if path provided
  if (is.character(habitat)) {
    habitat <- rast(habitat)
  }
  
  cat("Processing freshwater influence areas...\n")
  
  # -------------------------------------------------------------------------
  # Step 1: Create blank habitat mask
  # -------------------------------------------------------------------------
  
  blankHab2 <- habitat == 0
  
  # -------------------------------------------------------------------------
  # Step 2: Load freshwater polygon data
  # -------------------------------------------------------------------------
  
  cat("  Loading freshwater polygon data...\n")
  
  # Load using sf (supports geodatabase feature classes)
  if (is.character(FW_Polygon)) {
    # Check if it's a geodatabase path
    if (grepl("\\.gdb/", FW_Polygon)) {
      fw_sf <- st_read(FW_Polygon, quiet = TRUE)
    } else {
      fw_sf <- st_read(FW_Polygon, quiet = TRUE)
    }
  } else {
    fw_sf <- FW_Polygon
  }
  
  # -------------------------------------------------------------------------
  # Step 3: Convert polygon to raster
  # -------------------------------------------------------------------------
  
  cat("  Rasterizing freshwater polygons...\n")
  
  # Convert sf to terra vector
  fw_vect <- vect(fw_sf)
  
  # Rasterize using habitat as template
  # Use ORIG_FID or OBJECTID or create sequential IDs
  if ("ORIG_FID" %in% names(fw_vect)) {
    field_name <- "ORIG_FID"
  } else if ("OBJECTID" %in% names(fw_vect)) {
    field_name <- "OBJECTID"
  } else if ("FID" %in% names(fw_vect)) {
    field_name <- "FID"
  } else {
    # Create sequential IDs
    fw_vect$ID <- 1:nrow(fw_vect)
    field_name <- "ID"
  }
  
  FreshRas <- rasterize(
    fw_vect,
    habitat,
    field = field_name,
    background = 0
  )
  
  # -------------------------------------------------------------------------
  # Step 4: Mosaic blank habitat with freshwater raster
  # -------------------------------------------------------------------------
  
  cat("  Creating mosaic...\n")
  
  # Combine blankHab2 and FreshRas
  Freshwater_temp <- ifel(is.na(FreshRas) | FreshRas == 0, blankHab2, FreshRas)
  
  # -------------------------------------------------------------------------
  # Step 5: Extract by mask (clip to habitat extent)
  # -------------------------------------------------------------------------
  
  cat("  Masking to habitat extent...\n")
  
  # Create mask from habitat (where habitat is not NA)
  habitat_mask <- !is.na(habitat)
  
  # Apply mask
  freshwater2 <- ifel(habitat_mask, Freshwater_temp, NA)
  
  # Convert to binary (1 = freshwater influence, 0 = no influence)
  freshwater2 <- ifel(freshwater2 > 0, 1, 0)
  
  # -------------------------------------------------------------------------
  # Save output if path provided
  # -------------------------------------------------------------------------
  
  if (!is.null(output_path)) {
    cat(sprintf("  Saving output to: %s\n", output_path))
    writeRaster(freshwater2, output_path, overwrite = TRUE)
  }
  
  cat("Freshwater processing complete!\n")
  
  return(freshwater2)
}
