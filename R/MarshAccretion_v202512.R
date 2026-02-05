# ==============================================================================
# MARSH ACCRETION v202512
# Converted from ArcGIS Python to R
# ==============================================================================
# Purpose: Calculate marsh vertical accretion based on habitat type
# ==============================================================================

library(terra)

# ==============================================================================
# MAIN FUNCTION
# ==============================================================================

MarshAccretion_v202512 <- function(
    Juncus_Marsh_Accretion_mm_yr = 3.75,
    Salt_Marsh_Accretion_mm_yr = 1.6,
    habitat = NULL,
    topo = NULL,
    Constant_Accretion = TRUE,
    Max_Accretion_mm_yr = 4,
    MHHW = 0.238,
    MLLW = -0.45,
    Topo_Year = 2025,
    OutY = 2050,
    Mangrove_Accretion_mm_yr = 1.6
) {
  
  # Load rasters if paths provided
  if (is.character(habitat)) {
    habitat <- rast(habitat)
  }
  if (is.character(topo)) {
    topo <- rast(topo)
  }
  
  # Calculate years elapsed
  years_elapsed <- OutY - Topo_Year
  
  cat(sprintf("Calculating marsh accretion over %d years...\n", years_elapsed))
  
  # ===========================================================================
  # CONSTANT ACCRETION METHOD
  # ===========================================================================
  
  if (Constant_Accretion) {
    
    cat("Using constant accretion rates by habitat type...\n")
    
    # Habitat codes:
    # 6420 = Salt Marsh
    # 6425 = Juncus Marsh (Low Marsh)
    # 6120 = Mangrove
    
    # Calculate accretion for each habitat type
    # Formula: topo + (accretion_rate_mm_yr * years / 1000) to convert mm to m
    
    salt_marsh_accretion <- (Salt_Marsh_Accretion_mm_yr * years_elapsed) / 1000
    juncus_accretion <- (Juncus_Marsh_Accretion_mm_yr * years_elapsed) / 1000
    mangrove_accretion <- (Mangrove_Accretion_mm_yr * years_elapsed) / 1000
    
    cat(sprintf("  Salt Marsh accretion: %.3f m\n", salt_marsh_accretion))
    cat(sprintf("  Juncus Marsh accretion: %.3f m\n", juncus_accretion))
    cat(sprintf("  Mangrove accretion: %.3f m\n", mangrove_accretion))
    
    # Apply conditional logic using terra::ifel (if-else for rasters)
    # Nested conditions: Check habitat type and add appropriate accretion
    
    marshac1 <- ifel(
      habitat == 6420,  # Salt Marsh
      topo + salt_marsh_accretion,
      ifel(
        habitat == 6425,  # Juncus Marsh
        topo + juncus_accretion,
        ifel(
          habitat == 6120,  # Mangrove
          topo + mangrove_accretion,
          topo  # No change for other habitats
        )
      )
    )
    
    return(marshac1)
  }
  
  # ===========================================================================
  # MARSH98 ACCRETION METHOD (elevation-dependent)
  # ===========================================================================
  
  if (!Constant_Accretion) {
    
    cat("Using Marsh98 elevation-dependent accretion model...\n")
    
    # Calculate maximum accretion in meters
    max_accretion_m <- (Max_Accretion_mm_yr * years_elapsed) / 1000
    
    cat(sprintf("  Maximum accretion: %.3f m\n", max_accretion_m))
    
    # Marsh habitats: 6425 (Juncus), 6600 (Salt Barren), 6420 (Salt Marsh)
    is_marsh <- (habitat == 6425) | (habitat == 6600) | (habitat == 6420)
    
    # Elevation-dependent accretion
    # Between MLLW and MHHW: linear relationship
    # Below MLLW: maximum accretion
    
    in_tidal_range <- (topo >= MLLW) & (topo <= MHHW)
    below_mllw <- topo < MLLW
    
    # Calculate accretion factor based on elevation
    # Formula: max_accretion / (MLLW - MHHW) * (topo - MHHW)
    # This creates a linear gradient from max accretion at MLLW to 0 at MHHW
    
    accretion_factor <- max_accretion_m / (MLLW - MHHW) * (topo - MHHW)
    
    marshac2 <- ifel(
      is_marsh & in_tidal_range,
      topo + accretion_factor,
      ifel(
        is_marsh & below_mllw,
        topo + max_accretion_m,
        topo  # No change
      )
    )
    
    return(marshac2)
  }
  
  # Should not reach here
  stop("Either Constant_Accretion must be TRUE or FALSE")
}
