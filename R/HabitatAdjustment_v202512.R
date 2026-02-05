# ==============================================================================
# HABITAT ADJUSTMENT v202512
# Converted from ArcGIS Python to R
# ==============================================================================
# Purpose: Reclassify habitat types based on elevation and SLR-adjusted datums
# ==============================================================================

library(terra)

# Source helper functions
source("Ocean2Beach_v202512.R")
source("ProcessFreshwater_v202512.R")

# ==============================================================================
# MAIN FUNCTION
# ==============================================================================

HabitatAdjustment_v202512 <- function(
    topo = NULL,
    habitat = NULL,
    Habitats_Adjusted = NULL,
    HAT_SLR = 0.84,
    MHHW_SLR = 0.578,
    MTL_SLR = 0.25,
    MLW_SLR = 0.007,
    OutY = 2050,
    FW_Polygon = NULL,
    Protect_Developed = FALSE,
    MLLW_SLR = -0.11,
    MHW_SLR = 0.492,
    MLHW_SLR = 0.124,
    Ocean_500m_ras = NULL
) {
  
  # Load rasters if paths provided
  if (is.character(topo)) {
    topo <- rast(topo)
  }
  if (is.character(habitat)) {
    habitat <- rast(habitat)
  }
  
  cat("Starting habitat adjustment process...\n")
  cat(sprintf("  Year: %d\n", OutY))
  cat(sprintf("  Protect Developed: %s\n", Protect_Developed))
  
  # =========================================================================
  # SECTION 1: DEVELOPED LAND PROTECTION
  # =========================================================================
  
  cat("\nSection 1: Processing developed land categories...\n")
  
  # -------------------------------------------------------------------------
  # Upland Developed Hard Surface (1200)
  # These are always protected
  # -------------------------------------------------------------------------
  
  Upland_Developed_hard <- ifel(
    (habitat == 1200) & (topo != -999),
    1200,
    0
  )
  
  # -------------------------------------------------------------------------
  # Upland Developed Pervious (1100)
  # These are always protected
  # -------------------------------------------------------------------------
  
  Upland_Developed_hard_2 <- ifel(
    (habitat == 1100) & (topo != -999),
    1100,
    0
  )
  
  # -------------------------------------------------------------------------
  # Upland Developed Low Density (1800)
  # Protection depends on Protect_Developed setting
  # -------------------------------------------------------------------------
  
  if (Protect_Developed) {
    # Protect all low density development
    Upland_Dev_2 <- ifel(
      (habitat == 1800) & (topo != -999),
      1800,
      0
    )
  } else {
    Upland_Dev_2 <- rast(topo)  # Create template
    values(Upland_Dev_2) <- 0
  }
  
  # Alternative: Allow inundation above HAT
  if (!Protect_Developed) {
    Upland_Dev_1_2 <- ifel(
      (habitat == 1800) & (topo >= HAT_SLR),
      1800,
      0
    )
  } else {
    Upland_Dev_1_2 <- rast(topo)  # Create template
    values(Upland_Dev_1_2) <- 0
  }
  
  # Merge the two approaches
  Upland_Developed_soft <- ifel(
    Upland_Dev_2 > 0,
    Upland_Dev_2,
    Upland_Dev_1_2
  )
  
  # -------------------------------------------------------------------------
  # Golf Courses (1820)
  # -------------------------------------------------------------------------
  
  if (Protect_Developed) {
    Golf1 <- ifel(
      (habitat == 1820) & (topo != -999),
      1820,
      0
    )
  } else {
    Golf1 <- rast(topo)
    values(Golf1) <- 0
  }
  
  if (!Protect_Developed) {
    Golf2 <- ifel(
      (habitat == 1820) & (topo >= HAT_SLR),
      1820,
      0
    )
  } else {
    Golf2 <- rast(topo)
    values(Golf2) <- 0
  }
  
  Golf_soft <- ifel(Golf1 > 0, Golf1, Golf2)
  
  # -------------------------------------------------------------------------
  # Agriculture (2100)
  # -------------------------------------------------------------------------
  
  if (Protect_Developed) {
    Ag1 <- ifel(
      (habitat == 2100) & (topo != -999),
      2100,
      0
    )
  } else {
    Ag1 <- rast(topo)
    values(Ag1) <- 0
  }
  
  if (!Protect_Developed) {
    Ag2 <- ifel(
      (habitat == 2100) & (topo >= HAT_SLR),
      2100,
      0
    )
  } else {
    Ag2 <- rast(topo)
    values(Ag2) <- 0
  }
  
  Agriculture <- ifel(Ag1 > 0, Ag1, Ag2)
  
  # -------------------------------------------------------------------------
  # Tree Crops (2200)
  # -------------------------------------------------------------------------
  
  if (Protect_Developed) {
    Tree1 <- ifel(
      (habitat == 2200) & (topo != -999),
      2200,
      0
    )
  } else {
    Tree1 <- rast(topo)
    values(Tree1) <- 0
  }
  
  if (!Protect_Developed) {
    Tree2 <- ifel(
      (habitat == 2200) & (topo >= HAT_SLR),
      2200,
      0
    )
  } else {
    Tree2 <- rast(topo)
    values(Tree2) <- 0
  }
  
  Tree <- ifel(Tree1 > 0, Tree1, Tree2)
  
  # -------------------------------------------------------------------------
  # Vineyard (2400)
  # -------------------------------------------------------------------------
  
  if (Protect_Developed) {
    Vine1 <- ifel(
      (habitat == 2400) & (topo != -999),
      2400,
      0
    )
  } else {
    Vine1 <- rast(topo)
    values(Vine1) <- 0
  }
  
  if (!Protect_Developed) {
    Vine2 <- ifel(
      (habitat == 2400) & (topo >= HAT_SLR),
      2400,
      0
    )
  } else {
    Vine2 <- rast(topo)
    values(Vine2) <- 0
  }
  
  Vineyard <- ifel(Vine1 > 0, Vine1, Vine2)
  
  # -------------------------------------------------------------------------
  # Aquaculture (2550)
  # -------------------------------------------------------------------------
  
  if (Protect_Developed) {
    aqui1 <- ifel(
      (habitat == 2550) & (topo != -999),
      2550,
      0
    )
  } else {
    aqui1 <- rast(topo)
    values(aqui1) <- 0
  }
  
  if (!Protect_Developed) {
    aqui2 <- ifel(
      (habitat == 2550) & (topo >= HAT_SLR),
      2550,
      0
    )
  } else {
    aqui2 <- rast(topo)
    values(aqui2) <- 0
  }
  
  Aquiculture <- ifel(aqui1 > 0, aqui1, aqui2)
  
  # Continue in next section...
  
  # Store intermediate results for part 2
  intermediate <- list(
    Upland_Developed_hard = Upland_Developed_hard,
    Upland_Developed_hard_2 = Upland_Developed_hard_2,
    Upland_Developed_soft = Upland_Developed_soft,
    Golf_soft = Golf_soft,
    Agriculture = Agriculture,
    Tree = Tree,
    Vineyard = Vineyard,
    Aquiculture = Aquiculture
  )
  
  # Call part 2
  HabitatAdjustment_part2(
    topo = topo,
    habitat = habitat,
    Habitats_Adjusted = Habitats_Adjusted,
    HAT_SLR = HAT_SLR,
    MHHW_SLR = MHHW_SLR,
    MTL_SLR = MTL_SLR,
    MLHW_SLR = MLHW_SLR,
    MHW_SLR = MHW_SLR,
    MLW_SLR = MLW_SLR,
    MLLW_SLR = MLLW_SLR,
    OutY = OutY,
    FW_Polygon = FW_Polygon,
    Protect_Developed = Protect_Developed,
    Ocean_500m_ras = Ocean_500m_ras,
    intermediate = intermediate
  )
}
