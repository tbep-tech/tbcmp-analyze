# ==============================================================================
# HABITAT ADJUSTMENT v202512 - Part 2
# Coastal and Wetland Habitat Classifications
# ==============================================================================

HabitatAdjustment_part2 <- function(
    topo,
    habitat,
    Habitats_Adjusted,
    HAT_SLR,
    MHHW_SLR,
    MTL_SLR,
    MLHW_SLR,
    MHW_SLR,
    MLW_SLR,
    MLLW_SLR,
    OutY,
    FW_Polygon,
    Protect_Developed,
    Ocean_500m_ras,
    intermediate
) {
  
  # Extract intermediate results from part 1
  Upland_Developed_hard <- intermediate$Upland_Developed_hard
  Upland_Developed_hard_2 <- intermediate$Upland_Developed_hard_2
  Upland_Developed_soft <- intermediate$Upland_Developed_soft
  Golf_soft <- intermediate$Golf_soft
  Agriculture <- intermediate$Agriculture
  Tree <- intermediate$Tree
  Vineyard <- intermediate$Vineyard
  Aquiculture <- intermediate$Aquiculture
  
  # =========================================================================
  # SECTION 2: OCEAN PROXIMITY AND BEACH
  # =========================================================================
  
  cat("\nSection 2: Processing ocean proximity and beach...\n")
  
  # Call Ocean2Beach function
  oceancheck <- Ocean2Beach_v202512(habitat = habitat)
  
  # -------------------------------------------------------------------------
  # Beach (7100)
  # Conditions: 
  # - Existing beach above MLLW, OR
  # - Developed land near ocean, below HAT, above MLLW
  # -------------------------------------------------------------------------
  
  developed_codes <- c(1800, 1820, 2100, 2200, 2400, 2550)
  is_developed <- habitat %in% developed_codes
  
  beach <- ifel(
    ((habitat == 7100) & (topo > MLLW_SLR)) |
    (is_developed & (oceancheck == 1) & (topo < HAT_SLR) & (topo >= MLLW_SLR)),
    7100,
    0
  )
  
  # =========================================================================
  # SECTION 3: FRESHWATER INFLUENCE
  # =========================================================================
  
  cat("\nSection 3: Processing freshwater influence...\n")
  
  # Call ProcessFreshwater function
  Freshwater <- ProcessFreshwater_v202512(
    habitat = habitat,
    FW_Polygon = FW_Polygon
  )
  
  # =========================================================================
  # SECTION 4: MARINE AND COASTAL HABITATS
  # =========================================================================
  
  cat("\nSection 4: Classifying marine and coastal habitats...\n")
  
  # -------------------------------------------------------------------------
  # Subtidal (5400)
  # Deep water areas
  # -------------------------------------------------------------------------
  
  terrestrial_codes <- c(1800, 1820, 2100, 2200, 2400, 2550, 1900, 3200, 
                         4100, 4400, 6110, 6410, 6600, 6420, 6425, 6120, 6510, 9113)
  is_terrestrial <- habitat %in% terrestrial_codes
  
  subtidal <- ifel(
    (is_terrestrial & (topo < (MLLW_SLR - 1.5))) |
    ((habitat == 7100) & (topo < MLLW_SLR)) |
    ((habitat == 5200) & (topo <= HAT_SLR)) |
    (habitat == 5400),
    5400,
    0
  )
  
  # -------------------------------------------------------------------------
  # Open Water (5200)
  # Estuarine open water above HAT
  # -------------------------------------------------------------------------
  
  openwater <- ifel(
    (habitat == 5200) & (topo >= HAT_SLR),
    5200,
    0
  )
  
  # -------------------------------------------------------------------------
  # Tidal Flat (6510)
  # Between MLHW and MTL in freshwater-influenced areas
  # -------------------------------------------------------------------------
  
  tidalflat <- ifel(
    ((habitat == 6510) & (topo >= MLHW_SLR) & (Freshwater == 1)) |
    (is_terrestrial & (topo < MTL_SLR) & (topo >= MLHW_SLR) & (Freshwater == 1)),
    6510,
    0
  )
  
  # -------------------------------------------------------------------------
  # Mangroves (6120)
  # Between MLW and MHW in saltwater areas
  # -------------------------------------------------------------------------
  
  mangroves <- ifel(
    ((habitat == 6120) & (topo >= MLW_SLR) & (Freshwater == 0)) |
    ((habitat == 6425) & (topo >= MTL_SLR) & (Freshwater == 0)) |
    (is_terrestrial & (topo < MHW_SLR) & (topo >= MLW_SLR) & (Freshwater == 0)),
    6120,
    0
  )
  
  # -------------------------------------------------------------------------
  # Salt Barren (6600)
  # Between MHHW and HAT
  # -------------------------------------------------------------------------
  
  upland_codes <- c(1800, 1820, 2100, 2200, 2400, 2550, 1900, 3200, 
                    4100, 4400, 6110, 6410)
  is_upland <- habitat %in% upland_codes
  
  SaltBarren <- ifel(
    ((habitat == 6600) & (topo >= MHHW_SLR)) |
    (is_upland & (topo < HAT_SLR) & (topo >= MHHW_SLR)),
    6600,
    0
  )
  
  # -------------------------------------------------------------------------
  # High Marsh / Salt Marsh (6420)
  # Between MHW and MHHW
  # -------------------------------------------------------------------------
  
  highMarsh <- ifel(
    ((habitat == 6420) & (topo >= MHW_SLR)) |
    (is_upland & (topo < MHHW_SLR) & (topo >= MHW_SLR)),
    6420,
    0
  )
  
  # -------------------------------------------------------------------------
  # Juncus Low Marsh (6425)
  # Between MTL and MHW in freshwater areas
  # -------------------------------------------------------------------------
  
  marsh_codes <- c(1800, 1820, 2100, 2200, 2400, 2550, 1900, 3200,
                   4100, 4400, 6110, 6410, 6600, 6420, 6120)
  is_marsh_convertible <- habitat %in% marsh_codes
  
  JuncusMarsh <- ifel(
    (((habitat == 6425) | (habitat == 6120)) & (topo >= MTL_SLR) & (Freshwater == 1)) |
    (is_marsh_convertible & (topo < MHW_SLR) & (topo >= MTL_SLR) & (Freshwater == 1)),
    6425,
    0
  )
  
  # =========================================================================
  # SECTION 5: UPLAND HABITATS
  # =========================================================================
  
  cat("\nSection 5: Classifying upland habitats...\n")
  
  # -------------------------------------------------------------------------
  # Upland Undeveloped (1900)
  # -------------------------------------------------------------------------
  
  Upland_Undeveloped <- ifel(
    (habitat == 1900) & (topo >= HAT_SLR),
    1900,
    0
  )
  
  # -------------------------------------------------------------------------
  # Shrub (3200)
  # -------------------------------------------------------------------------
  
  Shrub <- ifel(
    (habitat == 3200) & (topo >= HAT_SLR),
    3200,
    0
  )
  
  # -------------------------------------------------------------------------
  # Forest (4100)
  # -------------------------------------------------------------------------
  
  Forest <- ifel(
    (habitat == 4100) & (topo >= HAT_SLR),
    4100,
    0
  )
  
  # -------------------------------------------------------------------------
  # Tree Plantation (4400)
  # -------------------------------------------------------------------------
  
  TreePlant <- ifel(
    (habitat == 4400) & (topo >= HAT_SLR),
    4400,
    0
  )
  
  # -------------------------------------------------------------------------
  # Freshwater Marsh (6410)
  # -------------------------------------------------------------------------
  
  FW_Marsh <- ifel(
    (habitat == 6410) & (topo >= HAT_SLR),
    6410,
    0
  )
  
  # -------------------------------------------------------------------------
  # Freshwater Swamp (6110)
  # -------------------------------------------------------------------------
  
  FW_Swamp <- ifel(
    (habitat == 6110) & (topo >= HAT_SLR),
    6110,
    0
  )
  
  # -------------------------------------------------------------------------
  # Seagrass (9113)
  # Shallow subtidal areas
  # -------------------------------------------------------------------------
  
  seagrass_convertible <- c(1800, 1820, 2100, 2200, 2400, 2550, 1900, 3200,
                            4100, 4400, 6110, 6410, 6600, 6420, 6425, 6510)
  is_seagrass_convertible <- habitat %in% seagrass_convertible
  
  seagrass <- ifel(
    ((habitat == 9113) & (topo >= (MLLW_SLR - 1.5))) |
    (is_seagrass_convertible & (topo < MLHW_SLR) & (topo >= (MLLW_SLR - 1.5))) |
    ((habitat == 6120) & (topo < MLW_SLR) & (topo >= (MLLW_SLR - 1.5)) & (Freshwater == 0)) |
    ((habitat == 6120) & (topo < MLHW_SLR) & (topo >= (MLLW_SLR - 1.5)) & (Freshwater == 1)),
    9113,
    0
  )
  
  # Continue in part 3 for final combination...
  
  HabitatAdjustment_part3(
    Habitats_Adjusted = Habitats_Adjusted,
    Upland_Developed_hard = Upland_Developed_hard,
    Upland_Developed_hard_2 = Upland_Developed_hard_2,
    Upland_Developed_soft = Upland_Developed_soft,
    Golf_soft = Golf_soft,
    Agriculture = Agriculture,
    Tree = Tree,
    Vineyard = Vineyard,
    Aquiculture = Aquiculture,
    beach = beach,
    subtidal = subtidal,
    openwater = openwater,
    tidalflat = tidalflat,
    mangroves = mangroves,
    SaltBarren = SaltBarren,
    highMarsh = highMarsh,
    JuncusMarsh = JuncusMarsh,
    Upland_Undeveloped = Upland_Undeveloped,
    Shrub = Shrub,
    Forest = Forest,
    TreePlant = TreePlant,
    FW_Marsh = FW_Marsh,
    FW_Swamp = FW_Swamp,
    seagrass = seagrass
  )
}
