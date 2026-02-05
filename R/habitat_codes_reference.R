# ==============================================================================
# HABITAT CODES REFERENCE
# Tampa Bay Coastal Management Program (TBCMP)
# ==============================================================================

HABITAT_CODES <- data.frame(
  Code = c(
    # Developed
    1100, 1200, 1800, 1820,
    # Agricultural
    2100, 2200, 2400, 2550,
    # Upland Natural
    1900, 3200, 4100, 4400,
    # Open Water
    5200, 5400,
    # Wetlands
    6110, 6120, 6410, 6420, 6425, 6510, 6600,
    # Coastal
    7100, 9113
  ),
  Category = c(
    # Developed
    "Developed", "Developed", "Developed", "Developed",
    # Agricultural
    "Agricultural", "Agricultural", "Agricultural", "Agricultural",
    # Upland Natural
    "Upland Natural", "Upland Natural", "Upland Natural", "Upland Natural",
    # Open Water
    "Open Water", "Open Water",
    # Wetlands
    "Wetlands", "Wetlands", "Wetlands", "Wetlands", "Wetlands", "Wetlands", "Wetlands",
    # Coastal
    "Coastal", "Coastal"
  ),
  Name = c(
    # Developed
    "Developed, Pervious",
    "Developed, Hard Surface",
    "Developed, Low Density",
    "Golf Course",
    # Agricultural
    "Agriculture",
    "Tree Crops",
    "Vineyard",
    "Aquaculture",
    # Upland Natural
    "Upland Undeveloped",
    "Shrub/Scrub",
    "Forest",
    "Tree Plantation",
    # Open Water
    "Estuarine Open Water",
    "Subtidal (Deep Water)",
    # Wetlands
    "Freshwater Swamp",
    "Mangrove",
    "Freshwater Marsh",
    "Salt Marsh (High Marsh)",
    "Juncus Marsh (Low Marsh)",
    "Tidal Flat",
    "Salt Barren",
    # Coastal
    "Beach",
    "Seagrass (SAV)"
  ),
  Elevation_Range = c(
    # Developed
    "Above HAT (if not protected)",
    "Above HAT (if not protected)",
    "Above HAT (if not protected)",
    "Above HAT (if not protected)",
    # Agricultural
    "Above HAT (if not protected)",
    "Above HAT (if not protected)",
    "Above HAT (if not protected)",
    "Above HAT (if not protected)",
    # Upland Natural
    "Above HAT",
    "Above HAT",
    "Above HAT",
    "Above HAT",
    # Open Water
    "Variable",
    "Below MLLW - 1.5m",
    # Wetlands
    "Above HAT",
    "MLW to MHW (saltwater)",
    "Above HAT",
    "MHW to MHHW",
    "MTL to MHW (freshwater)",
    "MLHW to MTL (freshwater)",
    "MHHW to HAT",
    # Coastal
    "MLLW to HAT (near ocean)",
    "MLLW-1.5m to MLHW"
  ),
  Accretion_mm_yr = c(
    # Developed
    0, 0, 0, 0,
    # Agricultural
    0, 0, 0, 0,
    # Upland Natural
    0, 0, 0, 0,
    # Open Water
    0, 0,
    # Wetlands
    0,
    1.6,  # Mangrove
    0,
    1.6,  # Salt Marsh
    3.75, # Juncus Marsh
    0,
    0,
    # Coastal
    0, 0
  ),
  Notes = c(
    # Developed
    "Always protected",
    "Always protected",
    "Protected if Protect_Developed=TRUE",
    "Protected if Protect_Developed=TRUE",
    # Agricultural
    "Protected if Protect_Developed=TRUE",
    "Protected if Protect_Developed=TRUE",
    "Protected if Protect_Developed=TRUE",
    "Protected if Protect_Developed=TRUE",
    # Upland Natural
    "Can migrate upslope",
    "Can migrate upslope",
    "Can migrate upslope",
    "Can migrate upslope",
    # Open Water
    "Estuarine water body",
    "Deep subtidal, always submerged",
    # Wetlands
    "Freshwater forested wetland",
    "Saltwater, accretion enabled",
    "Freshwater herbaceous wetland",
    "Saltwater, accretion enabled",
    "Freshwater, accretion enabled",
    "Freshwater tidal zone",
    "Transitional upland/wetland",
    # Coastal
    "Near ocean (500m buffer)",
    "Shallow subtidal/intertidal"
  )
)

# Print formatted table
print(HABITAT_CODES)

# ==============================================================================
# ELEVATION THRESHOLD HIERARCHY
# ==============================================================================

cat("\n=====================================\n")
cat("TIDAL DATUM HIERARCHY\n")
cat("=====================================\n")
cat("HAT   (Highest Astronomical Tide)    - Upland / Salt Barren boundary\n")
cat("MHHW  (Mean Higher High Water)        - Salt Barren / High Marsh boundary\n")
cat("MHW   (Mean High Water)               - High Marsh / Low Marsh boundary\n")
cat("MTL   (Mean Tide Level)               - Mid-marsh transition\n")
cat("MLHW  (Mean Lower High Water)         - Tidal Flat upper limit\n")
cat("MLW   (Mean Low Water)                - Mangrove lower limit\n")
cat("MLLW  (Mean Lower Low Water)          - Subtidal / Seagrass boundary\n")
cat("      -1.5m below MLLW                - Deep subtidal threshold\n")
cat("=====================================\n\n")

# ==============================================================================
# HABITAT TRANSITION RULES
# ==============================================================================

cat("=====================================\n")
cat("KEY HABITAT TRANSITION RULES\n")
cat("=====================================\n\n")

cat("1. FRESHWATER INFLUENCE:\n")
cat("   - Freshwater = 1: Enables Juncus marsh, tidal flats\n")
cat("   - Freshwater = 0: Enables mangroves\n\n")

cat("2. MARSH ACCRETION:\n")
cat("   - Mangrove: 1.6 mm/yr\n")
cat("   - Salt Marsh: 1.6 mm/yr\n")
cat("   - Juncus Marsh: 3.75 mm/yr\n\n")

cat("3. DEVELOPMENT PROTECTION:\n")
cat("   - Protect_Developed = TRUE: All development protected\n")
cat("   - Protect_Developed = FALSE: Only above HAT+SLR protected\n\n")

cat("4. HABITAT PRIORITY (highest to lowest):\n")
cat("   1. Developed Hard Surface (1200)\n")
cat("   2. Developed Pervious (1100)\n")
cat("   3. Other developed (1800, 1820, etc.)\n")
cat("   4. Beach (7100)\n")
cat("   5. Marine (5400, 5200)\n")
cat("   6. Coastal wetlands (6510, 6120, etc.)\n")
cat("   7. Upland natural (1900, 3200, 4100, 4400)\n")
cat("   8. Freshwater wetlands (6110, 6410)\n")
cat("   9. Seagrass (9113)\n\n")

cat("=====================================\n")
cat("CONVERSION EXAMPLES\n")
cat("=====================================\n\n")

cat("Example 1: Agriculture to Salt Marsh\n")
cat("  - Initial: Agriculture (2100) at 0.3m elevation\n")
cat("  - Scenario: HAT+SLR = 0.9m, MHW+SLR = 0.5m, MHHW+SLR = 0.6m\n")
cat("  - Result: Converts to High Marsh (6420)\n")
cat("  - Reason: Elevation (0.3m) is between MHW (0.5m) and MHHW (0.6m)\n\n")

cat("Example 2: Developed Low Density (Protected)\n")
cat("  - Initial: Low Density Developed (1800) at 0.2m\n")
cat("  - Scenario: HAT+SLR = 0.9m, Protect_Developed = TRUE\n")
cat("  - Result: Remains as 1800\n")
cat("  - Reason: Protection policy prevents conversion\n\n")

cat("Example 3: Developed Low Density (Not Protected)\n")
cat("  - Initial: Low Density Developed (1800) at 0.2m\n")
cat("  - Scenario: HAT+SLR = 0.9m, Protect_Developed = FALSE, near ocean\n")
cat("  - Result: Converts to Beach (7100)\n")
cat("  - Reason: Below HAT, within 500m of ocean\n\n")

# ==============================================================================
# SAVE REFERENCE TABLE
# ==============================================================================

# Save as CSV for easy reference
write.csv(HABITAT_CODES, "habitat_codes_reference.csv", row.names = FALSE)
cat("Reference table saved to: habitat_codes_reference.csv\n")
