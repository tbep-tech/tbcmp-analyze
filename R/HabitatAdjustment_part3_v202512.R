# ==============================================================================
# HABITAT ADJUSTMENT v202512 - Part 3
# Final Habitat Combination
# ==============================================================================

HabitatAdjustment_part3 <- function(
    Habitats_Adjusted,
    Upland_Developed_hard,
    Upland_Developed_hard_2,
    Upland_Developed_soft,
    Golf_soft,
    Agriculture,
    Tree,
    Vineyard,
    Aquiculture,
    beach,
    subtidal,
    openwater,
    tidalflat,
    mangroves,
    SaltBarren,
    highMarsh,
    JuncusMarsh,
    Upland_Undeveloped,
    Shrub,
    Forest,
    TreePlant,
    FW_Marsh,
    FW_Swamp,
    seagrass
) {

  # =========================================================================
  # SECTION 6: COMBINE ALL HABITATS
  # =========================================================================

  cat("\nSection 6: Combining all habitat classifications...\n")

  # Habitat priority order (from highest to lowest priority):
  # 1. Developed Hard Surface (1200)
  # 2. Developed Pervious (1100)
  # 3. Developed Low Density (1800)
  # 4. Golf Course (1820)
  # 5. Agriculture (2100)
  # 6. Tree Crop (2200)
  # 7. Vineyard (2400)
  # 8. Aquaculture (2550)
  # 9. Beach (7100)
  # 10. Subtidal (5400)
  # 11. Open Water (5200)
  # 12. Tidal Flat (6510)
  # 13. Mangrove (6120)
  # 14. Salt Barren (6600)
  # 15. High Marsh (6420)
  # 16. Juncus Marsh (6425)
  # 17. Upland Undeveloped (1900)
  # 18. Shrub (3200)
  # 19. Forest (4100)
  # 20. Tree Plantation (4400)
  # 21. Freshwater Marsh (6410)
  # 22. Freshwater Swamp (6110)
  # 23. Seagrass (9113)

  # Use nested ifel statements to implement priority
  combined_habitats <- ifel(
    Upland_Developed_hard == 1200, 1200,
    ifel(
      Upland_Developed_hard_2 == 1100, 1100,
      ifel(
        Upland_Developed_soft == 1800, 1800,
        ifel(
          Golf_soft == 1820, 1820,
          ifel(
            Agriculture == 2100, 2100,
            ifel(
              Tree == 2200, 2200,
              ifel(
                Vineyard == 2400, 2400,
                ifel(
                  Aquiculture == 2550, 2550,
                  ifel(
                    beach == 7100, 7100,
                    ifel(
                      subtidal == 5400, 5400,
                      ifel(
                        openwater == 5200, 5200,
                        ifel(
                          tidalflat == 6510, 6510,
                          ifel(
                            mangroves == 6120, 6120,
                            ifel(
                              SaltBarren == 6600, 6600,
                              ifel(
                                highMarsh == 6420, 6420,
                                ifel(
                                  JuncusMarsh == 6425, 6425,
                                  ifel(
                                    Upland_Undeveloped == 1900, 1900,
                                    ifel(
                                      Shrub == 3200, 3200,
                                      ifel(
                                        Forest == 4100, 4100,
                                        ifel(
                                          TreePlant == 4400, 4400,
                                          ifel(
                                            FW_Marsh == 6410, 6410,
                                            ifel(
                                              FW_Swamp == 6110, 6110,
                                              ifel(
                                                seagrass == 9113, 9113,
                                                0  # No habitat
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  # =========================================================================
  # SECTION 7: SAVE OUTPUT
  # =========================================================================

  cat("\nSection 7: Saving adjusted habitats...\n")

  if (!is.null(Habitats_Adjusted)) {
    cat(sprintf("  Output path: %s\n", Habitats_Adjusted))
    hem_class <- read_csv(file = here('data/hem_class_colors.csv')) |>
                 select(Value, ClassName)
    levels(combined_habitats) <- hem_class
    writeRaster(combined_habitats, Habitats_Adjusted, overwrite = TRUE, datatype = "INT2U", wopt = list(gdal = c("RAT=YES", "COMPRESS=LZW", "PREDICTOR=2")))
    cat("  Successfully saved!\n")
  }

  cat("\nHabitat adjustment complete!\n")

  return(combined_habitats)
}
