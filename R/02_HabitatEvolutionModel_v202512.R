# ==============================================================================
# HABITAT EVOLUTION MODEL v202512
# Converted from ArcGIS Python to R
# ==============================================================================
# Purpose: Main workflow for modeling habitat changes under sea level rise scenarios
# Author: Converted from ArcGIS ModelBuilder export
# Date: 2026-02-04
# ==============================================================================

# Required Libraries
library(terra)      # Raster operations
library(sf)         # Vector operations
library(dplyr)      # Data manipulation
library(foreign)    # For reading .dbf files

# Source helper functions
source("R\DatumAdjustment_v202512.R")
source("R\MarshAccretion_v202512.R")
source("R\HabitatAdjustment_v202512.R")

# ==============================================================================
# MAIN FUNCTION
# ==============================================================================

HabitatEvolutionModel_v202512 <- function(
    Datums_Table = "./data-raw/tbep/Datum_StPete_NAVD88.csv",
	SLR_Table = "./data-raw/tbep/SLR_StPete_NAVD88_IntHigh.csv",
    Protect_Developed = TRUE,
    freshwater = "./data-raw/tbep/FL_NHD_24k_CLIP_500mBUFFER_Albers.shp",
    veg = "./data/tbcmp_hem_filled.tif",
    topo = "./data/tbcmp_dem.tif",
    Juncus_Marsh_Accretion_mm_yr = 3.75,
    Mangrove_Accretion_mm_yr = 1.6,
    Salt_Marsh_Accretion_mm_yr = 1.6,
    Topo_Year = 2025,
    output_template = "./data/output/tbcmp_PD_LA_IntHi_{OutY}.tif"
) {

  # Configuration
  Constant_Accretion <- TRUE
  Max_Accretion_mm_yr <- 4

  # Read SLR Table
  cat("Reading SLR scenarios table...\n")
  slr_data <- read.csv(SLR_Table)

  # Check if 'Year' column exists
  if (!"Year" %in% names(slr_data)) {
    stop("'Year' column not found in SLR table")
  }

  # Read initial rasters
  cat("Loading initial raster data...\n")
  veg_rast <- rast(veg)
  topo_rast <- rast(topo)

  # Iterate through each year in SLR table
  for (i in 1:nrow(slr_data)) {

    OutY <- slr_data$Year[i]
    SLR_row <- slr_data[i, ]

    cat(sprintf("\n========================================\n"))
    cat(sprintf("Processing Year: %d\n", OutY))
    cat(sprintf("========================================\n"))

    # -------------------------------------------------------------------------
    # STEP 1: Datum Adjustment
    # -------------------------------------------------------------------------
    cat("Step 1: Adjusting tidal datums for SLR...\n")

    datum_results <- DatumAdjustment_v202512(
      Datums = Datums_Table,
      OutY = OutY,
      SLR_row = SLR_row
    )

    HAT_SLR <- datum_results$HAT_SLR
    MHHW_SLR <- datum_results$MHHW_SLR
    MTL_SLR <- datum_results$MTL_SLR
    MLHW_SLR <- datum_results$MLHW_SLR
    MHW_SLR <- datum_results$MHW_SLR
    MLW_SLR <- datum_results$MLW_SLR
    MLLW_SLR <- datum_results$MLLW_SLR

    cat(sprintf("  HAT+SLR: %.3f m\n", HAT_SLR))
    cat(sprintf("  MHHW+SLR: %.3f m\n", MHHW_SLR))
    cat(sprintf("  MTL+SLR: %.3f m\n", MTL_SLR))

    # -------------------------------------------------------------------------
    # STEP 2: Marsh Accretion
    # -------------------------------------------------------------------------
    cat("Step 2: Calculating marsh accretion...\n")

    marshac_rast <- MarshAccretion_v202512(
      Juncus_Marsh_Accretion_mm_yr = Juncus_Marsh_Accretion_mm_yr,
      Salt_Marsh_Accretion_mm_yr = Salt_Marsh_Accretion_mm_yr,
      habitat = veg_rast,
      topo = topo_rast,
      Constant_Accretion = Constant_Accretion,
      Max_Accretion_mm_yr = Max_Accretion_mm_yr,
      MHHW = MHHW_SLR,
      MLLW = MLLW_SLR,
      Topo_Year = Topo_Year,
      OutY = OutY,
      Mangrove_Accretion_mm_yr = Mangrove_Accretion_mm_yr
    )

    # -------------------------------------------------------------------------
    # STEP 3: Habitat Adjustment
    # -------------------------------------------------------------------------
    cat("Step 3: Adjusting habitat classifications...\n")

    output_path <- gsub("\\{OutY\\}", OutY, output_template)

    HabitatAdjustment_v202512(
      topo = marshac_rast,
      habitat = veg_rast,
      Habitats_Adjusted = output_path,
      HAT_SLR = HAT_SLR,
      MHHW_SLR = MHHW_SLR,
      MTL_SLR = MTL_SLR,
      MLW_SLR = MLW_SLR,
      OutY = OutY,
      FW_Polygon = freshwater,
      Protect_Developed = Protect_Developed,
      MLLW_SLR = MLLW_SLR,
      MHW_SLR = MHW_SLR,
      MLHW_SLR = MLHW_SLR
    )

    # Update topology for next iteration
    topo_rast <- marshac_rast

    cat(sprintf("Completed processing for year %d\n", OutY))
    cat(sprintf("Output saved to: %s\n", output_path))
  }

  cat("\n========================================\n")
  cat("Habitat Evolution Model Complete!\n")
  cat("========================================\n")

  return(invisible(NULL))
}

# ==============================================================================
# SCRIPT EXECUTION (if run directly)
# ==============================================================================

if (!interactive()) {
  # Parse command line arguments if needed
  args <- commandArgs(trailingOnly = TRUE)

  # Run with default parameters or pass custom ones
  HabitatEvolutionModel_v202512()
}
