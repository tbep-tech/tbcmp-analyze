# ==============================================================================
# EXAMPLE WORKFLOW: Habitat Evolution Model
# ==============================================================================
# This script demonstrates how to use the converted R scripts
# Modify paths and parameters for your specific project
# ==============================================================================

# Clean environment
rm(list = ls())
gc()

# ==============================================================================
# STEP 1: LOAD REQUIRED LIBRARIES
# ==============================================================================

library(terra)
library(sf)
library(foreign)
library(dplyr)

# ==============================================================================
# STEP 2: SET WORKING DIRECTORY AND SOURCE FUNCTIONS
# ==============================================================================

# Set your working directory
setwd("path/to/your/project")

# Source all R scripts
source("DatumAdjustment_v202512.R")
source("MarshAccretion_v202512.R")
source("Ocean2Beach_v202512.R")
source("ProcessFreshwater_v202512.R")
source("HabitatAdjustment_v202512.R")
source("HabitatAdjustment_part2_v202512.R")
source("HabitatAdjustment_part3_v202512.R")
source("HabitatEvolutionModel_v202512.R")

# ==============================================================================
# STEP 3: DEFINE INPUT PATHS
# ==============================================================================

# Input data paths
slr_table_path <- "D:/gis/TBEP/TBCMP/SLR_StPete_NAVD88_IntHigh.dbf"
datums_table_path <- "D:/gis/TBEP/TBCMP/Datum_StPete_NAVD88.dbf"
habitat_raster_path <- "D:/gis/TBEP/TBCMP/TBCMP_TBEP_DATASETS.gdb/TBCMP_HEM_CODES_UPDATED_2m"
dem_raster_path <- "D:/gis/TBEP/TBCMP/TBCMP_TBEP_DATASETS.gdb/TBCMP_SLAMM_DEM_NULL0_2m"
freshwater_polygon_path <- "D:/gis/TBEP/TBCMP/TBCMP_TBEP_DATASETS.gdb/FL_NHD_24k_CLIP_500mBUFFER_SP"

# Output directory
output_dir <- "D:/gis/TBEP/TBCMP/R_outputs"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ==============================================================================
# STEP 4: INSPECT INPUT DATA (OPTIONAL BUT RECOMMENDED)
# ==============================================================================

cat("Inspecting input data...\n\n")

# Check SLR table
slr_data <- read.dbf(slr_table_path)
cat("SLR Scenarios:\n")
print(head(slr_data))
cat("\n")

# Check Datums table
datums_data <- read.dbf(datums_table_path)
cat("Tidal Datums:\n")
print(datums_data)
cat("\n")

# Load and inspect rasters
habitat <- rast(habitat_raster_path)
cat("Habitat Raster:\n")
print(habitat)
cat("\n")

cat("Unique habitat codes:\n")
print(sort(unique(values(habitat, na.rm = TRUE))))
cat("\n")

dem <- rast(dem_raster_path)
cat("DEM:\n")
print(dem)
cat("Elevation range:", range(values(dem, na.rm = TRUE)), "\n\n")

# ==============================================================================
# STEP 5: RUN FULL MODEL
# ==============================================================================

cat("========================================\n")
cat("Running Full Habitat Evolution Model\n")
cat("========================================\n\n")

# Run the model with your parameters
HabitatEvolutionModel_v202512(
  SLR_Table = slr_table_path,
  Datums_Table = datums_table_path,
  Protect_Developed = TRUE,
  FL_NHD_24k_CLIP_500mBUFFER_SP = freshwater_polygon_path,
  veg = habitat_raster_path,
  topo = dem_raster_path,
  Juncus_Marsh_Accretion_mm_yr = 3.75,
  Mangrove_Accretion_mm_yr = 1.6,
  Salt_Marsh_Accretion_mm_yr = 1.6,
  Topo_Year = 2025,
  output_template = file.path(output_dir, "habitat_{OutY}.tif")
)

# ==============================================================================
# STEP 6: POST-PROCESSING AND ANALYSIS (OPTIONAL)
# ==============================================================================

cat("\n========================================\n")
cat("Post-Processing\n")
cat("========================================\n\n")

# List all output files
output_files <- list.files(output_dir, pattern = "^habitat_.*\\.tif$", full.names = TRUE)
cat("Generated outputs:\n")
print(output_files)
cat("\n")

# Load outputs for analysis
if (length(output_files) > 0) {
  
  # Load first and last timestep
  initial_habitat <- habitat  # Original
  final_habitat <- rast(output_files[length(output_files)])
  
  # Calculate habitat change
  cat("Calculating habitat changes...\n")
  
  # Reclassify to broad categories for easier analysis
  # You can customize this based on your needs
  reclass_matrix <- matrix(c(
    1100, 1899, 1,  # Developed
    1900, 1900, 2,  # Upland Undeveloped
    2100, 2599, 3,  # Agriculture
    3200, 3200, 4,  # Shrub
    4100, 4400, 5,  # Forest
    5200, 5400, 6,  # Open Water
    6110, 6110, 7,  # FW Swamp
    6120, 6120, 8,  # Mangrove
    6410, 6425, 9,  # Marsh
    6510, 6600, 10, # Tidal/Barren
    7100, 7100, 11, # Beach
    9113, 9113, 12  # Seagrass
  ), ncol = 3, byrow = TRUE)
  
  initial_broad <- classify(initial_habitat, reclass_matrix, others = 0)
  final_broad <- classify(final_habitat, reclass_matrix, others = 0)
  
  # Calculate area statistics
  cat("\nInitial habitat areas (pixels):\n")
  print(freq(initial_broad))
  
  cat("\nFinal habitat areas (pixels):\n")
  print(freq(final_broad))
  
  # Calculate change raster
  change_raster <- final_broad - initial_broad
  
  # Save change raster
  change_path <- file.path(output_dir, "habitat_change.tif")
  writeRaster(change_raster, change_path, overwrite = TRUE)
  cat(sprintf("\nChange raster saved to: %s\n", change_path))
  
  # Summary statistics
  cat("\nChange Summary:\n")
  cat("  Pixels with loss (negative change):", 
      sum(values(change_raster, na.rm = TRUE) < 0), "\n")
  cat("  Pixels with gain (positive change):", 
      sum(values(change_raster, na.rm = TRUE) > 0), "\n")
  cat("  Pixels unchanged:", 
      sum(values(change_raster, na.rm = TRUE) == 0), "\n")
}

# ==============================================================================
# STEP 7: CREATE SIMPLE VISUALIZATIONS (OPTIONAL)
# ==============================================================================

if (length(output_files) > 0 && requireNamespace("graphics", quietly = TRUE)) {
  
  cat("\nCreating visualizations...\n")
  
  # Create output plot directory
  plot_dir <- file.path(output_dir, "plots")
  if (!dir.exists(plot_dir)) {
    dir.create(plot_dir, recursive = TRUE)
  }
  
  # Plot initial vs final habitat
  png(file.path(plot_dir, "habitat_comparison.png"), 
      width = 1200, height = 600, res = 100)
  par(mfrow = c(1, 2))
  plot(initial_broad, main = "Initial Habitat (2025)", 
       col = terrain.colors(12))
  plot(final_broad, main = sprintf("Final Habitat (%s)", 
                                     gsub(".*_(\\d{4})\\.tif", "\\1", 
                                          output_files[length(output_files)])),
       col = terrain.colors(12))
  dev.off()
  
  cat("Visualization saved to:", file.path(plot_dir, "habitat_comparison.png"), "\n")
}

# ==============================================================================
# COMPLETION
# ==============================================================================

cat("\n========================================\n")
cat("Workflow Complete!\n")
cat("========================================\n")
cat("Check outputs in:", output_dir, "\n")
