################################################################################
# Land Use Change Hot Spot Analysis
# This script compares two raster land use datasets and identifies
# spatial hot spots of change using Getis-Ord Gi* statistics
################################################################################

# Install and load required packages
required_packages <- c("terra", "sf", "spdep", "ggplot2", "viridis",
                       "gridExtra", "raster")

# Function to install missing packages
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
}

install_if_missing(required_packages)

# Load libraries
library(terra)
library(sf)
library(spdep)
library(ggplot2)
library(viridis)
library(gridExtra)

################################################################################
# SECTION 1: Load and Prepare Data
################################################################################

# Load raster datasets
# Replace these paths with your actual data
raster_time1 <- rast("./data/tbcmp_hem_filled.tif")  # Baseline time period (2025)
raster_time2 <- rast("./data/output/tbcmp_PD_LA_IntHi_2100.tif")  # Later time period (2050, 2080, 2100)

# Ensure rasters have the same extent and resolution
if (!compareGeom(raster_time1, raster_time2, stopOnError = FALSE)) {
  cat("Resampling raster_time2 to match raster_time1...\n")
  raster_time2 <- resample(raster_time2, raster_time1, method = "near")
}

################################################################################
# SECTION 2: Calculate Change
################################################################################

# Create binary change raster (0 = no change, 1 = change)
change_raster <- raster_time1 != raster_time2
names(change_raster) <- "change"

# Calculate magnitude of change (difference in class values)
magnitude_change <- abs(raster_time2 - raster_time1)
names(magnitude_change) <- "magnitude"

# Summary statistics
cat("\n=== Change Summary ===\n")
cat("Total cells:", ncell(change_raster), "\n")
cat("Cells changed:", sum(values(change_raster), na.rm = TRUE), "\n")
cat("Percent changed:",
    round(100 * sum(values(change_raster), na.rm = TRUE) / ncell(change_raster), 2),
    "%\n\n")

################################################################################
# SECTION 3: Hot Spot Analysis (Getis-Ord Gi*)
################################################################################

# Convert change raster to points for spatial analysis
change_points <- as.points(change_raster)
change_sf <- st_as_sf(change_points)

# Extract coordinates and values
coords <- st_coordinates(change_sf)
change_values <- change_sf$change

# Create spatial weights matrix (k-nearest neighbors)
# Adjust k based on your data density
k <- 8  # Number of nearest neighbors

cat("Creating spatial weights matrix...\n")
nb <- knn2nb(knearneigh(coords, k = k))
weights <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Calculate Getis-Ord Gi* statistic
cat("Calculating Getis-Ord Gi* hot spot statistics...\n")
gi_star <- localG(change_values, weights, zero.policy = TRUE)

# Add Gi* values back to sf object
change_sf$gi_star <- as.vector(gi_star)

# Classify hot spots and cold spots
# |Gi*| > 2.58 = 99% confidence
# |Gi*| > 1.96 = 95% confidence
# |Gi*| > 1.65 = 90% confidence

change_sf$hotspot_class <- cut(change_sf$gi_star,
                               breaks = c(-Inf, -2.58, -1.96, -1.65,
                                          1.65, 1.96, 2.58, Inf),
                               labels = c("Cold Spot (99%)", "Cold Spot (95%)",
                                          "Cold Spot (90%)", "Not Significant",
                                          "Hot Spot (90%)", "Hot Spot (95%)",
                                          "Hot Spot (99%)"))

# Create raster of Gi* values
gi_star_raster <- rasterize(vect(change_sf), raster_time1, field = "gi_star")
names(gi_star_raster) <- "Gi_star"

################################################################################
# SECTION 4: Visualization
################################################################################

# Convert to data frames for ggplot
df_change <- as.data.frame(change_raster, xy = TRUE)
df_gi <- as.data.frame(gi_star_raster, xy = TRUE)
df_hotspot <- as.data.frame(change_sf)

# Plot 1: Original change detection
p1 <- ggplot(df_change, aes(x = x, y = y, fill = as.factor(change))) +
  geom_raster() +
  scale_fill_manual(values = c("0" = "gray90", "1" = "red3"),
                    labels = c("No Change", "Change"),
                    name = "Land Use\nChange") +
  coord_equal() +
  theme_minimal() +
  labs(title = "Land Use Change Detection",
       x = "X Coordinate", y = "Y Coordinate") +
  theme(legend.position = "bottom")

# Plot 2: Gi* values (continuous)
p2 <- ggplot(df_gi, aes(x = x, y = y, fill = Gi_star)) +
  geom_raster() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, name = "Gi* Statistic") +
  coord_equal() +
  theme_minimal() +
  labs(title = "Getis-Ord Gi* Hot Spot Analysis",
       subtitle = "Red = Hot Spots (clustering of change), Blue = Cold Spots",
       x = "X Coordinate", y = "Y Coordinate") +
  theme(legend.position = "bottom")

# Plot 3: Classified hot spots
p3 <- ggplot(df_hotspot, aes(x = X, y = Y, color = hotspot_class)) +
  geom_point(size = 0.5, alpha = 0.6) +
  scale_color_manual(values = c(
    "Cold Spot (99%)" = "#0000FF",
    "Cold Spot (95%)" = "#6666FF",
    "Cold Spot (90%)" = "#9999FF",
    "Not Significant" = "gray80",
    "Hot Spot (90%)" = "#FF9999",
    "Hot Spot (95%)" = "#FF6666",
    "Hot Spot (99%)" = "#FF0000"
  ), name = "Hot Spot\nClassification") +
  coord_equal() +
  theme_minimal() +
  labs(title = "Classified Hot Spots of Land Use Change",
       subtitle = "Based on statistical significance levels",
       x = "X Coordinate", y = "Y Coordinate") +
  theme(legend.position = "bottom")

# Combine plots
combined_plot <- grid.arrange(p1, p2, p3, ncol = 2)

# Save combined plot
ggsave("landuse_hotspot_analysis.png", combined_plot,
       width = 14, height = 10, dpi = 300)

cat("\nPlot saved as 'landuse_hotspot_analysis.png'\n")

################################################################################
# SECTION 5: Summary Statistics and Export
################################################################################

# Hot spot summary
hotspot_summary <- table(change_sf$hotspot_class)
cat("\n=== Hot Spot Classification Summary ===\n")
print(hotspot_summary)
cat("\n")

# Calculate percentage in each category
hotspot_percent <- prop.table(hotspot_summary) * 100
cat("=== Hot Spot Percentages ===\n")
print(round(hotspot_percent, 2))
cat("\n")

# Export results
# Save Gi* raster
writeRaster(gi_star_raster, "gi_star_results.tif", overwrite = TRUE)
cat("Gi* raster saved as 'gi_star_results.tif'\n")

# Save change raster
writeRaster(change_raster, "change_detection.tif", overwrite = TRUE)
cat("Change raster saved as 'change_detection.tif'\n")

# Save hot spot points as shapefile
st_write(change_sf, "hotspot_points.shp", delete_dsn = TRUE)
cat("Hot spot points saved as 'hotspot_points.shp'\n")

# Create summary report
sink("hotspot_analysis_report.txt")
cat("===============================================\n")
cat("LAND USE CHANGE HOT SPOT ANALYSIS REPORT\n")
cat("===============================================\n\n")
cat("Analysis Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
cat("Input Parameters:\n")
cat("  - Number of nearest neighbors (k):", k, "\n")
cat("  - Raster dimensions:", nrow(raster_time1), "x", ncol(raster_time1), "\n")
cat("  - Total cells analyzed:", ncell(change_raster), "\n\n")
cat("Change Detection Results:\n")
cat("  - Cells with change:", sum(values(change_raster), na.rm = TRUE), "\n")
cat("  - Percent changed:",
    round(100 * sum(values(change_raster), na.rm = TRUE) / ncell(change_raster), 2),
    "%\n\n")
cat("Hot Spot Classification:\n")
print(hotspot_summary)
cat("\nHot Spot Percentages:\n")
print(round(hotspot_percent, 2))
cat("\n===============================================\n")
sink()

cat("\nAnalysis complete! Summary report saved as 'hotspot_analysis_report.txt'\n")

################################################################################
# SECTION 6: Optional - Focused Hot Spot Maps
################################################################################

# Create a simplified binary hot/cold spot map (95% confidence)
change_sf$simplified_hotspot <- "Not Significant"
change_sf$simplified_hotspot[change_sf$gi_star >= 1.96] <- "Hot Spot (p < 0.05)"
change_sf$simplified_hotspot[change_sf$gi_star <= -1.96] <- "Cold Spot (p < 0.05)"

p4 <- ggplot(change_sf, aes(x = X, y = Y, color = simplified_hotspot)) +
  geom_point(size = 1, alpha = 0.7) +
  scale_color_manual(values = c(
    "Cold Spot (p < 0.05)" = "#0066CC",
    "Not Significant" = "gray85",
    "Hot Spot (p < 0.05)" = "#CC0000"
  ), name = "") +
  coord_equal() +
  theme_minimal() +
  labs(title = "Significant Hot Spots of Land Use Change (α = 0.05)",
       x = "X Coordinate", y = "Y Coordinate") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"))

ggsave("simplified_hotspots.png", p4, width = 10, height = 8, dpi = 300)

cat("Simplified hot spot map saved as 'simplified_hotspots.png'\n")
cat("\n=== Analysis Complete ===\n")
