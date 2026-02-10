library(terra)

# Assume 'r' is your SpatRaster and 'polys' is your SpatVector (polygons)
# 1. Ensure polygons have unique IDs
polys$id <- 1:nrow(polys)

# 2. Prepare an empty list or vector to store results
results_list <- list()

# 3. Loop through each polygon
for (i in 1:nrow(polys)) {
  # Select single polygon
  single_poly <- polys[i, ]

  # 4. Crop raster to the polygon extent to save memory
  r_sub <- crop(r, single_poly)

  # 5. Extract values
  # 'exact=TRUE' can be used for more accuracy, but is slower
  extracted_data <- terra::extract(r_sub, single_poly, ID = FALSE)

  # 6. Summarize immediately if necessary (e.g., mean)
  # or store the raw data
  results_list[[i]] <- data.frame(id = single_poly$id, mean_val = mean(extracted_data[,1], na.rm = TRUE))

  # Clean up temporary raster
  rm(r_sub)
}

# 7. Combine results
final_results <- do.call(rbind, results_list)
