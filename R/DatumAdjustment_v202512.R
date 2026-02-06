# ==============================================================================
# DATUM ADJUSTMENT v202512
# Converted from ArcGIS Python to R
# ==============================================================================
# Purpose: Adjust tidal datums for sea level rise scenarios
# ==============================================================================

library(foreign)  # For reading .dbf files
library(dplyr)

# ==============================================================================
# HELPER FUNCTION: Check if field exists
# ==============================================================================

ifFieldExists <- function(input_data, field_name) {
  return(!field_name %in% names(input_data))
}

# ==============================================================================
# MAIN FUNCTION
# ==============================================================================

DatumAdjustment_v202512 <- function(
    Datums = "./data-raw/tbep/Datum_StPete_NAVD88.csv",
    OutY = 2025,
    SLR_row = NULL
) {

  # Configuration
  Topo_Year_SLR <- 0

  # Read Datums table
  if (is.character(Datums)) {
    datums_data <- read.dbf(Datums)
  } else {
    datums_data <- Datums
  }

  # Get SLR value from SLR_row
  # Assuming SLR_row has a column with SLR values
  # Common column names: SLR, SLR_m, SeaLevelRise, etc.
  slr_col_names <- c("SLR", "SLR_m", "SeaLevelRise", "Rise")
  SLR <- NULL

  for (col_name in slr_col_names) {
    if (col_name %in% names(SLR_row)) {
      SLR <- SLR_row[[col_name]]
      break
    }
  }

  if (is.null(SLR)) {
    # If no SLR column found, try to use any numeric column after Year
    numeric_cols <- names(SLR_row)[sapply(SLR_row, is.numeric)]
    numeric_cols <- numeric_cols[numeric_cols != "Year"]
    if (length(numeric_cols) > 0) {
      SLR <- SLR_row[[numeric_cols[1]]]
      cat(sprintf("Using column '%s' for SLR value: %.3f m\n", numeric_cols[1], SLR))
    } else {
      stop("Could not find SLR value in SLR_row")
    }
  }

  # Create new field name
  field_name <- paste0("Dat_", OutY)

  # Check if field exists
  output_value <- ifFieldExists(datums_data, field_name)

  # Add field if it doesn't exist
  if (output_value) {
    cat(sprintf("Adding field: %s\n", field_name))
    datums_data[[field_name]] <- NA
  }

  # Calculate adjusted datum values
  # Formula: DatumEleva + SLR - Topo_Year_SLR
  if ("DatumEleva" %in% names(datums_data)) {
    datums_data[[field_name]] <- round(datums_data$DatumEleva + SLR - Topo_Year_SLR, 3)
  } else if ("Elevation" %in% names(datums_data)) {
    datums_data[[field_name]] <- round(datums_data$Elevation + SLR - Topo_Year_SLR, 3)
  } else {
    stop("Could not find elevation column in Datums table")
  }

  # Extract individual datum values
  # Assuming there's a 'Datum' column with datum names
  datum_col <- NULL
  if ("Datum" %in% names(datums_data)) {
    datum_col <- "Datum"
  } else if ("DatumType" %in% names(datums_data)) {
    datum_col <- "DatumType"
  } else if ("Type" %in% names(datums_data)) {
    datum_col <- "Type"
  } else {
    stop("Could not find datum type column in Datums table")
  }

  # Extract values for each datum type
  HAT_SLR <- datums_data[[field_name]][datums_data[[datum_col]] == "HAT"]
  MHHW_SLR <- datums_data[[field_name]][datums_data[[datum_col]] == "MHHW"]
  MTL_SLR <- datums_data[[field_name]][datums_data[[datum_col]] == "MTL"]
  MLHW_SLR <- datums_data[[field_name]][datums_data[[datum_col]] == "MLHW"]
  MHW_SLR <- datums_data[[field_name]][datums_data[[datum_col]] == "MHW"]
  MLW_SLR <- datums_data[[field_name]][datums_data[[datum_col]] == "MLW"]
  MLLW_SLR <- datums_data[[field_name]][datums_data[[datum_col]] == "MLLW"]

  # Handle cases where datum might not be found
  if (length(HAT_SLR) == 0) HAT_SLR <- NA
  if (length(MHHW_SLR) == 0) MHHW_SLR <- NA
  if (length(MTL_SLR) == 0) MTL_SLR <- NA
  if (length(MLHW_SLR) == 0) MLHW_SLR <- NA
  if (length(MHW_SLR) == 0) MHW_SLR <- NA
  if (length(MLW_SLR) == 0) MLW_SLR <- NA
  if (length(MLLW_SLR) == 0) MLLW_SLR <- NA

  # Return results as a list
  results <- list(
    HAT_SLR = HAT_SLR,
    MHHW_SLR = MHHW_SLR,
    MTL_SLR = MTL_SLR,
    MLHW_SLR = MLHW_SLR,
    MHW_SLR = MHW_SLR,
    MLW_SLR = MLW_SLR,
    MLLW_SLR = MLLW_SLR,
    adjusted_datums = datums_data
  )

  return(results)
}
