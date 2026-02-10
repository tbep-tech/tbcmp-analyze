library(here)
library(tidyverse)

# Set the directory path
directory_path <- "./data/output/"

# Get list of all CSV files in the directory
csv_files <- list.files(path = directory_path,
                        pattern = "\\.csv$",
                        full.names = TRUE)

# Read and combine all CSV files
all_data <- csv_files |>
  lapply(read.csv) |>
  bind_rows() |>
  filter(value != 0 & !is.na(value)) |>
  mutate(hem_category = case_when(value %in% c(1100, 1200) ~ "Upland Developed - Hard",
                                  value %in% c(1800, 1820, 2100, 2200, 2400) ~ "Upland Developed - Soft",
                                  value %in% c(1900, 3200, 4100, 4400) ~ "Upland Undeveloped",
                                  value %in% c(5200) ~ "Open Freshwater",
                                  value %in% c(5400) ~ "Open Water",
                                  value %in% c(6110, 6410) ~ "Freshwater Marsh & Wetlands",
                                  value %in% c(6120) ~ "Mangroves",
                                  value %in% c(6420) ~ "High Marshes",
                                  value %in% c(6425) ~ "Juncus Marshes",
                                  value %in% c(6600) ~ "Salt Barrens",
                                  value %in% c(7100) ~ "Beach - Dune",
                                  value %in% c(6510) ~ "Subtidal - Tidal Flats",
                                  value %in% c(9113) ~ "Subtidal - Seagrass",
                                  TRUE ~ NA)) |>
  rename(county = id) |>
  group_by(county, land_policy, hem_category, yr) |>
  summarize(mean_acres = mean(acres),
            std_acres = sd(acres))
