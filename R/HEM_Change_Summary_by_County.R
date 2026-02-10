library(here)
library(tidyverse)
library(flextable)
library(ftExtra)

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

# format for flextable
totab <- all_data |> 
  ungroup() |> 
  mutate(
    mean_acres = round(mean_acres, 0),
    mean_acres = case_when(
      !is.na(mean_acres) ~ format(mean_acres, big.mark = ",", scientific = FALSE), 
      TRUE ~ NA_character_
    ),
    std_acres = round(std_acres, 0),
    std_acres = case_when(
      !is.na(std_acres) ~ format(std_acres, big.mark = ",", scientific = FALSE), 
      TRUE ~ NA_character_
    )
  ) |>
  unite("mean_std_acres", mean_acres, std_acres, sep = " ± ", na.rm = T) |>
  unite('policy_yr', land_policy, yr, sep = "_") |>
  mutate(
    policy_yr = factor(policy_yr, levels = c('baseline_2025', 'PD_2050', 'AM_2050', 'PD_2080', 'AM_2080', 'PD_2100', 'AM_2100'))
  ) |>
  pivot_wider(names_from = policy_yr, values_from = mean_std_acres, names_sort = T, values_fill = "0") |> 
  arrange(
    county, hem_category
  )

# add flextable with extra header row for years
totab |> 
  filter(county == 'Citrus') |>
  select(-county) |> 
  flextable() |> 
  add_header_row(values = c("", "", "2050", "2050", "2080", "2800", "2100", "2100")) |>
  merge_at(i = 1, j = 3:4, part = 'header') |>
  merge_at(i = 1, j = 5:6, part = 'header') |>
  merge_at(i = 1, j = 7:8, part = 'header') |>
  set_header_labels(
    hem_category = "HEM Category",
    baseline_2025 = "Baseline",
    PD_2050 = "PD",
    AM_2050 = "AM",
    PD_2080 = "PD",
    AM_2080 = "AM",
    PD_2100 = "PD",
    AM_2100 = "AM"
  ) |> 
  autofit()
