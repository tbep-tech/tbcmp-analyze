# ---------------------------------------------------------------------------
# 04b_HEM_Change_Summary_by_Basin.R
#
# HEM change summary by SWFWMD/FDEP drainage basin, with an explicit
# "Coastal Waters" residual for project-area cells that fall OUTSIDE the
# Drainage Basin Boundaries layer.
#
# Companion to:
#   03c_process_tiffs_basins.R (per-basin GeoTIFF summaries)
#   04_HEM_Change_Summary_by_County.R (same tables, county units)
#
# Why a residual is needed
# ------------------------
# The basin layer covers 3,660,058 of the 6,551,125 acres in the project-area
# raster (55.9%). The remaining 2,891,067 acres (44.1%) are open Gulf and
# nearshore waters with no basin assignment. Summarizing only the basin CSVs
# silently drops ~44% of the raster, almost all of it HEM 5400 (Open Water)
# and 9113 (Subtidal - Seagrass), which is exactly where SLR-driven change
# concentrates. This script closes that gap with st_difference().
#
# Note: Tampa Bay proper is NOT part of the residual. It is carried in the
# basin layer as FEATURE == "BAY" (HUC 03100206 / EXTHUC 99990000 /
# BASIN "TAMPA BAY"), as are Sarasota Bay, Boca Ciega Bay and others. The
# residual is genuinely offshore + unassigned nearshore, not the estuary.
#
# Tampa Bay Estuary Program / Tampa Bay Coastal Master Plan
# ---------------------------------------------------------------------------

library(here)
library(tidyverse)
library(sf)
library(terra)
library(exactextractr)
library(flextable)
library(ftExtra)
options(scipen = 999)

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
geotiff_dir  <- here("data/output/raster/")
basin_csvdir <- here("data/output/basin/")     # written by 03c
coast_csvdir <- here("data/output/coastal/")   # written by Part B below
basin_shp    <- here("data-raw/swfwmd/Drainage_Basin_Boundaries.shp")
coastal_rda  <- here("data/tbcmp_coastal.Rdata")

# Acres per cell. 0.000988422 = 4 m^2 (2x2m HEM outputs). Must match 03c.
cell_acres <- 0.000988422

# Tile size in metres for extracting the coastal residual. The offshore
# polygon is ~2.9M acres; at 2m resolution that is ~2.9 billion cells and a
# single exact_extract() call will exhaust memory. 10km tiles keep each call
# to roughly 25M cells.
tile_size_m <- 10000

# Drop residual fragments smaller than this. Differencing the AOI against the
# basin union yields one 2,891,065-acre part plus 32 slivers totalling
# 2.16 acres. The threshold exists only to discard those; dropped area is
# reported so the accounting stays honest.
min_part_acres <- 1

# Rebuild geometry / re-extract even if cached outputs exist
force_rebuild <- FALSE

# ---------------------------------------------------------------------------
# HEM recode helper
#
# 04_HEM_Change_Summary_by_County.R repeats this case_when three times. The
# label strings below are copied verbatim from that script so the outputs of
# the two remain joinable. They intentionally differ from ClassName in
# data/hem_class_colors.csv ("High Marshes" vs "High Salt Marsh", etc.).
# ---------------------------------------------------------------------------
hem_recode <- function(value) {
  case_when(
    value %in% c(1100, 1200)                   ~ "Upland Developed - Hard",
    value %in% c(1800, 1820, 2100, 2200, 2400) ~ "Upland Developed - Soft",
    value %in% c(1900, 3200, 4100, 4400)       ~ "Upland Undeveloped",
    value %in% c(5200)                         ~ "Open Freshwater",
    value %in% c(5400)                         ~ "Open Water",
    value %in% c(6110, 6410)                   ~ "Freshwater Marsh & Wetlands",
    value %in% c(6120)                         ~ "Mangroves",
    value %in% c(6420)                         ~ "High Marshes",
    value %in% c(6425)                         ~ "Juncus Marshes",
    value %in% c(6600)                         ~ "Salt Barrens",
    value %in% c(7100)                         ~ "Beach - Dune",
    value %in% c(6510)                         ~ "Subtidal - Tidal Flats",
    value %in% c(9113)                         ~ "Subtidal - Seagrass",
    TRUE                                       ~ NA_character_
  )
}

# ===========================================================================
# PART A - Build the coastal-waters residual via st_difference()
# ===========================================================================

if (!file.exists(coastal_rda) || force_rebuild) {

  cat("=== Part A: building coastal-waters residual ===\n")

  # AOI is the full extent of the project raster, which is the bounding box of
  # the 7-county area (see 01_data_prep.R). Using the raster extent rather
  # than the county union guarantees basins + coastal = 100% of every tiff.
  base_raster <- rast(here("data/tbcmp_base_raster_10m.tif"))
  prj         <- crs(base_raster)

  aoi <- st_bbox(base_raster) |>
    st_as_sfc() |>
    st_as_sf() |>
    st_set_crs(prj)

  aoi_acres <- as.numeric(st_area(aoi)) * 0.000247105
  cat("  AOI:", format(round(aoi_acres), big.mark = ","), "acres\n")

  rm(base_raster); gc()

  # --- Basin union ---------------------------------------------------------
  # Source layer is NAD83(HARN) / Florida West ftUS; st_transform handles it.
  basins <- st_read(basin_shp, quiet = TRUE) |>
    st_transform(prj) |>
    st_make_valid()

  basins_u <- basins |>
    st_geometry() |>
    st_union() |>
    st_make_valid()

  basins_in_aoi <- st_intersection(basins_u, st_geometry(aoi))
  basin_acres   <- as.numeric(st_area(basins_in_aoi)) * 0.000247105
  cat("  Drainage basins within AOI:", format(round(basin_acres), big.mark = ","),
      "acres (", round(100 * basin_acres / aoi_acres, 1), "%)\n")

  # --- The difference ------------------------------------------------------
  # Difference against the AOI, NOT against the county layer. The basin layer
  # is internally topologically clean, so AOI - basins gives one contiguous
  # part. Differencing TIGER county lines (a different source at a different
  # scale) against USGS-quad basin lines would generate thousands of shoreline
  # slivers instead.
  coastal_raw <- st_difference(st_geometry(aoi), basins_u) |>
    st_make_valid()

  coastal_parts <- coastal_raw |>
    st_cast("POLYGON", warn = FALSE) |>
    st_as_sf() |>
    mutate(part_acres = as.numeric(st_area(geometry)) * 0.000247105)

  dropped <- coastal_parts |> filter(part_acres < min_part_acres)
  cat("  Residual parts:", nrow(coastal_parts),
      "| dropping", nrow(dropped), "fragments <", min_part_acres, "acre(s) totalling",
      round(sum(dropped$part_acres), 2), "acres\n")

  coastal_parts <- coastal_parts |> filter(part_acres >= min_part_acres)

  # --- Attribute the residual to counties ----------------------------------
  # Intersecting the (clean) residual with counties labels nearshore water by
  # jurisdiction; whatever is left is offshore Gulf. This is the labelling
  # step, not the differencing step, so it introduces no slivers of its own.
  load(file = here("data/tbcmp_cnt.Rdata"))
  tbcmp_cnt <- st_transform(tbcmp_cnt, prj) |> st_make_valid()

  coastal_cnt <- st_intersection(coastal_parts, tbcmp_cnt) |>
    st_make_valid() |>
    group_by(county) |>
    summarise(.groups = "drop") |>
    mutate(
      unit_id   = paste0("COASTAL_", str_replace_all(county, "\\s+", "_")),
      unit_name = paste(county, "Coastal Waters"),
      unit_huc  = NA_character_,
      unit_feat = "COASTAL"
    ) |>
    select(unit_id, unit_name, unit_huc, unit_feat)

  offshore <- st_difference(
    st_union(st_geometry(coastal_parts)),
    st_union(st_geometry(tbcmp_cnt))
  ) |>
    st_make_valid() |>
    st_as_sf() |>
    rename(geometry = 1) |>
    mutate(
      unit_id   = "COASTAL_Offshore",
      unit_name = "Offshore Gulf Waters",
      unit_huc  = NA_character_,
      unit_feat = "COASTAL"
    ) |>
    select(unit_id, unit_name, unit_huc, unit_feat)

  tbcmp_coastal <- bind_rows(coastal_cnt, offshore) |>
    filter(!st_is_empty(geometry)) |>
    mutate(acres_geom = as.numeric(st_area(geometry)) * 0.000247105)

  cat("\n  Coastal units:\n")
  tbcmp_coastal |>
    st_drop_geometry() |>
    mutate(acres_geom = format(round(acres_geom), big.mark = ",")) |>
    select(unit_id, acres_geom) |>
    as.data.frame() |>
    print(row.names = FALSE)

  cat("\n  Reconciliation: basins + coastal =",
      format(round(basin_acres + sum(tbcmp_coastal$acres_geom)), big.mark = ","),
      "of", format(round(aoi_acres), big.mark = ","), "AOI acres\n\n")

  save(tbcmp_coastal, file = coastal_rda, compress = "xz")
  st_write(tbcmp_coastal, here("data/tbcmp_coastal.gpkg"),
           delete_dsn = TRUE, quiet = TRUE)   # for visual QA in GIS

  rm(basins, basins_u, basins_in_aoi, coastal_raw, coastal_parts, coastal_cnt, offshore)
  gc()

} else {
  cat("=== Part A: loading cached coastal residual ===\n")
  load(file = coastal_rda)
}

# ===========================================================================
# PART B - Extract HEM values for the coastal units
#
# Same crop/mask/exact_extract pattern as 03c, but tiled. Writes CSVs with the
# same column schema as the basin summaries so Part C can bind them directly.
# ===========================================================================

if (!dir.exists(coast_csvdir)) dir.create(coast_csvdir, recursive = TRUE)

geotiff_files <- list.files(geotiff_dir, pattern = "\\.tif$|\\.tiff$", full.names = TRUE)

cat("=== Part B: extracting coastal units from", length(geotiff_files), "rasters ===\n")

for (tiff_file in geotiff_files) {

  tiff_name   <- tools::file_path_sans_ext(basename(tiff_file))
  output_file <- file.path(coast_csvdir, paste0(tiff_name, "_coastal_summary.csv"))

  if (file.exists(output_file) && !force_rebuild) {
    cat("Skipping (exists):", tiff_name, "\n")
    next
  }

  cat("Processing:", tiff_name, "\n")

  parts        <- str_split(tiff_name, "_")[[1]]
  land_policy  <- if (length(parts) > 1) parts[2] else NA
  accretion    <- if (length(parts) > 2) parts[3] else NA
  slr_scenario <- if (length(parts) > 3) parts[4] else NA
  yr           <- if (length(parts) > 4) parts[5] else NA

  tbcmp_raster <- rast(tiff_file)
  coastal_sub  <- st_transform(tbcmp_coastal, crs(tbcmp_raster))

  unit_list <- list()

  for (i in seq_len(nrow(coastal_sub))) {

    single_poly <- coastal_sub[i, ]
    cat("  Unit", i, "of", nrow(coastal_sub), ":", single_poly$unit_id, "\n")

    # Tile the polygon so no single exact_extract() call blows out memory
    tiles <- st_make_grid(single_poly, cellsize = tile_size_m) |>
      st_as_sf() |>
      st_filter(single_poly) |>
      st_intersection(st_geometry(single_poly)) |>
      st_make_valid()

    tiles <- tiles[!st_is_empty(st_geometry(tiles)), ]

    if (nrow(tiles) == 0) next

    tile_res <- list()

    for (j in seq_len(nrow(tiles))) {

      if (j %% 25 == 0) cat("    tile", j, "of", nrow(tiles), "\n")

      tile_poly <- tiles[j, ]

      # Tiles at the AOI edge can fall outside the raster footprint
      r_sub <- try(crop(tbcmp_raster, tile_poly) |> mask(tile_poly), silent = TRUE)
      if (inherits(r_sub, "try-error") || ncell(r_sub) == 0) next

      extracted <- exact_extract(r_sub, tile_poly, fun = NULL, force_df = TRUE,
                                 max_cells_in_memory = 3e+08, progress = FALSE)

      tile_res[[j]] <- extracted |>
        bind_rows() |>
        filter(!is.na(value)) |>
        group_by(value) |>
        summarise(count = sum(coverage_fraction, na.rm = TRUE), .groups = "drop")

      rm(extracted, r_sub)
      gc()
    }

    if (length(tile_res) == 0) next

    # Sum across tiles back to the whole unit
    unit_list[[i]] <- bind_rows(tile_res) |>
      group_by(value) |>
      summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
      mutate(
        id           = single_poly$unit_id,
        basin_name   = single_poly$unit_name,
        basin_huc    = single_poly$unit_huc,
        basin_feat   = single_poly$unit_feat,
        filename     = tiff_name,
        land_policy  = land_policy,
        accretion    = accretion,
        slr_scenario = slr_scenario,
        yr           = yr
      ) |>
      select(id, value, count, basin_name, basin_huc, basin_feat,
             filename, land_policy, accretion, slr_scenario, yr)

    rm(tiles, tile_res)
    gc()
  }

  coastal_summary <- bind_rows(unit_list) |>
    mutate(acres = count * cell_acres)

  write.csv(coastal_summary, output_file, row.names = FALSE)
  cat("Saved results to:", output_file, "\n\n")

  rm(tbcmp_raster, unit_list)
  gc()
}

# ===========================================================================
# PART C - Combine basin + coastal summaries
# ===========================================================================

cat("=== Part C: combining summaries ===\n")

basin_csvs <- list.files(basin_csvdir, pattern = "\\.csv$", full.names = TRUE)
coast_csvs <- list.files(coast_csvdir, pattern = "\\.csv$", full.names = TRUE)

if (length(basin_csvs) == 0) {
  stop("No basin summaries found in ", basin_csvdir, ". Run 03c_process_tiffs_basins.R first.")
}

read_summaries <- function(files, domain_label) {
  files |>
    lapply(read.csv) |>
    bind_rows() |>
    mutate(domain = domain_label)
}

combined <- bind_rows(
  read_summaries(basin_csvs, "Drainage Basin"),
  read_summaries(coast_csvs, "Coastal Waters")
) |>
  filter(value != 0 & !is.na(value)) |>
  mutate(hem_category = hem_recode(value)) |>
  rename(unit = id)

# --- QA: how much of each raster is basin vs coastal? ----------------------
cat("\n  Domain split by scenario (acres):\n")
combined |>
  summarize(acres = sum(acres), .by = c(filename, domain)) |>
  pivot_wider(names_from = domain, values_from = acres) |>
  mutate(
    total       = `Drainage Basin` + `Coastal Waters`,
    pct_coastal = round(100 * `Coastal Waters` / total, 1)
  ) |>
  head(10) |>
  as.data.frame() |>
  print(row.names = FALSE)

# Anything that shifts between domains across scenarios means a unit boundary
# problem, not a habitat change - the geometry is static.
cat("\n")

all_data <- combined |>
  summarize(sum_acres = sum(acres),
            .by = c(unit, domain, land_policy, accretion, slr_scenario, hem_category, yr)) |>
  summarize(mean_acres = mean(sum_acres),
            std_acres  = sd(sum_acres),
            .by = c(unit, domain, land_policy, hem_category, yr))

# --- Baseline --------------------------------------------------------------
baseline <- combined |>
  filter(land_policy == "baseline" & yr == 2025) |>
  summarize(sum_acres = sum(acres), .by = c(domain, hem_category))

baseline_total <- baseline |>
  summarize(sum_acres = sum(sum_acres), .by = hem_category)

# --- Projections -----------------------------------------------------------
projections <- combined |>
  filter(yr != 2025) |>
  summarize(sum_acres = sum(acres),
            .by = c(unit, domain, land_policy, accretion, slr_scenario, hem_category, yr))

# Envelope across accretion x SLR, by domain
projection_summary <- projections |>
  summarize(min = min(sum_acres),
            max = max(sum_acres),
            .by = c(unit, domain, hem_category, yr)) |>
  summarize(sum_min = sum(min),
            sum_max = sum(max),
            .by = c(domain, hem_category, yr))

# --- Change relative to baseline, by domain --------------------------------
# This is the payoff: seagrass and open-water change in the coastal residual
# was invisible in the basin-only summaries.
domain_change <- all_data |>
  summarize(mean_acres = sum(mean_acres), .by = c(domain, land_policy, hem_category, yr)) |>
  left_join(
    baseline |> rename(baseline_acres = sum_acres),
    by = c("domain", "hem_category")
  ) |>
  mutate(
    delta_acres = mean_acres - baseline_acres,
    pct_change  = round(100 * delta_acres / baseline_acres, 1)
  ) |>
  arrange(hem_category, domain, yr, land_policy)

# ===========================================================================
# PART D - flextable output (structure follows 04_HEM_Change_Summary_by_County.R)
# ===========================================================================

build_hem_table <- function(dat, group_col) {

  group_col <- rlang::ensym(group_col)

  totab <- dat |>
    ungroup() |>
    pivot_longer(cols = c(mean_acres, std_acres),
                 names_to = "statistic", values_to = "acres") |>
    unite("land_policy_yr", land_policy, yr, sep = "_")

  totabave <- totab |>
    filter(statistic == "mean_acres") |>
    select(-statistic) |>
    pivot_wider(names_from = land_policy_yr, values_from = acres,
                names_sort = TRUE, values_fill = 0)

  totabstd <- totab |>
    filter(statistic == "std_acres") |>
    select(-statistic) |>
    pivot_wider(names_from = land_policy_yr, values_from = acres,
                names_sort = TRUE, values_fill = 0)

  totabcol <- totabave |>
    pivot_longer(cols = -c(!!group_col, hem_category, baseline_2025),
                 names_to = "scenario", values_to = "mean_acres") |>
    mutate(
      col = sign(mean_acres - baseline_2025),
      col = case_when(col == 1 ~ "green", col == -1 ~ "red", TRUE ~ "black"),
      scenario = factor(
        scenario,
        levels = c("PD_2050", "AM_2050", "PD_2080", "AM_2080", "PD_2100", "AM_2100"),
        labels = c("PD_2050_col", "AM_2050_col", "PD_2080_col", "AM_2080_col",
                   "PD_2100_col", "AM_2100_col")
      )
    ) |>
    select(-mean_acres, -baseline_2025) |>
    pivot_wider(names_from = scenario, values_from = col, names_sort = TRUE)

  totabave |>
    left_join(totabstd, by = c(rlang::as_string(group_col), "hem_category"),
              suffix = c("_mean", "_std")) |>
    pivot_longer(cols = c(ends_with("_mean"), ends_with("_std")),
                 names_to = c("land_policy", "yr", "statistic"),
                 names_pattern = "(.*)_(.*)_(.*)", values_to = "acres") |>
    mutate(
      acres = round(acres, 1),
      acres = case_when(yr == 2025 & statistic == "std" ~ NA_real_, TRUE ~ acres)
    ) |>
    pivot_wider(names_from = statistic, values_from = acres) |>
    mutate(
      mean = case_when(!is.na(mean) ~ format(mean, big.mark = ",", scientific = FALSE),
                       TRUE ~ NA_character_),
      std  = case_when(!is.na(std)  ~ format(std,  big.mark = ",", scientific = FALSE),
                       TRUE ~ NA_character_)
    ) |>
    unite("mean_std_acres", mean, std, sep = " ± ", na.rm = TRUE) |>
    unite("policy_yr", land_policy, yr, sep = "_") |>
    mutate(
      policy_yr = factor(policy_yr,
        levels = c("baseline_2025", "PD_2050", "AM_2050", "PD_2080",
                   "AM_2080", "PD_2100", "AM_2100"))
    ) |>
    pivot_wider(names_from = policy_yr, values_from = mean_std_acres,
                names_sort = TRUE, values_fill = "0") |>
    left_join(totabcol, by = c(rlang::as_string(group_col), "hem_category"),
              suffix = c("", "_col"))
}

render_hem_flextable <- function(tab, caption) {
  flextable(tab, col_keys = c("hem_category", "baseline_2025", "PD_2050", "AM_2050",
                              "PD_2080", "AM_2080", "PD_2100", "AM_2100")) |>
    add_header_row(values = c("", "", "2050", "2050", "2080", "2080", "2100", "2100")) |>
    merge_at(i = 1, j = 3:4, part = "header") |>
    merge_at(i = 1, j = 5:6, part = "header") |>
    merge_at(i = 1, j = 7:8, part = "header") |>
    set_header_labels(
      hem_category  = "HEM Category",
      baseline_2025 = "Baseline",
      PD_2050 = "PD", AM_2050 = "AM",
      PD_2080 = "PD", AM_2080 = "AM",
      PD_2100 = "PD", AM_2100 = "AM"
    ) |>
    color(j = c("PD_2050", "AM_2050", "PD_2080", "AM_2080", "PD_2100", "AM_2100"),
          color = c(tab$PD_2050_col, tab$AM_2050_col, tab$PD_2080_col,
                    tab$AM_2080_col, tab$PD_2100_col, tab$AM_2100_col)) |>
    set_caption(caption) |>
    autofit()
}

# --- Table 1: Drainage Basin vs Coastal Waters -----------------------------
domain_dat <- all_data |>
  summarize(mean_acres = sum(mean_acres),
            std_acres  = sqrt(sum(std_acres^2)),   # units treated as independent
            .by = c(domain, land_policy, hem_category, yr))

totab_domain <- build_hem_table(domain_dat, domain)

tab_coastal <- totab_domain |> filter(domain == "Coastal Waters") |> select(-domain)
tab_basin   <- totab_domain |> filter(domain == "Drainage Basin") |> select(-domain)

ft_coastal <- render_hem_flextable(tab_coastal, "Coastal Waters (outside Drainage Basin Boundaries)")
ft_basin   <- render_hem_flextable(tab_basin,   "Drainage Basins")

# --- Table 2: a single unit ------------------------------------------------
focal_unit <- "COASTAL_Offshore"   # e.g. "99990000_TAMPA_BAY", "COASTAL_Pinellas"

totab_unit <- build_hem_table(all_data |> select(-domain), unit)

tab_focal <- totab_unit |> filter(unit == focal_unit) |> select(-unit)
ft_focal  <- render_hem_flextable(tab_focal, focal_unit)

ft_coastal
ft_basin
ft_focal

cat("\nDone. Objects of interest: combined, all_data, baseline, baseline_total,\n",
    "projection_summary, domain_change, ft_basin, ft_coastal, ft_focal\n")
