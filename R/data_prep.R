library(here)
library(curl)
library(sf)
library(tbeptools)
library(tidycensus)
library(tidyverse)
library(terra)
library(exactextractr)
library(tmap)
#library(future)
#plan(multisession)

num_cores <- detectCores() - 1

##Download base files for prepping project area extent and existing conditions

#Establish base projection (Utilized in ArcGIS spatial analyses)
prj <- 3087

#TBCMP project area bounded by 7 county census areas including associated coastal areas tied to local/state jurisdictions
curl_download(url = "https://www2.census.gov/geo/tiger/TIGER2025/COUSUB/tl_2025_12_cousub.zip",
              destfile = "./data-raw/census/tl_2025_12_cousub.zip")
unzip("./data-raw/census/tl_2025_12_cousub.zip", exdir = "./data-raw/census/")

fl_counties <- st_read("./data-raw/census/tl_2025_12_cousub.shp")

tbcmp_cnt <- fl_counties |>
  st_transform(prj) |>
  filter(COUNTYFP %in% c("017","053","057","081","101","103","115")) |>
  mutate(county = case_when(COUNTYFP == "017" ~ "Citrus",
                            COUNTYFP == "053" ~ "Hernando",
                            COUNTYFP == "101" ~ "Pasco",
                            COUNTYFP == "103" ~ "Pinellas",
                            COUNTYFP == "057" ~ "Hillsborough",
                            COUNTYFP == "081" ~ "Manatee",
                            COUNTYFP == "115" ~ "Sarasota",
                            TRUE ~ "MISSING")) |>
  group_by(county) |>
  summarise()

tbcmp_bb <- st_bbox(tbcmp_cnt) |>
  st_as_sfc()

bb <- vect(tbcmp_bb)

#Create empty raster of project area that will be used to align and conduct subsequent spatial analyses
tbcmp_raster <- terra::rast(extent = bb,
                            res = 2,
                            crs = "EPSG:3087")

tbcmp_raster_GOM <- tbcmp_raster
values(tbcmp_raster_GOM) <- 5400

#Download land use and cover classifications, crosswalk to HEM codes, and then rasterize to project area empty rster above

#These SWFWMD layers can be recoded to extract through any web-based APIs in the future
lulc2023 <- st_read("T:/05_GIS/SWFWMD/LULC_2023/LANDUSELANDCOVER2023.shp") |>
            st_transform(prj) |>
            dplyr::rename(FLUCCSDESC = FLUCSDESC) |>
            dplyr::group_by(FLUCCSCODE, FLUCCSDESC) |>
            dplyr::summarise()

tbsg2024 <- st_read("T:/05_GIS/TBEP/TBCMP/Seagrass_in_2024.shp") |>
            st_transform(prj) |>
            dplyr::group_by(FLUCCSCODE, FLUCCSDESC) |>
            dplyr::summarise() |>
            dplyr::filter(FLUCCSCODE != 0)

spsg2024 <- st_read("T:/05_GIS/TBEP/TBCMP/Seagrass_in_2024_for_the_Springs_Coast.shp") |>
            st_transform(prj) |>
            dplyr::group_by(FLUCCSCODE, FLUCCSDESC) |>
            dplyr::summarise() |>
            dplyr::filter(FLUCCSCODE != 0)

sg_dif <- st_difference(tbsg2024, st_union(spsg2024))

sg2024 <- bind_rows(spsg2024, sg_dif) |>
          group_by(FLUCCSCODE, FLUCCSDESC) |>
          summarise()

lu_dif <- st_difference(lulc2023, st_union(sg2024))

#Get the FLUCCSCODE, HEM and SLAMM code crosswalk file previously made to perform next step.

#codes <- sf::st_read("T:/05_GIS/TBEP/TBCMP/TBCMP_TBEP_DATASETS.gdb", layer = "FINAL_FULL_EXTENT_DISSOLVED") |>
#         st_drop_geometry() |>
#         rename(HEM_CODE = HEM_Code) |>
#         distinct(FLUCCSCODE,SLAMM_CODE,HEM_CODE)

#save(codes, file = here('data/code_xwalk.Rdata'))

load(file = here('data/code_xwalk.Rdata'))

lulc23_sg24 <- bind_rows(sg2024, lu_dif)  |>
               mutate(FLUCCSDESC = case_when(FLUCCSCODE == 9121 ~ "Attached Algae",      #Correct for different descriptions in the DISTRICT layers
                                             FLUCCSCODE == 5400 ~ "BAYS AND ESTUARIES",  #Correct for different descriptions in the DISTRICT layers
                                             .default = FLUCCSDESC)) |>
               group_by(FLUCCSCODE, FLUCCSDESC) |>
               summarise() |>
               left_join(codes, by = "FLUCCSCODE")

save(lulc23_sg24, file = here('data/lulc23_sg24_base.RData'), compress = 'xz')

load(file = here('data/lulc23_sg24_base.RData'))

tbcmp_hem <- rasterize(vect(lulc23_sg24), tbcmp_raster, field = "HEM_CODE", fun = max)

tbcmp_hem_filled <- cover(tbcmp_hem, tbcmp_raster_GOM) #Filled null values for Gulf waters with HEM code 5400 (and a small portion of NW Citrus County with no LULC info)

hem_class <- read_csv(file = here('data/hem_class_colors.csv')) |>
             select(Value, ClassName)

hem_color <- read_csv(file = here('data/hem_class_colors.csv')) |>
             distinct(ClassName, color_hex) |>
             select(color_hex)

#hem_colors <- c('#FF7F7F', '#FFBEBE', '#E1E1E1', '#73DFFF', '#BEE8FF', '#FFFF73', '#55FF00', '#737300', '#FFAA00', '#FFD37F', '#267300')

levels(tbcmp_hem_filled) <- hem_class
#coltab(tbcmp_hem_filled) <- hem_color       #Not working to embed colors in raster

writeRaster(tbcmp_hem_filled, filename = here('data/tbcmp_hem_filled.tif'), overwrite = T, datatype = "INT2U", wopt = list(gdal = c("RAT=YES")))

#tbcmp_hem_filled <- rast(here('data/tbcmp_hem_filled.tif'))

tm_shape(tbcmp_hem_filled) +
  tm_raster(col = "ClassName", palette = hem_color, title = "Land Use") +   #View the pretty habitat/veg base layer
  tm_shape(tbcmp_cnt) +
  tm_polygons(fill_alpha = 0) +
  tm_layout(legend.outside = TRUE)

hem_summary <- freq(tbcmp_hem_filled) |>
               as.data.frame() |>
               group_by(value) |>
               summarise(sum = sum(count)) |>
               mutate(acres = sum * 0.000988422)

county_list <- list()

for (i in 1:nrow(tbcmp_cnt)) {
          single_poly <- tbcmp_cnt[i, ]
          r_sub <- crop(tbcmp_hem_filled, single_poly) |>
                   mask(single_poly)
          extracted <- exact_extract(r_sub, single_poly, fun = NULL, force_df = TRUE,
                                     max_cells_in_memory = 3e+08)
          county_list[[i]] <- data.frame(id = single_poly$county, extracted) |>
                              unnest() |>
                              group_by(id, value) |>
                              summarise(count = sum(coverage_fraction, na.rm = T))
          rm(extracted)
          rm(r_sub)
          gc()
}

county_summary <- do.call(rbind, county_list) |>
                  mutate(acres = count * 0.000988422)

#df <- as.data.frame(tbcmp_hem_filled, xy=TRUE)

#p <- ggplot(data = tbcmp_hem_filled, aes(x = x, y = y, fill = Value))+
#            geom_raster() +
#            scale_fill_discrete(hem_color) +
#            coord_equal() +
#            theme_minimal

#Download statewide DEM data and/or NOAA bathymetric data to develop a complete topo-bathy layer for the project area.

#Utilizing statewide DEM from FGDL to start (expressed in inches)
curl_download(url = "https://fgdl.org/zips/geospatial_data/archive/flidar_mosaic_in_aug20.zip",
              destfile = "./data-raw/fgdl/flidar_mosaic_in_aug20.zip")
#unzip("./data-raw/fgdl/flidar_mosaic_in_aug20.zip", exdir = "./data-raw/fgdl/")

#For now, you need to manually unzip the geodatabase and then export the GeoDB raster as a GeoTiff in ArcGIS/QGIS

fl_dem <- rast("./data-raw/fgdl/fllidar_mosaic_in_aug20.tif") |>
          terra::project(crs(bb)) |>
          terra::crop(ext(tbcmp_raster)) |>
          terra::mask(mask = bb)

fl_dem_m <- fl_dem * 0.0254   #Convert to meters

fl_dem_m_aligned <- resample(fl_dem_m, tbcmp_raster, method="bilinear")

#Download a NOAA CUDEM file for FL from here: https://coast.noaa.gov/dataviewer/#/lidar/search/ .
#Can likely use their API to get the BB, or this tool in the future: https://github.com/ciresdem/cudem

#Runs out of memory and scratch disk space (>250GB) for the original raster downloaded from above.
#Workaround is to process and export a 2m raster from ArcGIS or QGIS for input here
#noaa_cudem_m <- rast("./data-raw/noaa/NOAA_CUDEM_tbcmp_extent.tif") |>
#                terra::project(crs(bb)) |>
#                terra::crop(ext(tbcmp_raster)) |>
#                terra::mask(mask = bb)

#noaa_cudem_aligned <- resample(noaa_cudem_m, tbcmp_raster, method="bilinear")

noaa_cudem_m <- rast("./data/originals/tbcmp_hem_dem_original.tif")
noaa_cudem_aligned <- resample(noaa_cudem_m, tbcmp_raster, method="bilinear")

tbcmp_dem <- cover(noaa_cudem_aligned, fl_dem_m_aligned)

tbcmp_dem1 <- subst(tbcmp_dem, NA, -999)

writeRaster(tbcmp_dem1, filename = here('data/tbcmp_dem.tif'), overwrite = T)

tbcmp_slope <- terrain(tbcmp_dem, "slope", unit = "degrees", neighbors = 8) #Supposedly mimics what ArcGIS produces using the planar method

writeRaster(tbcmp_slope, filename = here('data/tbcmp_slope.tif'), overwrite = T)
