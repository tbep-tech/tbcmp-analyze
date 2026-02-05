# ArcGIS to R Conversion: Habitat Evolution Model
## Tampa Bay Coastal Management Program (TBCMP)

---

## Overview
This repository contains GIS base files and analyses used to understand habitat vulnerability and restoration potential within the seven (7) county project area of the Tampa Bay Coastal Master Plan project.

Additionally, the repository contains R code converted from ArcGIS Python/ModelBuilder scripts for modeling coastal habitat changes under sea level rise scenarios. The model (based on the SLAMM) predicts how different habitat types will transition as sea levels rise, accounting for differing marsh accretion rates, tidal datum adjustments and SLR scenarios, and softly developed land use protection policies.

---

## Components

### Main Scripts
1. **data_prep.R**
   - Assembles base GIS files from known sources to conduct subsequent analyses
   - Saves the created raster files as local tif files that can be referenced later
   - Truncates all files to a bounding box of the 7 county area (inclusive of jurisdictional coastal waters)  

2. **HabitatEvolutionModel_v202512.R**
   - Main orchestration script
   - Iterates through SLR scenarios by year
   - Calls all sub-models in sequence

3. **DatumAdjustment_v202512.R**
   - Adjusts tidal datums (HAT, MHHW, MTL, etc.) for sea level rise
   - Reads datum and SLR scenario tables

4. **MarshAccretion_v202512.R**
   - Calculates vertical marsh accretion
   - Two methods: constant accretion vs. elevation-dependent (Marsh98)
   - Adjusts topography based on habitat-specific accretion rates

5. **HabitatAdjustment_v202512.R** (+ parts 2 & 3)
   - Core habitat reclassification logic
   - Uses elevation thresholds and tidal datums
   - Applies land protection policies
   - Combines 23 different habitat types

6. **Ocean2Beach_v202512.R**
   - Identifies ocean-proximate areas for beach conversion
   - Creates 500m buffer around ocean/water bodies

7. **ProcessFreshwater_v202512.R**
   - Creates freshwater influence mask
   - Uses NHD waterbody polygons
   - Differentiates freshwater vs. saltwater habitats

---

## Requirements

### R Packages

```r
install.packages(c(
  "here",		# Working directory name helper
  "curl",		# Download utility to acquire known base files
  "tbeptools",	# Marcus likes this referenced
  "tidycensus", # Access US Census spatial files
  "tidyverse",  # Data manipulation and piping
  "terra",      # Raster operations
  "sf",         # Vector operations
  "foreign",    # Read .dbf files
  "dplyr"       # Data manipulation
))
```

### System Requirements

- R version 4.0 or higher
- Minimum 8 GB RAM (16+ GB recommended for large study areas)
- GDAL installed (usually comes with `terra` package)

---

## Input Data Structure

### Required Inputs

1. **SLR Scenario Table** (.csv)
   - Must contain `Year` column
   - SLR values in meters (column name: SLR, SLR_m, or similar)
   
2. **Tidal Datums Table** (.csv)
   - Columns: `Datum`, `DatumEleva` (or `Elevation`)
   - Datum types: HAT, MHHW, MHW, MTL, MLHW, MLW, MLLW

3. **Vegetation/Habitat Raster**
   - Integer raster with habitat codes (see Habitat Codes table)
   - 2m resolution applied in the TBCMP project area

4. **Topography/DEM Raster**
   - Elevation in meters (NAVD88)
   - Same resolution and extent as habitat raster
   - NoData value set to: -999

5. **Freshwater Polygon** (optional but recommended)
   - FL NHD major river layer (or similar) applied in this study 
   - Can be a geodatabase feature class, but exported shapefile used here

---

## Usage

### Basic Usage

```r
# Source the main script
source("HabitatEvolutionModel_v202512.R")

# Run with default parameters
HabitatEvolutionModel_v202512()
```

### Custom Parameters

```r
HabitatEvolutionModel_v202512(
  SLR_Table = "path/to/SLR_scenarios.dbf",
  Datums_Table = "path/to/datums.dbf",
  Protect_Developed = TRUE,  # Protect developed areas from conversion
  veg = "path/to/habitat_raster.tif",
  topo = "path/to/dem.tif",
  Juncus_Marsh_Accretion_mm_yr = 3.75,
  Salt_Marsh_Accretion_mm_yr = 1.6,
  Mangrove_Accretion_mm_yr = 1.6,
  Topo_Year = 2025,
  output_template = "outputs/habitat_{OutY}.tif"
)
```

### Running Individual Components

```r
# Example: Calculate marsh accretion only
source("MarshAccretion_v202512.R")

habitat <- rast("habitat.tif")
topo <- rast("dem.tif")

new_topo <- MarshAccretion_v202512(
  habitat = habitat,
  topo = topo,
  Juncus_Marsh_Accretion_mm_yr = 3.75,
  Salt_Marsh_Accretion_mm_yr = 1.6,
  Mangrove_Accretion_mm_yr = 1.6,
  Topo_Year = 2025,
  OutY = 2050
)
```

---

## Habitat Classification Logic

### Elevation-Based Habitat Zones

The model uses tidal datums to define habitat zones:

```
HAT  ═══════════════════════════  Upland / Salt Barren boundary
MHHW ───────────────────────────  Salt Barren / High Marsh boundary
MHW  ───────────────────────────  High Marsh / Low Marsh boundary
MTL  ───────────────────────────  Low Marsh transition zone
MLHW ───────────────────────────  Tidal Flat upper limit
MLW  ───────────────────────────  Mangrove lower limit
MLLW ═══════════════════════════  Subtidal / Seagrass boundary
      -1.5m below MLLW           Deep subtidal
```

### Freshwater vs. Saltwater Differentiation

- **Freshwater = 1**: Within NHD waterbody polygons
  - Enables Juncus marsh, tidal flats
  - Prevents mangroves

- **Freshwater = 0**: Outside waterbody polygons
  - Enables mangroves
  - Prevents Juncus marsh in certain zones

### Land Protection Policy

When `Protect_Developed = TRUE`:
- All developed land (1100, 1200, 1800, 1820, 2100, 2200, 2400, 2550) remains
- Development prevents habitat migration

When `Protect_Developed = FALSE`:
- Only development above HAT + SLR is protected
- Inundated areas can convert to wetlands/water

---

## Habitat Codes Reference

### Developed (1000s)
- **1100**: Developed, Pervious
- **1200**: Developed, Hard Surface
- **1800**: Developed, Low Density
- **1820**: Golf Course

### Agricultural (2000s)
- **2100**: Agriculture
- **2200**: Tree Crops
- **2400**: Vineyard
- **2550**: Aquaculture

### Upland Natural (1900, 3000s, 4000s)
- **1900**: Upland Undeveloped
- **3200**: Shrub/Scrub
- **4100**: Forest
- **4400**: Tree Plantation

### Open Water (5000s)
- **5200**: Estuarine Open Water
- **5400**: Subtidal (deep water)

### Wetlands (6000s)
- **6110**: Freshwater Swamp
- **6120**: Mangrove
- **6410**: Freshwater Marsh
- **6420**: Salt Marsh (High Marsh)
- **6425**: Juncus Marsh (Low Marsh)
- **6510**: Tidal Flat
- **6600**: Salt Barren

### Coastal (7000s, 9000s)
- **7100**: Beach
- **9113**: Seagrass (Submerged Aquatic Vegetation)

---

## Output Structure

### Output Files

For each year in the SLR table:
```
outputs/
  ├── habitat_2030.tif
  ├── habitat_2040.tif
  ├── habitat_2050.tif
  └── ...
```

Each output raster contains habitat codes indicating predicted habitat type for that year.

### Intermediate Products

The model may create intermediate files:
- `marshac1_{year}.tif`: Accreted topography
- `oceancheck.tif`: Ocean proximity mask
- `freshwater2.tif`: Freshwater influence mask

---

## Differences from ArcGIS Version

### Functional Equivalents

| ArcGIS | R (terra) |
|--------|-----------|
| `Con()` | `ifel()` |
| `arcpy.sa.ExtractByMask()` | `mask()` or conditional `ifel()` |
| `arcpy.conversion.RasterToPolygon()` | `as.polygons()` |
| `arcpy.conversion.PolygonToRaster()` | `rasterize()` |
| `arcpy.analysis.Buffer()` | `st_buffer()` (sf) |
| `arcpy.management.MosaicToNewRaster()` | `merge()` or `mosaic()` |
| `arcpy.ListFields()` | `names()` |
| `read.dbf()` | `foreign::read.dbf()` |

### Key Behavioral Differences

1. **NoData Handling**: 
   - ArcGIS uses -999 or system NoData
   - R/terra uses `NA`
   - Code checks for both: `(topo != -999)` and `!is.na(topo)`

2. **Raster Algebra**:
   - ArcGIS: Implicit broadcasting
   - R/terra: Explicit vectorization with `ifel()`

3. **Vector Operations**:
   - ArcGIS: Built into `arcpy.analysis`
   - R: Requires `sf` package for vector operations

4. **File Geodatabases**:
   - ArcGIS: Native support
   - R: `sf::st_read()` can read feature classes
   - Rasters: Export to GeoTIFF first, or use `terra::rast()` with layer name

---

## Troubleshooting

### Common Issues

**Issue**: "Cannot open file" error
- **Solution**: Check file paths (use forward slashes or escaped backslashes)
- For geodatabase: `st_read("path/to/file.gdb", layer = "layer_name")`

**Issue**: Memory errors
- **Solution**: Process in chunks or use `terra::tmpFiles()` to manage temp files
- Increase R memory: `memory.limit(size = 16000)` (Windows)

**Issue**: Habitat codes don't match
- **Solution**: Check your habitat raster codes against the reference table
- Use `freq(habitat)` to see all unique values

**Issue**: Datum values returning NA
- **Solution**: Check spelling of datum names in table (case-sensitive)
- Verify datum column name matches expected values

---

## Performance Optimization

### Tips for Large Study Areas

1. **Use GeoTIFF with compression**:
```r
writeRaster(raster, "output.tif", 
            gdal = c("COMPRESS=LZW", "PREDICTOR=2"))
```

2. **Process in tiles** for very large areas:
```r
# Split into tiles, process separately, then merge
```

3. **Parallel processing** (for multiple years):
```r
library(parallel)
# Not implemented in base code, but can be added
```

4. **Monitor memory**:
```r
terra::tmpFiles(orphan = TRUE, remove = TRUE)  # Clean temp files
gc()  # Garbage collection
```

---

## Citation

If using this code, please cite:

```
Tampa Bay Coastal Management Program (TBCMP)
Habitat Evolution Model v202512
Converted from ArcGIS to R, February 2026
```

---

## Support

For questions or issues:
1. Check this README
2. Review habitat codes table
3. Verify input data formats
4. Check R package versions

---

## License

[Specify license here]

---

## Acknowledgments

- Original ArcGIS ModelBuilder: TBCMP Team
- Conversion to R: [Your info]
- Sea Level Rise Scenarios: [Data source]
- Tidal Datum Data: [Data source]
