# ARCGIS TO R CONVERSION SUMMARY
## Habitat Evolution Model for Tampa Bay Coastal Management Program

---

## Conversion Overview

**Date**: February 4, 2026  
**Source**: ArcGIS ModelBuilder Python exports  
**Target**: R scripts using terra and sf packages  
**Status**: ✅ Complete

---

## Files Converted

### Main Workflow
- ✅ **HabitatEvolutionModelv202512.py** → **HabitatEvolutionModel_v202512.R**
  - Main orchestration script
  - Iterates through SLR scenarios
  - Calls all sub-models

### Supporting Functions

1. ✅ **DatumAdjustmentv202512.py** → **DatumAdjustment_v202512.R**
   - Adjusts tidal datums for SLR
   - Reads .dbf tables
   - Returns datum values

2. ✅ **MarshAccretionv202512.py** → **MarshAccretion_v202512.R**
   - Calculates vertical marsh accretion
   - Two methods: constant vs. elevation-dependent
   - Adjusts DEM based on habitat type

3. ✅ **HabitatAdjustmentv202512.py** → **HabitatAdjustment_v202512.R** (3 parts)
   - Core habitat reclassification
   - 23 habitat types
   - Elevation-based transitions
   - Land protection policies

4. ✅ **Ocean2Beachv202512.py** → **Ocean2Beach_v202512.R**
   - Creates ocean proximity mask
   - 500m buffer around water
   - Enables beach conversion

5. ✅ **ProcessFreshwaterv202512.py** → **ProcessFreshwater_v202512.R**
   - Freshwater influence mask
   - NHD polygon rasterization
   - Saltwater/freshwater differentiation

---

## Key Conversion Decisions

### Package Choices

| Purpose | ArcGIS | R Package | Rationale |
|---------|--------|-----------|-----------|
| Raster operations | arcpy.sa | **terra** | Modern, fast, memory-efficient |
| Vector operations | arcpy.analysis | **sf** | Standard, GDAL-based |
| Raster algebra | Con() | **ifel()** | Terra's conditional function |
| Table reading | arcpy | **foreign** | Reads .dbf files |

### Architectural Changes

1. **No arcgisbinding**: Per user requirement, pure open-source R
2. **Split large functions**: HabitatAdjustment split into 3 parts for clarity
3. **Explicit intermediate storage**: R doesn't have ArcGIS's automatic temp management
4. **Function sourcing**: Each script can be used independently

### Geodatabase Handling

**Challenge**: ArcGIS File Geodatabases are proprietary  
**Solution**: 
- `sf::st_read()` can read feature classes directly
- For rasters, recommend exporting to GeoTIFF first
- Alternative: `terra::rast("path.gdb/rastername")`

---

## Function Mapping

### Raster Operations

| ArcGIS | R (terra) | Notes |
|--------|-----------|-------|
| `Con(condition, true, false)` | `ifel(condition, true, false)` | Conditional raster |
| `raster == value` | `raster == value` | Direct comparison |
| `raster + value` | `raster + value` | Arithmetic |
| `ExtractByMask()` | `mask()` or `ifel()` | Masking |
| `RasterCalculator` | Direct R expressions | No wrapper needed |
| `Mosaic()` | `merge()` or `mosaic()` | Combine rasters |

### Vector Operations

| ArcGIS | R (sf) | Notes |
|--------|--------|-------|
| `Buffer()` | `st_buffer()` | Buffer polygons |
| `Select()` | `filter()` or `[subset]` | Attribute selection |
| `RasterToPolygon()` | `as.polygons()` (terra) | Convert raster |
| `PolygonToRaster()` | `rasterize()` (terra) | Convert vector |

### Table Operations

| ArcGIS | R | Notes |
|--------|---|-------|
| `AddField()` | `$new_col <- value` | Add column |
| `TableSelect()` | `filter()` (dplyr) | Select rows |
| `GetFieldValue()` | `data$column[row]` | Extract value |
| `CalculateField()` | `mutate()` or `$col <-` | Calculate |

---

## Parameter Equivalents

### Input/Output

| Python | R | Example |
|--------|---|---------|
| `"path\\to\\file.gdb\\layer"` | `"path/to/file.gdb/layer"` | Forward slashes |
| `r"raw\string"` | `"path/to/file"` | No special syntax |
| `arcpy.Raster(path)` | `rast(path)` | Load raster |
| `.save(output)` | `writeRaster()` | Save raster |

### Data Types

| Python | R | Notes |
|--------|---|-------|
| `True/False` | `TRUE/FALSE` | Capitalized |
| `None` | `NULL` | Null value |
| `float()` | `as.numeric()` | Type conversion |
| `-999` (NoData) | `NA` | Missing data |

---

## Notable Differences

### 1. NoData Handling

**ArcGIS**:
```python
Con((habitat == 1200) & (topo != -999), 1200, 0)
```

**R**:
```r
ifel((habitat == 1200) & (topo != -999) & !is.na(topo), 1200, 0)
```

### 2. Nested Conditions

**ArcGIS**:
```python
Con(cond1, val1, Con(cond2, val2, Con(cond3, val3, default)))
```

**R**:
```r
ifel(cond1, val1, 
  ifel(cond2, val2, 
    ifel(cond3, val3, default)))
```

### 3. Raster Algebra

**ArcGIS**: Implicit broadcasting
```python
new_raster = old_raster + 0.5
```

**R**: Same behavior
```r
new_raster <- old_raster + 0.5
```

### 4. Memory Management

**ArcGIS**: Automatic temp file cleanup  
**R**: Manual cleanup recommended
```r
terra::tmpFiles(remove = TRUE)
gc()
```

---

## Testing Recommendations

### Unit Tests

1. **DatumAdjustment**: 
   - Test with known SLR values
   - Verify all 7 datums returned

2. **MarshAccretion**:
   - Test constant vs. elevation-dependent methods
   - Verify accretion calculations

3. **HabitatAdjustment**:
   - Test each habitat transition rule
   - Verify protection policies

### Integration Tests

1. Run full model with small test area
2. Compare outputs to ArcGIS version (if available)
3. Check for NAs in unexpected places
4. Verify habitat code ranges (should only be valid codes)

### Performance Tests

1. Memory usage with large rasters
2. Processing time per year
3. Temp file accumulation

---

## Known Limitations

### Current Implementation

1. **Single-threaded**: No parallel processing (can be added)
2. **Memory-intensive**: Loads full rasters (terra is efficient, but large areas may need tiling)
3. **No progress bars**: Can be added with progress package
4. **Limited error handling**: Basic checks only

### Geodatabase Support

- **Feature classes**: ✅ Works with sf::st_read()
- **Rasters**: ⚠️ May need export to GeoTIFF
- **Tables**: ⚠️ Export to .dbf or .csv first

### Platform Differences

- **Windows paths**: Use forward slashes or escape backslashes
- **Case sensitivity**: R is case-sensitive (file names, variable names)
- **File locking**: Windows may lock files differently than ArcGIS

---

## Migration Checklist

### Before Running

- [ ] Install required R packages (terra, sf, foreign, dplyr)
- [ ] Verify GDAL installation
- [ ] Export geodatabase rasters to GeoTIFF (recommended)
- [ ] Check file paths (forward slashes)
- [ ] Verify habitat codes in your data
- [ ] Review datum table format
- [ ] Check SLR table format

### First Run

- [ ] Start with small test area
- [ ] Verify input data loads correctly
- [ ] Check intermediate outputs
- [ ] Monitor memory usage
- [ ] Compare one year to ArcGIS output (if available)

### Production

- [ ] Set appropriate output directory
- [ ] Configure memory limits if needed
- [ ] Plan for temp file cleanup
- [ ] Document custom parameters
- [ ] Archive outputs

---

## Support Resources

### Documentation Provided

1. **README.md**: Comprehensive user guide
2. **example_workflow.R**: Complete working example
3. **habitat_codes_reference.R**: Habitat classification reference
4. This conversion summary

### External Resources

- terra package: https://rspatial.github.io/terra/
- sf package: https://r-spatial.github.io/sf/
- R Spatial: https://www.r-spatial.org/

---

## Future Enhancements

### Potential Improvements

1. **Parallel processing**: Process multiple years simultaneously
2. **Progress reporting**: Add progress bars
3. **Validation tools**: Automated QA/QC checks
4. **Visualization**: Built-in plotting functions
5. **Tiling support**: Process large areas in chunks
6. **Web interface**: Shiny app for parameter selection
7. **Batch processing**: Process multiple scenarios
8. **Cloud support**: AWS/Google Cloud integration

### Code Optimization

1. Pre-allocate rasters where possible
2. Use terra's focal operations for neighborhood analysis
3. Implement caching for repeated calculations
4. Add parallel::mclapply for multi-core processing

---

## Acknowledgments

**Original Model**: Tampa Bay Coastal Master Plan (TBCMP)  
**ArcGIS Development**: ESA & TBEP Team  
**R Conversion**: February 2026  

---

## Version History

**v202512** (February 2026)
- Initial conversion from ArcGIS Python
- Full workflow implementation
- Documentation and examples

---

## License

[Specify appropriate license]

---

**End of Conversion Summary**
