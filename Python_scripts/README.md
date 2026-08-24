# Python Scripts

Python pipeline and utility scripts for the Global NCP Hotspot Analysis.

## Active Pipeline

Run in this order when processing new data (e.g. adding a new year):

| Script | What it does |
|:---|:---|
| `build_master_grid.py` | Builds the canonical 10km IUCN AOO equal-area grid from source data |
| `enrich_grid.py` | Joins regional attributes to the grid (country, WB region, income group, WWF biome) |
| `coastal_protection_join.py` | Joins 1992 and 2020 coastal protection point files → `c_protection_1992_2020_joined.gpkg` |
| `rasterize_coastal.py` | Converts coastal protection shore points to 300m raster (spatial mean per pixel) |
| `summary_pipeline_landgrid.py` | **Main extraction pipeline** — runs `exactextract` zonal statistics for all ES and socioeconomic rasters against the 10km grid. Config: `analysis_configs/services_slim.yaml` |
| `calculate_bitemporal_change.py` | Computes absolute difference and Symmetric Percentage Change (SPC) for all service pairs (1992 vs 2020) |
| `calculate_ratios.py` | Computes ratio variables from raw extracted columns |
| `extraction_script.py` | Extracts population exposure for downstream/travel-time beneficiary masks by hotspot overlap category |

## Supporting Tools

| Script | What it does |
|:---|:---|
| `batch_raster_diff.py` | Batch raster differencing across all service rasters (used for Path C validation) |
| `reproject_vector.py` | Reprojects a vector file to a target CRS |
| `msk_zeros_diff.py` | Masks zero-value pixels in difference rasters — used for Pollination to exclude pixels outside the valid agricultural mask |

## Validation and Diagnostics

| Script | What it does |
|:---|:---|
| `verify_grid_area.py` | Confirms all grid cells have exactly 100 km² area after reprojection |
| `diagnose_grid.py` | Checks for invalid/problematic geometries in the master grid |
| `list_gpkg_columns.py` | Prints column names of a GeoPackage without loading the full file into memory — useful for inspecting large GPKGs quickly |
| `extract_book_data_fills.py` | Reads analysis outputs (CSVs, GPKGs) and extracts key numbers (hotspot counts, population exposure, KS results) for filling in book/paper placeholders |

## Archive

`archive/` contains scripts that are no longer active:

| Script | Reason archived |
|:---|:---|
| `summary_pipeline_rasterzones.py` | Deprecated — rasterized extraction approach abandoned after geometry issues; `summary_pipeline_landgrid.py` (vector approach) is the active pipeline |
| `summarize_cp_points_grid.py` | Non-functional — early attempt to spatially join coastal points to the grid; superseded by `rasterize_coastal.py` |
| `temp_nature_access_diff.py` | One-off — hardcoded remote paths; superseded by `batch_raster_diff.py` |
| `check_valid_geometries.py` | One-off geometry validation run during grid preparation |

## Coastal Protection Pipeline Note

The coastal protection data requires extra pre-processing because InVEST outputs are shore points, not rasters:

```
coastal_protection_join.py   →  rasterize_coastal.py   →  summary_pipeline_landgrid.py
(join 1992+2020 points)          (points → 300m raster)      (extract to 10km grid)
```

If adding new years, re-run all three steps.
