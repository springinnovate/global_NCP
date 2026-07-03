# scripts/ — Standalone Production Scripts

Scripts run independently (not sourced from analysis notebooks) to generate specific outputs.
All scripts load `R/paths.R` for path management and write to `outputs/`.

## Validation

| Script | Purpose | When to re-run |
|:---|:---|:---|
| `audit_claims.R` | Validates key paper claims against actual data: income group disparity, regional disparity, attribution gap %, biome and country rankings. Writes to `outputs/audit_summary.txt`. | After any hotspot re-extraction or threshold change — acts as a regression test for core numbers. |

## Reference data

| Script | Purpose | When to re-run |
|:---|:---|:---|
| `export_reclass_table.R` | Source definition of the LCC reclassification: ESA CCI classes → 9 functional classes → binary Natural/Transformed. Exports `outputs/tables/lcc_reclassification_table.csv`. | Only if the classification scheme changes. The CSV is already tracked in git. |

## Output-generating scripts (run to regenerate outputs)

| Script | What it generates | When to re-run |
|:---|:---|:---|
| `generate_regional_subsets.R` | Per-group CSV subsets under `outputs/tables/regional_subsets/` | After any hotspot threshold change |
| `get_regional_pop.R` | Regional population statistics from 10k_change_calc.gpkg | After population data update |
| `make_hotness_barplots.R` | Compound risk bar plots by income group, WB region, biome | After hotspot re-extraction |
| `make_country_intensity_bars.R` | Country-level hotspot intensity bar charts (top N) | After hotspot re-extraction |
| `gdal_rasterize_hotspots.sh` | Hotspot GeoTIFFs (hotspot_count + 8 service flags, 10km) | After hotspot re-extraction |

## mapping/ — Map generation scripts

All scripts produce maps saved to `outputs/plots/maps/` or `outputs/maps/`.

| Script | What it generates |
|:---|:---|
| `make_attribution_map.R` | Attribution gap maps: ES hotspots vs LCC driver hotspots |
| `make_faceted_maps.R` | Faceted global maps by region/income/biome with semantic colour scale |
| `make_hotspot_count_map.R` | Global hotspot frequency heatmap ("hotness" map) |
| `make_lcc_driver_map.R` | LCC driver hotspot maps with policy-relevant transition categories |
| `make_lcc_overview_map.R` | Global net LCC change overview (Forest / Cropland / Grassland) |
| `make_socieconomic_maps.R` | 2×2 faceted socioeconomic context maps (Pop, HDI, GDP, GINI) |
| `change_bars_pixel.R` | Pixel-level change bar plots and zonal stats consolidation |

## archive/ — One-off scripts (results already produced)

| Script | Purpose |
|:---|:---|
| `audit_claims.R` | One-time audit of income group disparity claims → `outputs/audit_summary.txt` |
| `enrich_grid_attributes.R` | Superseded by `Python_scripts/enrich_grid.py` which does the same nev_name→country join (lines 139–142). Kept for reference only. |
| `export_reclass_table.R` | One-time: exported LCC reclassification table as data frame |
| `gdal_rasterization.sh` | Template GDAL rasterization workflow (inactive reference) |
| `plot_hex.R` | Exploratory hexbin scatterplot — not part of final outputs |
