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
| `extract_hotspots_5service.R` | 5-service hotspot redesign (Nature_Access, Pollination, N_export, Sed_export, C_Risk) + water/access/combined_cross overlap columns, global scope, pct+abs. Writes `data/processed/hotspots_5service/{pct,abs}/global/`. | After any change to the 5-service definition or threshold |
| `gdal_rasterize_hotspots_5service.R` | Rasterizes the 5-service overlap columns (`count_water`, `count_access`, `combined_cross`) for Rich's beneficiary pipeline, same convention as `gdal_rasterize_hotspots.sh`. | After `extract_hotspots_5service.R` output changes |
| `build_lc_grid_fid_crosswalk.R` | Crosswalk between `10k_lcc_granular_metrics.gpkg`'s own `grid_fid` and the master grid's `fid` (two different exports of the same tessellation). Root-cause fix for the 2026-07-08 grid-id mismatch that inverted the attribution-gap finding. | Only if either grid export is regenerated |
| `compute_attribution_true_union.R` | True cell-level union overlap between ES hotspots and LCC driver hotspots across all 5 drivers — replaces the stale 2-driver `lcc_es_hotspot_overlap.csv`. | After any hotspot or LCC-driver re-extraction |

## mapping/ — Map generation scripts

All scripts produce maps saved to `outputs/plots/maps/` or `outputs/maps/`.

| Script | What it generates |
|:---|:---|
| `make_attribution_map.R` | Attribution gap maps: ES hotspots vs LCC driver hotspots |
| `make_faceted_maps.R` | Faceted global maps by region/income/biome with semantic colour scale |
| `make_hotspot_count_map.R` | Global hotspot frequency heatmap ("hotness" map) |
| `make_lcc_driver_map.R` | LCC driver hotspot maps with policy-relevant transition categories |
| `make_lcc_overview_map.R` | Global net LCC change overview (Forest / Cropland / Grassland) |
| `make_lcc_true_overlap_map.R` | Corrected caption/companion to the LCC driver map — only cells where ES hotspots and LCC driver hotspots actually spatially intersect (the driver map alone shows the raw driver-hotspot union, unconditioned on ES status) |
| `make_socieconomic_maps.R` | 2×2 faceted socioeconomic context maps (Pop, HDI, GDP, GINI) |
| `change_bars_pixel.R` | Pixel-level change bar plots and zonal stats consolidation |
| `make_5service_overlap_maps.R` | 5-service redesign: water / access / combined cross-category overlap maps (Becky's 2026-07-21 instructions) |
| `make_5service_overlap_summary.R` | Summary stats + faceted bar chart companion to `make_5service_overlap_maps.R`, same tier definitions |
| `make_native_change_figure.R` | Native-10km (no dissolve) paired export/retention change figure, replacing the biome-dissolved change map for the 5-service redesign |
| `make_paper_supplement_maps.py` | Per-service change+hotspot maps supplement (Becky-requested, 2026-07-21 paper comment) — Python, not R, because of an R/GDAL crash on this machine for these particular rasters |
| `make_global_thumbnail_maps.R` | Global-extent thumbnail versions of the LAC maps below, for the IDB-WWF workshop deck intro slide |
| `make_lac_critical_assets_map.R` | LAC "critical natural assets" map (IDB-WWF workshop deck), from Chaplin-Kramer et al. 2022 |
| `make_lac_hotspot_map.R` | LAC compound hotspot map (IDB-WWF workshop deck) |

## archive/ — One-off scripts (results already produced)

| Script | Purpose |
|:---|:---|
| `enrich_grid_attributes.R` | Superseded by `Python_scripts/enrich_grid.py` which does the same nev_name→country join (lines 139–142). Kept for reference only. |
| `gdal_rasterization.sh` | Template GDAL rasterization workflow (inactive reference) |
| `plot_hex.R` | Exploratory hexbin scatterplot — not part of final outputs |
