# analysis_configs/

YAML configuration files for the Python extraction pipelines.
All configs are consumed by `Python_scripts/summary_pipeline_landgrid.py` unless noted otherwise.

## Active configs

| File | Path | What it extracts | Pipeline script |
|:---|:---|:---|:---|
| `services_slim.yaml` | **Path B** — main | ES service rasters (8 services, per-ha) to 10km grid | `summary_pipeline_landgrid.py` |
| `beneficiaries_slim.yaml` | **Path B** | Socioeconomic rasters (Pop, GDP, HDI, Gini) to 10km grid | `summary_pipeline_landgrid.py` |
| `c_protection_synth.yaml` | **Path B** | Coastal rasters (Rt, Rt_ratio) to 10km grid — run after `rasterize_coastal.py` | `summary_pipeline_landgrid.py` |
| `services_diff_ha_groupings.yaml` | **Path A** | Pre-computed difference rasters to regional polygons (countries, biomes) | `summary_pipeline_landgrid.py` |
| `services_diff_ha.yaml` | **Path C** (validation) | Pre-computed difference rasters to 10km grid — validates Path B results | `summary_pipeline_landgrid.py` |

## Notes

- `global_ncp_base_ha.ini` — archived; see `archive/` folder
- **Path A vs Path B**: Path A extracts *difference rasters* directly to *regional polygons* (bypassing the grid) for unbiased regional totals. Path B extracts *base-year rasters* to the *10km grid* first, then computes change at the cell level. See `docs/methodology.md` for rationale.
- `services_diff_ha.yaml` and `c_protection_synth.yaml` still reference `AOOGrid_10x10km_land_4326_clean.gpkg` (the pre-enrichment grid). For new runs, update to `landgrid_1_clean_enriched_4326.gpkg`.

## Archive

| File | Why archived |
|:---|:---|
| `zonal_stats_diff.yaml` | Older Path C config using the external `zonal_stats_toolkit` and old grid. Superseded by `services_diff_ha.yaml`. |
| `global_ncp_base_ha.ini` | Path A template in INI format (likely Rich Sharp's original toolkit format). Aggregates base-year rasters to 4 regional groupings (country, WB region, income group, biome). Functionally covered by `services_diff_ha_groupings.yaml`; archived because the INI-format tool is not in the current pipeline environment. |
