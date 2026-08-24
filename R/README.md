# R/ — Analytical Library

Reusable functions loaded via `devtools::load_all()` in all analysis notebooks. Do not rename or move files without updating any explicit `source()` calls in `analysis/*.qmd` and `docs/manuscript/chapters/*.qmd`.

## Core utilities

| File | Purpose |
|:---|:---|
| `paths.R` | **Critical.** Defines all project, data, and output paths via `data_dir()`, `output_dir()`, etc. Every notebook sources this. Reads `GLOBAL_NCP_DATA` environment variable. |
| `pct_change.R` | The SPC math primitive: `calc_symmetric_pct_change(v1, v2)`. One function, the formula only. |
| `pct_change_calc.R` | Pipeline wrapper: `compute_change()` detects `variable_YYYY_mean` column naming patterns and calls `calc_symmetric_pct_change()` internally. Use this in notebooks; `pct_change.R` is its dependency. |
| `normalizR.R` | Simple 0–1 raster normalization utility. |

## Hotspot detection and profiling

| File | Purpose |
|:---|:---|
| `get_hotspots.R` | Core hotspot extraction: `extract_hotspots()`, `extract_hotspots_by()`, `filter_multidim()`. Defines the top-5% rank-based threshold logic. |
| `utils_hotspot.R` | Canonical service order (`svc_order`), `vmsg()` logging, `agg_by_group()` aggregation helper, and other shared hotspot utilities. |
| `KS_helpers.R` | KS test heatmap, bar, and mountain plot helpers. |
| `ks_hotspots.R` | Main KS two-sample test with FDR correction, Cliff's Delta, and directional summaries. |

## Visualisation

| File | Purpose |
|:---|:---|
| `plotting_functions.R` | Biome label mappings, relative intensity plots, regional hotspot concentration charts. Sourced explicitly in `docs/manuscript/chapters/07-regional-profiles.qmd`. |
| `hotspot_violins.R` | Boxplot/violin plots for hotspot SPC distributions by grouping variable. |
| `ecdf_grid.R` | ECDF overlay grids for comparing hotspot vs background distributions. |
| `save_ECDF.R` | Batch-renders and exports ECDF grids with auto log-transform selection. |

## Land cover change

| File | Purpose |
|:---|:---|
| `utils_lcc_metrics.R` | Zonal LCC metric extraction from raster pairs; uses `diffeR` for contingency tables. Supports parallel processing. |
| `iterate_lcc_metrics.R` | Iterates `utils_lcc_metrics.R` across year-step pairs for multi-temporal LCC analysis. |

## Percentile utilities

| File | Purpose |
|:---|:---|
| `percentileR.R` | *(archived in `R/archive/`)* Early percentile-based hotspot definition, superseded by `get_hotspots.R`. |

## Note on pct_change.R vs pct_change_calc.R

These are **not duplicates** — they are two levels of the same calculation:

```
pct_change.R          →  calc_symmetric_pct_change(v1, v2)   # raw formula
pct_change_calc.R     →  compute_change(df, ...)              # column-detection wrapper
                                calls calc_symmetric_pct_change() internally
```

Use `compute_change()` in analysis notebooks. `pct_change.R` is a dependency, not a standalone tool.
