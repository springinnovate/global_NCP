# Codex Context (Minimal)

## Goal

Global NCP time-series: extract hotspots of change per service; summarize + plot globally and by subregions (WB region, income group, WWF biome). Produce bar/violin plots and KS tests later.

## Invariants (don’t change)

* Paths helper: `R/paths.R` → `data_dir()`, `project_dir()`, `out_plots()`.
  * `project_dir()` now walks to repo root; `out_plots()` resolves to `/outputs/plots`.
* Checkpointing: save/read `analysis/checkpoints/` via `analysis/restore_checkpoint.R`.
* Geometry keys: `fid` (unique), `c_fid`.
* Long table columns: `fid, c_fid, service, abs_chg, pct_chg, <grouping vars>`.
* Canonical services order `svc_order`:
  `C_Risk, N_export, Sed_export, C_Risk_Red_Ratio, N_Ret_Ratio, Sed_Ret_Ratio, Pollination, Nature_Access`.
* Hotspot rules (`HOTS_CFG`):

  * `rule_mode = "vectors"`, `threshold_mode = "percent"`, `pct_cutoff = 0.05`
  * loss = `Nature_Access, Pollination, N_Ret_Ratio, Sed_Ret_Ratio, C_Risk_Red_Ratio`
  * gain = `Sed_export, N_export, C_Risk`
  * combos = `deg_combo`, `rec_combo`
  * groupings = `income_grp, region_wb, WWF_biome`
* Outputs:

  * Hotspots gpkg: `processed/hotspots/{abs|pct}/{global|<group_col>}/hotspots_*.gpkg`
  * Index CSV: `processed/hotspots/_hotspots_index.csv`
  * Plots: `outputs/plots/{abs|pct}/<group_col>/*.png` (white background)

## Entry Points

* Data Prep: `analysis/prepare_data.qmd` & `analysis/process_data.qmd`.
* Hotspots: `analysis/hotspot_extraction.qmd` (builds `plt_long`, extracts layers).
* Analysis: `analysis/hotspot_intensity.qmd`, `analysis/hotspot_multiservice.qmd`, `analysis/KS_tests_hotspots.qmd`.
* Extract hotspots: `R/extract_hotspots.R`, `R/run_one_hotset.R`.
* Bars: `make_change_bars()` (facet order fixed; optional Global bar rightmost with outline).
* Violins: `run_hotspot_violins_by()` in `R/hotspot_violins.R` (free y scales; per-service trim at 99.9%; shared helper).

## Conventions

* Always factor `service` with `svc_order` (extras appended alphabetically).
* When `include_global = TRUE`, add `facet = "Global"` **last**, distinct fill or black outline.
* Save plots with `bg = "white"`, `dpi = 300`, fixed width/height (no transparent bg).

## Next Tasks for Codex

* Ensure violins use current `HOTS_CFG` and write to the new folder layout. ✅
* Add optional save/load of `plt_long.rds` to skip recomputation.
* Add KS test runner (global + subgroup) using the already written hotspots.
* Light refactors: pure functions, fewer side effects, centralize constants.
