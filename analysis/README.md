# Analysis Entry Points

This repository still contains several historical Rmd/Qmd notebooks under `analysis/`, but only a few drive the current hotspot workflow. Run the following in order when regenerating results:

1. **Optional checkpoint restore** – `analysis/restore_checkpoint.R`  
   Loads `HOTS_CFG`, `plt_long`, and `grid_sf` from the latest saved checkpoint under `data/processed/intermediate`. Use this to skip rebuilding `plt_long` when nothing upstream changed.

2. **Hotspot extraction + plots** – `analysis/hotspot_extraction.qmd`  
   - Builds `plt_long` (if not restored), validates `HOTS_CFG`, and runs the hotspot export chunk (writes `processed/hotspots/...` and `_hotspots_index.csv`).  
   - Generates trimmed-change bar plots and hotspot violins via functions in `R/`.  
   - Writes figures to `outputs/plots/{abs|pct}/<group_col>/...`.

3. **Checkpoint save** – `analysis/save_checkpoint.R` (optional)  
   Saves updated `plt_long`, `grid_sf`, `HOTS_CFG`, and `hot_index` back to `data/processed/intermediate` for future sessions.

4. **KS tests / follow-up notebooks** – `analysis/KS_tests_hotspots.qmd` (and others as they come online).  
   These consume the hotspot outputs and produce statistical summaries or additional figures.

> Historical notebooks such as `ch_analysis.Rmd`, `data_prep.Rmd`, etc., are kept for reference but are not part of the reproducible pipeline. Leave them untouched unless you intentionally need legacy code.
