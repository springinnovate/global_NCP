# Analysis Runbook

This document outlines the steps for executing the R/Quarto analysis pipeline. While the `analysis/` directory may contain historical notebooks, only the following are part of the current, reproducible workflow.

## Execution Order

1.  **Optional Checkpoint Restore** – `analysis/restore_checkpoint.R`
    *   **Purpose:** Loads `HOTS_CFG`, `plt_long`, and `grid_sf` from the latest saved checkpoint under `data/processed/intermediate`. This allows you to skip rebuilding `plt_long` if upstream data processing hasn't changed.

2.  **Hotspot Extraction + Plots** – `analysis/hotspot_extraction.qmd`
    *   **Purpose:** Builds `plt_long` (if not restored), validates `HOTS_CFG`, and runs the hotspot export chunk (writes `processed/hotspots/...` and `_hotspots_index.csv`).
    *   **Outputs:** Generates trimmed-change bar plots and hotspot violins via functions in `R/`. Writes figures to `outputs/plots/{abs|pct}/<group_col>/...`.

3.  **Spatial Clustering & Overlap** – `analysis/hotspot_synthesis.qmd`
    *   **Purpose:** Consumes `plt_long` and canonical geometries to calculate hotspot intensity (coverage), relative intensity, and multi-service "hotness."
    *   **Outputs:** Exports summary tables and clustering plots.

4.  **Checkpoint Save** – `analysis/save_checkpoint.R` (optional)
    *   **Purpose:** Saves updated `plt_long`, `grid_sf`, `HOTS_CFG`, and `hot_index` back to `data/processed/intermediate` for future sessions.

5.  **KS Tests / Follow-up Notebooks** – `analysis/KS_tests_hotspots.qmd` (and others as they come online).
    *   **Purpose:** These consume the hotspot outputs and produce statistical summaries or additional figures.

## Historical Notebooks

Historical notebooks such as `ch_analysis.Rmd`, `data_prep.Rmd`, etc., are kept for reference but are not part of the reproducible pipeline. They should be left untouched unless you intentionally need to access legacy code.
