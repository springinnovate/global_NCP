# Analysis Runbook

This repository contains several historical Rmd/Qmd notebooks under `analysis/`, but only a few drive the current hotspot workflow. Run the following in order when regenerating results:

1.  **Data Processing** – `analysis/process_data.qmd`
    *   Consolidates the base zonal statistics outputs.
    *   Calculates bi-temporal change (absolute and symmetric percentage).
    *   Produces the canonical `processed/10k_change_calc.gpkg` file used by all downstream steps.

2.  **Hotspot Extraction & Plots** – `analysis/hotspot_extraction.qmd`
    *   Reads `10k_change_calc.gpkg` and identifies hotspots based on the rules in `HOTS_CFG`.
    *   Exports hotspot vector layers to `processed/hotspots/`.
    *   Generates summary plots (e.g., bar plots, boxplots) and saves them to `outputs/plots/`.

3.  **Spatial Clustering & Synthesis** – `analysis/hotspot_synthesis.qmd`
    *   Calculates hotspot intensity (coverage), relative intensity, and multi-service "hotness".
    *   Exports summary tables (`hotspot_area_stats.csv`, etc.) and clustering plots.

4.  **Socioeconomic & Driver Analysis** – `analysis/KS_tests_hotspots.qmd`, `make_attribution_map.R`, etc.
    *   These notebooks consume the hotspot outputs to produce statistical summaries, attribution maps, and other final figures.

> Historical notebooks such as `ch_analysis.Rmd`, `data_prep.Rmd`, etc., are kept for reference but are not part of the reproducible pipeline.