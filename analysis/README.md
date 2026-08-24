# Analysis Directory

::: callout-note
**Directory Scope & Archival Policy**

This directory is for **active** narrative analysis and synthesis notebooks (`.qmd`).

*   Core R functions belong in `/R/`.
*   Standalone utility scripts (e.g., for map generation) belong in `/scripts/`.
*   **Legacy or completed notebooks** from previous analysis phases are moved to the root `/notebooks/` directory to keep this folder focused.
:::

This directory contains the primary Quarto notebooks for the data processing, synthesis, and interpretation stages of the Global NCP hotspot analysis.

## Core Notebooks

The notebooks should be run in the following order to ensure data dependencies are met. For more detail, see `docs/runbook.md`.

1.  **`process_data.qmd`**: Consolidates base zonal statistics, calculates bi-temporal change (absolute and SPC), and produces the canonical `processed/10k_change_calc.gpkg` file used by all downstream steps.

2.  **`hotspot_extraction.qmd`**: Reads `10k_change_calc.gpkg`, identifies hotspots based on the rules in `HOTS_CFG`, and exports hotspot vector layers. It also generates many of the summary plots and LCC driver overlap tables.

3.  **`hotspot_synthesis.qmd`**: Calculates hotspot intensity (coverage), relative intensity, multi-service "hotness", and population exposure. Exports the final summary tables (`hotspot_area_stats.csv`, `hotspot_pop_exposure.csv`, etc.). The final chunk (`regional-subsets-export`) also splits `hotspot_area_stats.csv` into per-group CSV files under `data/processed/tables/regional_subsets/` for use by the regional report template. Run this chunk interactively when `hotspot_area_stats.csv` changes without needing to re-render the whole notebook.

4.  **`KS_tests_hotspots.qmd`**: Consumes the hotspot outputs to perform socioeconomic profiling using Kolmogorov-Smirnov tests.

After the analysis chain, run `scripts/audit_claims.R` to validate key paper claims (income group disparity, attribution gap %, regional intensities) against the actual output CSVs.

---
*This README should be updated as the project evolves.*