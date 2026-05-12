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

*   `hotspot_synthesis.qmd`: The main data production script. It takes the grid-level change data (`10k_change_calc.gpkg`) and generates all summary tables for hotspot intensity, multi-service overlap, and population exposure. **This is the primary script for generating the data used in the final analysis.**

*   `results_interpretation.qmd`: The primary interpretation script. It loads the summary tables produced by `hotspot_synthesis.qmd` and is used to generate the narrative, key takeaways, and figures for the manuscript.

---
*This README should be updated as the project evolves.*