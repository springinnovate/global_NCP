# README

Jeronimo Rodriguez Escobar
Affiliation: Global Science, WWF
Supervisor: Becky Chaplin-Kramer
Version: v1.3.1
Last updated: 2026-04-09

# Executive Summary

This workflow brings together global data on ecosystem services, land cover, and people to help us understand where nature is changing, who is affected, and where action is most needed. It combines data processing, change detection, and hotspot mapping in a way that is transparent and reproducible.

**Why does it matter?**
By identifying areas of rapid change or high importance, this pipeline supports better decision-making for conservation, policy, and sustainable development.

## Glossary
- **AOO**: Area of Occupancy. A standard 10 km equal-area grid used for spatial analysis.
- **ES**: Ecosystem Services. Benefits people obtain from nature (e.g., pollination, coastal protection).
- **Hotspot**: A grid cell showing unusually high or low relative change in ecosystem services (the extreme 5% tail of the distribution).
- **KS Analysis**: Kolmogorov-Smirnov test, a statistical method used here to compare the socioeconomic profiles of hotspots vs. non-hotspots.
- **Zonal Statistics**: Calculations that summarize high-resolution raster data within the boundaries of polygons (grid cells or regions).

# Overview

Working version of a structured workflow for extracting, analyzing, and visualizing **zonal summary statistics** from global raster datasets including **ecosystem service (ES)**, **land cover (LC)**, and socioeconomic (beneficiary) layers. The analysis is built around the IUCN AOO **10 km equal-area grid** (land-only) enriched with country/region/biome attributes, with outputs aggregated to countries, regions, income groups, and biomes.

The core extraction workflow uses Python (`taskgraph` + `exactextract`) for zonal summaries; R/Quarto is used for consolidation, change calculations, hotspot extraction, and KS tests.

For a detailed technical description of the pipeline steps, see analysis/README_pipeline.md.

## Documentation Structure

To keep information organized, this project uses the following structure:

*   **`README.md` (This file):** High-level project overview, setup, and pipeline usage.
*   **`README_Methodology.md`:** Detailed theoretical framework, mathematical definitions (e.g., Symmetric Percentage Change), and aggregation logic.
*   **`analysis/README.md`:** A technical **Runbook** for executing the R/Quarto analysis scripts in the correct order.

These tools support reproducible extraction and visualization of ES trends and change detection across modeled periods. They enable exploratory and comparative analyses of spatial transformations, ES provision, and relationships to beneficiary groups.

# Objectives

-   Extract and standardize zonal summary statistics for ES rasters.
-   Compute bi-temporal changes in ES provision (e.g., 1992–2020).
-   Generate land cover change matrices and synthesize metrics like gain, loss, and persistence.
-   Support hotspot detection using top/bottom thresholds or directional logic.
-   Enable exploratory visualization and plotting using `ggplot2` or `tmap`.
-   Assess the distribution of hotspots across services, locations, and demographics using statistical analysis (e.g., KS tests), going beyond simple visualization.

# Input Data

## Polygon Layers / Grid

-   **IUCN AOO 10 km equal-area grid (land-only)**: stored under `vector_basedata/`, enriched with country, income group, WB/UN regions, continent, WWF biome.
-   Country boundaries and regional lookups (income, WB/UN regions, continent) under `vector_basedata/`.
-   WWF Biomes and Ecoregions.

## Raster Layers

Stored under the external data root (`raw/`), include:

-   InVEST-modeled ecosystem services for 1992 and 2020
-   ESA 300m land cover products (reclassified into binary: Transformed/Natural)
-   Global gridded socioeconomic datasets (e.g., GDP, HDI, population density)

## Modeled Ecosystem Services

1.  **Nitrogen Export** – [InVEST NDR](https://naturalcapitalproject.stanford.edu/software/invest): kg/hectare/year (Standardized from pixel)
2.  **Sediment Export/Retention** – [InVEST SDR](https://naturalcapitalproject.stanford.edu/software/invest): ton/hectare/year (Standardized from pixel)
3.  **USLE** – Soil erosion proxy. Derived from the *Revised Universal Soil Loss Equation* [USLE](https://storage.googleapis.com/releases.naturalcapitalproject.org/invest-userguide/latest/en/sdr.html)
4.  **Pollination** – [InVEST Pollination Model](https://naturalcapitalproject.stanford.edu/software/invest): People fed on habitat
5.  **Coastal Protection** – [InVEST Coastal Vulnerability](https://naturalcapitalproject.stanford.edu/software/invest): Unitless vulnerability index
6.  Sediment Retention Service: $$
    \text{Potential Sediment Retention} = \frac{\text{USLE} - \text{Export}}{\text{USLE}}
    $$

## Land Cover Layers

ESA 300m maps reclassified as:

-   **Class 1**: Transformed
-   **Class 2**: Natural

Land cover change metrics (gain, loss, persistence, etc.) are derived using `diffeR::crosstabm()` and `diffeR::difftablej()` following Pontius & Santacruz (2014). These include:

-   Gain / Loss
-   Persistence
-   Quantity / Exchange / Shift

Metrics are computed for each class and overall and then reshaped into wide format.

::: {.cell layout-align="center"}
<img src="output_maps/OriginalServices_chg_1992_2020.png" width="60%"/>
:::

# Pipeline Usage (Python)

The `summary_pipeline_landgrid.py` script executes batch zonal summaries using
`taskgraph` inside a Docker container. Inputs are defined in YAML files under
`analysis_configs/` (e.g., `services_slim.yaml`, `beneficiaries_slim.yaml`,
`c_protection_synth.yaml`) and point to the canonical IUCN AOO 10 km vector grid
(`AOOGrid_10x10km_land_4326_clean.gpkg`) plus the raw raster inputs.

To execute:

``` bash
docker pull therealspring/global_ncp-computational-environment:latest

# Linux/macOS
docker run -it --rm \
  -v $(pwd):/workspace \
  -v /home/jeronimo/data/global_ncp:/data \
  -w /workspace \
  therealspring/global_ncp-computational-environment:latest /bin/bash

# Windows
docker run -it --rm ^
  -v %CD%:/workspace ^
  -v C:\path\to\global_ncp\data:/data ^
  -w /workspace ^
  therealspring/global_ncp-computational-environment:latest /bin/bash
```

Then, run the workflow:

``` bash
python summary_pipeline_landgrid.py --data-root /data analysis_configs/services_slim.yaml
python summary_pipeline_landgrid.py --data-root /data analysis_configs/beneficiaries_slim.yaml
python summary_pipeline_landgrid.py --data-root /data analysis_configs/c_protection_synth.yaml
```

Each raster-vector combo is processed in parallel, using `exactextract` for
zonal summaries. Results are cached and returned quickly on reruns.

If you change grids or configs, clear the workspace cache (or set a new
workspace dir) to avoid stale taskgraph outputs:

``` bash
rm -f summary_pipeline_workspace/*.gpkg
rm -f summary_pipeline_workspace/taskgraph_data.db
```

``` bash
# requires GLOBAL_NCP_DATA to be set (e.g., /home/jeronimo/data/global_ncp)
COASTAL_INCLUDE_CH=1 python Python_scripts/rasterize_coastal.py
```

``` bash
# identify outputs (services = older, beneficiaries = newer)
ls -lt summary_pipeline_workspace/*.gpkg

OUT_DIR=/home/jeronimo/data/global_ncp/interim
TS=$(date +%Y%m%d_%H%M%S)

SERV_SRC=/home/jeronimo/projects/global_NCP/summary_pipeline_workspace/<services_file>.gpkg
ogr2ogr -wrapdateline -datelineoffset 180 \
  "$OUT_DIR/10k_grid_synth_serv_${TS}.gpkg" "$SERV_SRC"

BEN_SRC=/home/jeronimo/projects/global_NCP/summary_pipeline_workspace/<beneficiaries_file>.gpkg
ogr2ogr -wrapdateline -datelineoffset 180 \
  "$OUT_DIR/10k_grid_synth_benef_${TS}.gpkg" "$BEN_SRC"

# coastal protection summary (single output in workspace)
COAST_SRC=/home/jeronimo/projects/global_NCP/summary_pipeline_workspace/<coastal_file>.gpkg
ogr2ogr -wrapdateline -datelineoffset 180 \
  "$OUT_DIR/10k_grid_synth_coastal_${TS}.gpkg" "$COAST_SRC"

# Naming convention: synthesis outputs start with "10k_"
```

# R Analysis Workflow

The R analysis workflow is conducted through a series of Quarto notebooks located in the `analysis/` directory. These notebooks should be executed in the following order after the Python pre-processing is complete.

## 1. Data Consolidation (`analysis/process_data.qmd`)
- **Purpose:** Synthesizes the raw `10k_grid_synth_*.gpkg` interim files. Re-aggregates fragmented polygons via `st_intersects` spatial join to the canonical 10km grid, and calculates absolute/percentage change.
- **Outputs:** Generates the canonical `10k_change_calc.gpkg`.

## 2. Land Cover Change Analysis (`analysis/LC_change_preparation.qmd` & `LC_change_granular.qmd`)
- **Purpose:** Extracts raw ESA/C3S land cover data, simplifies it into 9 canonical classes, and calculates specific transition metrics (Gross Forest Loss, Urban/Cropland Expansion).
- **Outputs:** Exports `10k_lcc_granular_metrics.gpkg`.

## 3. Hotspot Extraction + Plots (`analysis/hotspot_extraction.qmd`)
- **Purpose:** Identifies hotspots using the centralized `HOTS_CFG` (5% threshold). Integrates Land Cover metrics to attribute specific LCC drivers to ES hotspots. Generates regional bar plots and hotspot distribution boxplots.
- **Outputs:** Exports hotspot feature layers to `processed/hotspots/` and plots to `outputs/plots/`.

## 4. Statistical Analysis (`analysis/KS_tests_hotspots.qmd`)
- **Purpose:** Runs Kolmogorov-Smirnov (KS) tests comparing the socioeconomic profile (GDP, Population, HDI) of hotspots against the median 5% of the background landscape.
- **Outputs:** Generates heatmaps, ECDF distribution plots, and Cliff's Delta effect size charts.

## 5. Supplemental Analysis
- **`analysis/hotspot_intensity.qmd`:** Quantifies the spatial extent of hotspots (percentage of land area) and regional enrichment.
- **`analysis/hotspot_multiservice.qmd`:** Analyzes overlap of hotspots across different services ("Hotness").

## Analytical Framework
The analysis of Global NCP Hotspots (1992–2020) leverages an `exactextract`-powered vector backbone to guarantee fractional geometry precision. It is conducted through two distinct but complementary lenses:

A. Geographic Distribution (Where are they?)

Objective: To quantify the spatial extent of hotspots across different jurisdictional and ecological boundaries, utilizing the master vector grid's authoritative attribute joins.

Grouping Variables: World Bank Regions, Income Groups, and Biomes.

Key Metric: Percent Area (%) — The proportion of land area within each unit classified as a "Hotspot" of change.

B. Socioeconomic Characterization (Who is affected?)

Objective: To determine if hotspots are statistically concentrated in areas with specific socioeconomic profiles compared to non-hotspot areas.

Comparison: "Hotspot" vs. "Non-Hotspot" locations.

# Future Directions

-   Implement PostgreSQL + PostGIS backend
-   Normalize values (e.g., population-weighted) during extraction
-   Extend temporal coverage (e.g., 1990–2020 at 5-year intervals)
-   **TODO:** Compare results between the 10km grid-based approach and per-pixel analysis to quantify differences and determine the optimal method.
-   Add transitions and swap metrics to land cover summaries
-   Build R + Python dashboards or plug-ins for visualization

::: {.callout-tip icon="true"}
## Future Tasks & Ideas

Here are some ideas and future tasks for this analysis:

1.  **Adapt analysis for multi-temporal data:** Adapt analysis to handle updated modeled ES layers and multiple points in time (beyond bi-temporal T0, T1). Strategize for incorporating multi-temporal data.
2.  **Quantify hotspot vs. non-hotspot change:** Develop a method to quantify and visualize the share of total change (from bar plots) that occurs within hotspots versus outside of them, possibly using stacked bar plots.
:::

## License

This project is licensed under the [Apache License 2.0](LICENSE).

# Contributors

-   Jeronimo Rodriguez Escobar
-   Richard P. Sharp

For contributions or issues, [open a GitHub issue](https://github.com/springinnovate/global_NCP/issues) or submit a pull request.
