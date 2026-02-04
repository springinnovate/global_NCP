# README

Jeronimo Rodriguez Escobar
Affiliation: Global Science, WWF
Supervisor: Becky Chaplin-Kramer
Version: v1.0.2
Last updated: 2026-01-22

# Overview

Working version of a structured workflow for extracting, analyzing, and visualizing **zonal summary statistics** from global raster datasets including **ecosystem service (ES)**, **land cover (LC)**, and socioeconomic (beneficiary) layers. The analysis is built around the IUCN AOO **10 km equal-area grid** (land-only) enriched with country/region/biome attributes, with outputs aggregated to countries, regions, income groups, and biomes.

The core extraction workflow uses Python (`taskgraph` + `exactextract`) for zonal summaries; R/Quarto is used for consolidation, change calculations, hotspot extraction, and KS tests.

For a detailed technical description of the pipeline steps, see analysis/README_pipeline.md.

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

1.  **Nitrogen Export** – [InVEST NDR](https://naturalcapitalproject.stanford.edu/software/invest): kg/pixel/year
2.  **Sediment Export/Retention** – [InVEST SDR](https://naturalcapitalproject.stanford.edu/software/invest): ton/pixel/year
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
`c_protection_synth.yaml`) and point to the canonical IUCN AOO 10 km land grid
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

## Coastal Protection Rasterization (points → rasters)

Coastal protection outputs are provided as point features. Rasterize them to the
ESA 300 m land cover template before running zonal summaries:

``` bash
# requires GLOBAL_NCP_DATA to be set (e.g., /home/jeronimo/data/global_ncp)
COASTAL_INCLUDE_CH=1 python Python_scripts/rasterize_coastal.py
```

This produces rasters in:
`$GLOBAL_NCP_DATA/interim/coastal_protection_rasters/`
for `Rt_1992`, `Rt_2020`, and the ratios (plus `Rt_serv_ch` when enabled).

After the run, the pipeline writes timestamped GPKGs to `summary_pipeline_workspace/`.
Move/rename them into the interim folder and wrap dateline geometries for clean
mapping (prevents the 180° wraparound polygon artifact). Use a timestamp to
avoid overwriting prior outputs:

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

*(Note: The `notebooks/` directory is considered legacy and the most current work resides in `analysis/`.)*

## 1. Data Consolidation and Change Analysis

-   **File:** `analysis/Consolidation.qmd`
-   **Purpose:** This notebook synthesizes the summary data generated by the Python pipeline into a single, unified database. It then calculates the bi-temporal change between the two time periods and generates initial characterization plots (e.g., bar plots) of these changes, grouped by various administrative and geographical units.
    -   **Outputs:** `processed/10k_change_calc.gpkg` (canonical subset for downstream work) and `processed/10k_grid_ES_change_benef.gpkg` (full QA dataset with additional mean/sum fields if you need to expand later).

## 2. Hotspot Extraction and Beneficiary Data Integration

-   **File:** `analysis/hotspot_extraction.qmd`
-   **Purpose:** Using the consolidated database, this notebook identifies and extracts "hotspots"—areas of significant change or high/low ecosystem service values. It then merges key beneficiary (socioeconomic) data onto these hotspot geometries, preparing the dataset for the subsequent statistical analysis.

## 3. Kolmogorov-Smirnov (KS) Analysis

-   **File:** `analysis/KS_tests_hotspots.qmd`
-   **Purpose:** This final notebook performs a Kolmogorov-Smirnov (KS) statistical analysis. It compares the distributions of the beneficiary variables within the identified hotspots versus non-hotspot areas to identify significant differences.

## 4. Hotspot Intensity & Multi-service Analysis

-   **File:** `analysis/hotspot_intensity.qmd`
-   **Purpose:** Quantifies the spatial extent of hotspots. Calculates the percentage of land area classified as a hotspot ("Intensity") and the share of global hotspots located within each region/biome.
    -   **Outputs:** `processed/hotspot_area_stats.csv`, Intensity bar plots.

-   **File:** `analysis/hotspot_multiservice.qmd`
-   **Purpose:** Analyzes the overlap of hotspots across different services ("Hotness"). Identifies regions with high coincidence of multiple service declines.
    -   **Outputs:** `processed/hotspot_multiservice_stats.csv`, Hotness distribution plots.

## Analytical Framework
The analysis of Global NCP Hotspots (1992–2020) is conducted through two distinct but complementary lenses:

A. Geographic Distribution (Where are they?)

Objective: To quantify the spatial extent of hotspots across different jurisdictional and ecological boundaries.

Grouping Variables: World Bank Regions, Income Groups, and Biomes.

Key Metric: Percent Area (%) — The proportion of land area within each unit classified as a "Hotspot" of change.

B. Socioeconomic Characterization (Who is affected?)

Objective: To determine if hotspots are statistically concentrated in areas with specific socioeconomic profiles compared to non-hotspot areas.

Comparison: "Hotspot" vs. "Non-Hotspot" locations.

Key Metrics: Distributions of GDP per capita, Population Density, and other socioeconomic indicators (analyzed via Violin Plots and KS/ANOVA tests).

# Methodology Notes

## Symmetric Percentage Change
To address mathematical artifacts where the sign of percentage change differs from absolute change (common when baselines are negative or near-zero), this analysis uses a **symmetric percentage change** calculation (`pct_mode="symm"`). This ensures that the direction of the percentage change always aligns with the absolute difference ($t_1 - t_0$).

**Distribution Limits:** The Symmetric Percentage Change (SPC) metric is bounded between **-200%** (Total Loss) and **+200%** (New Emergence). Consequently, extreme values and clustering at these boundaries, as well as bi-modal distributions (e.g., in Sediment Export), are expected features of the metric rather than data artifacts.

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
