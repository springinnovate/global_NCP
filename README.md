# README

Jeronimo Rodriguez Escobar

# Overview

Working version of a structured workflow for extracting, analyzing, and visualizing **zonal summary statistics** from global raster datasets including **ecosystem service (ES)**, **land cover (LC)**, and socioeconomic (beneficiary) layers. The analysis is built around a synthesized **10 km grid** (`processed/10k_change_calc.gpkg`, external data dir) enriched with country/region/biome attributes, with outputs aggregated to countries, regions, income groups, and biomes.

The core of the workflow leverages the R package [`exactextractr`](https://github.com/isciences/exactextractr), which enables efficient zonal operations between raster and vector data. Python workflows use `taskgraph` for parallel execution.

These tools support reproducible extraction and visualization of ES trends and change detection across modeled periods. They enable exploratory and comparative analyses of spatial transformations, ES provision, and relationships to beneficiary groups.

# Objectives

-   Extract and standardize zonal summary statistics for ES rasters.
-   Compute bi-temporal changes in ES provision (e.g., 1992–2020).
-   Generate land cover change matrices and synthesize metrics like gain, loss, and persistence.
-   Support hotspot detection using top/bottom thresholds or directional logic.
-   Enable exploratory visualization and plotting using `ggplot2` or `tmap`.

# Input Data

## Polygon Layers / Grid

-   **10 km grid with change + attributes**: `processed/10k_change_calc.gpkg` (external data dir), enriched with country, income group, WB/UN regions, continent, WWF biome.
-   Country boundaries and regional lookups (income, WB/UN regions, continent) under `vector_basedata/`.
-   WWF Biomes and Ecoregions.

## Raster Layers

Stored in `input_ES/`, include:

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

The `summary_pipeline.py` script executes batch zonal summaries using `taskgraph`. Inputs and logic are defined through these key data structures:

-   `ANALYSIS_DATA`
-   `REFERENCE_SUMMARY_VECTOR_PATHS`
-   `ZONAL_OPS`

These define the rasters, vectors, and operations to apply. To execute:

``` bash
docker pull therealspring/global_ncp-computational-environment:latest

# Linux/macOS
docker run -it --rm -v $(pwd):/workspace therealspring/global_ncp-computational-environment:latest /bin/bash

# Windows
docker run -it --rm -v %CD%:/workspace therealspring/global_ncp-computational-environment:latest /bin/bash
```

Then, run the workflow:

``` bash
python summary_pipeline.py
```

Each raster-vector combo is processed in parallel, using `exactextract` for zonal summaries. Results are cached and returned quickly on reruns.

After the run, the pipeline writes timestamped GPKGs to `summary_pipeline_workspace/`.
Move/rename them into the interim folder and wrap dateline geometries for clean
mapping (prevents the 180° wraparound polygon artifact):

``` bash
# identify outputs (services = older, beneficiaries = newer)
ls -lt summary_pipeline_workspace/*.gpkg

OUT_DIR=/home/jeronimo/data/global_ncp/interim
SERV_SRC=/home/jeronimo/projects/global_NCP/summary_pipeline_workspace/<services_file>.gpkg
BEN_SRC=/home/jeronimo/projects/global_NCP/summary_pipeline_workspace/<beneficiaries_file>.gpkg

ogr2ogr -wrapdateline -datelineoffset 180 \
  "$OUT_DIR/10k_grid_synth_serv.gpkg" "$SERV_SRC"

ogr2ogr -wrapdateline -datelineoffset 180 \
  "$OUT_DIR/10k_grid_synth_benef.gpkg" "$BEN_SRC"
```

# R Analysis Workflow

The R analysis workflow is conducted through a series of Quarto notebooks located in the `analysis/` directory. These notebooks should be executed in the following order after the Python pre-processing is complete.

*(Note: The `notebooks/` directory is considered legacy and the most current work resides in `analysis/`.)*

## 1. Data Consolidation and Change Analysis

-   **File:** `analysis/Consolidation.qmd`
-   **Purpose:** This notebook synthesizes the summary data generated by the Python pipeline into a single, unified database. It then calculates the bi-temporal change between the two time periods and generates initial characterization plots (e.g., bar plots) of these changes, grouped by various administrative and geographical units.

## 2. Hotspot Extraction and Beneficiary Data Integration

-   **File:** `analysis/hotspot_extraction.qmd`
-   **Purpose:** Using the consolidated database, this notebook identifies and extracts "hotspots"—areas of significant change or high/low ecosystem service values. It then merges key beneficiary (socioeconomic) data onto these hotspot geometries, preparing the dataset for the subsequent statistical analysis.

## 3. Kolmogorov-Smirnov (KS) Analysis

-   **File:** `analysis/KS_tests_hotspots.qmd`
-   **Purpose:** This final notebook performs a Kolmogorov-Smirnov (KS) statistical analysis. It compares the distributions of the beneficiary variables within the identified hotspots versus non-hotspot areas to identify significant differences.

# Future Directions

-   Implement PostgreSQL + PostGIS backend
-   Normalize values (e.g., population-weighted) during extraction
-   Extend temporal coverage (e.g., 1990–2020 at 5-year intervals)
-   Add transitions and swap metrics to land cover summaries
-   Build R + Python dashboards or plug-ins for visualization

## License

This project is licensed under the [Apache License 2.0](LICENSE).

# Contributors

-   Jeronimo Rodriguez Escobar
-   Richard P. Sharp

For contributions or issues, [open a GitHub issue](https://github.com/springinnovate/global_NCP/issues) or submit a pull request.
