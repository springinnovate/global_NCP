# README

Jeronimo Rodriguez Escobar
Affiliation: Global Science, WWF
Supervisor: Becky Chaplin-Kramer
Version: v1.3.3
Last updated: 2026-05-12

# Executive Summary

This workflow brings together global data on ecosystem services, land cover, and people to help us understand where nature is changing, who is affected, and where action is most needed. It combines data processing, change detection, and hotspot mapping in a way that is transparent and reproducible.

**Why does it matter?**
By identifying areas of rapid change or high importance, this pipeline supports better decision-making for conservation, policy, and sustainable development.

## Quick Start: Running the Full Pipeline

### Prerequisites
- Docker (for Python component)
- R (4.0+) with required packages (see `/environment.yml` for conda environment)
- Access to global_ncp data directory (external; set `GLOBAL_NCP_DATA` environment variable)

### Data Preparation (One-Time Setup)

The analysis pipelines require a clean, canonical 10km grid file. The original method used an R script (`analysis/prepare_data.qmd`), but this has been a source of persistent geometry errors.

A more robust, pure-Python alternative is now provided and is the recommended approach.

#### Grid Preparation using Python (Recommended)

This script creates a clean grid file guaranteed to be compatible with the downstream Python pipelines. Run this from your terminal, providing the path to your data directory.

```bash
# Replace `/path/to/global_ncp/data` with the actual path to your data
python analysis/prepare_grid_python.py --data-root /path/to/global_ncp/data
```

### Running the Python Stage (Raster-based Zonal Statistics)

```bash
# Pull Docker image
docker pull therealspring/global_ncp-computational-environment:latest

# First, generate the zone raster (one-time setup) from an R console:
# Rscript analysis/create_zone_raster.R

# Then, run the raster-based zonal stats pipeline inside the Docker container:
docker run -it --rm \
  -v $(pwd):/workspace \
  -v /path/to/global_ncp/data:/data \
  -w /workspace \
  therealspring/global_ncp-computational-environment:latest /bin/bash

# Inside container, execute raster-based zonal summaries:
python summary_pipeline_rasterzones.py --data-root /data analysis_configs/services_raster.yaml
# python summary_pipeline_rasterzones.py --data-root /data analysis_configs/beneficiaries_raster.yaml
```

### Running the R/Quarto Analysis Chain

Execute the following Quarto notebooks **in order** from the repository root:

```bash
# Full sequential analysis
quarto render analysis/prepare_data.qmd
quarto render analysis/process_data.qmd
quarto render analysis/hotspot_extraction.qmd
quarto render analysis/hotspot_synthesis.qmd
quarto render analysis/KS_tests_hotspots.qmd
quarto render analysis/results_interpretation.qmd
```

Or render the full book (includes all chapters):

```bash
quarto render
```

Output files will be saved to:
- `processed/` – Intermediate GeoPackage and data files
- `outputs/plots/` – Generated visualizations and maps


- **AOO**: Area of Occupancy. A standard 10 km equal-area grid used for spatial analysis.
- **ES**: Ecosystem Services. Benefits people obtain from nature (e.g., pollination, coastal protection).
- **Hotspot**: A grid cell showing unusually high or low relative change in ecosystem services (the extreme 5% tail of the distribution).
- **KS Analysis**: Kolmogorov-Smirnov test, a statistical method used here to compare the socioeconomic profiles of hotspots vs. non-hotspots.
- **Zonal Statistics**: Calculations that summarize high-resolution raster data within the boundaries of polygons (grid cells or regions).

# Overview

Working version of a structured workflow for extracting, analyzing, and visualizing **zonal summary statistics** from global raster datasets including **ecosystem service (ES)**, **land cover (LC)**, and socioeconomic (beneficiary) layers. The analysis is built around the IUCN AOO **10 km equal-area grid** (land-only) enriched with country/region/biome attributes, with outputs aggregated to countries, regions, income groups, and biomes.

The core extraction workflow uses Python (`taskgraph` + `exactextract`) for zonal summaries; R/Quarto is used for consolidation, change calculations, hotspot extraction, and KS tests.

For a detailed technical description of the pipeline, see the project's official documentation in the `/docs` directory, particularly `docs/methodology.md` and `docs/runbook.md`.

### Pipeline Architecture

```mermaid
%%{init: {'flowchart': {'rankSpacing': 300, 'nodeSpacing': 30}}}%%
flowchart LR
    %% Subgraph Styling
    style INPUTS fill:#F8F9FA,stroke:#D3D3D3,stroke-width:2px
    style PROCESSING fill:#F8F9FA,stroke:#D3D3D3,stroke-width:2px
    style OUTPUTS fill:#F8F9FA,stroke:#D3D3D3,stroke-width:2px

    %% Input Layer
    subgraph INPUTS [" "]
        direction TB
        RawES["&lt;span style='font-size: 38px;'&gt;&lt;b&gt;Global InVEST ES Models&lt;/b&gt;&lt;/span&gt; &lt;br/&gt; &lt;span style='font-size: 32px;'&gt;300m Rasters &lt;i&gt;(1992 & 2020)&lt;/i&gt;&lt;/span&gt;"]
        RawGrid["&lt;span style='font-size: 38px;'&gt;&lt;b&gt;IUCN AOO 10km Master Grid&lt;/b&gt;&lt;/span&gt; &lt;br/&gt; &lt;span style='font-size: 32px;'&gt;&lt;i&gt;Vector with Subregional Attributes&lt;/i&gt;&lt;/span&gt;"]
        RawLC["&lt;span style='font-size: 38px;'&gt;&lt;b&gt;ESA CCI Land Cover&lt;/b&gt;&lt;/span&gt; &lt;br/&gt; &lt;span style='font-size: 32px;'&gt;300m Rasters &lt;i&gt;(1992 & 2020)&lt;/i&gt;&lt;/span&gt;"]
        RawSoc["&lt;span style='font-size: 38px;'&gt;&lt;b&gt;Socioeconomic Data&lt;/b&gt;&lt;/span&gt; &lt;br/&gt; &lt;span style='font-size: 32px;'&gt;Rasters &lt;i&gt;(Pop, GDP, HDI)&lt;/i&gt;&lt;/span&gt;"]
    end

    %% Processing Layer
    subgraph PROCESSING [" "]
        direction TB
        IntA["&lt;span style='font-size: 38px;'&gt;&lt;b&gt;Path A: Global Trajectories&lt;/b&gt;&lt;/span&gt; &lt;br/&gt; &lt;span style='font-size: 32px;'&gt;Zonal Summaries &lt;i&gt;(1992 & 2020)&lt;/i&gt;&lt;/span&gt;"]
        MathA["&lt;span style='font-size: 38px;'&gt;&lt;b&gt;Path A Metrics&lt;/b&gt;&lt;/span&gt; &lt;br/&gt; &lt;span style='font-size: 32px;'&gt;SPC & Absolute Difference&lt;/span&gt;"]

        IntB["&lt;span style='font-size: 38px;'&gt;&lt;b&gt;Path B: Grid Analysis&lt;/b&gt;&lt;/span&gt; &lt;br/&gt; &lt;span style='font-size: 32px;'&gt;10km Zonal Summaries &lt;i&gt;(1992 & 2020)&lt;/i&gt;&lt;/span&gt;"]
        MathB["&lt;span style='font-size: 38px;'&gt;&lt;b&gt;Path B Metrics&lt;/b&gt;&lt;/span&gt; &lt;br/&gt; &lt;span style='font-size: 32px;'&gt;SPC & Absolute Difference&lt;/span&gt;"]

        MathLC["&lt;span style='font-size: 38px;'&gt;&lt;b&gt;Land Cover Transitions&lt;/b&gt;&lt;/span&gt; &lt;br/&gt; &lt;span style='font-size: 32px;'&gt;Reclassified LC Contingency &lt;br/&gt; Matrices per 10km Gridcell&lt;/span&gt;"]
        MathSoc["&lt;span style='font-size: 38px;'&gt;&lt;b&gt;Socioeconomic Stats & KS Tests&lt;/b&gt;&lt;/span&gt; &lt;br/&gt; &lt;span style='font-size: 32px;'&gt;10km Grid Aggregation &lt;br/&gt; & Statistical Profiling&lt;/span&gt;"]
    end

    %% Outputs Layer
    subgraph OUTPUTS [" "]
        direction TB
        P1["&lt;span style='font-size: 38px;'&gt;&lt;b&gt;WHAT: Global Trajectories&lt;/b&gt;&lt;/span&gt; &lt;br/&gt; &lt;i style='font-size: 32px; font-weight: normal;'&gt;Bar Charts, Summary Tables, &lt;br/&gt; & Cartographies (GPKGs)&lt;/i&gt;"]
        P2["&lt;span style='font-size: 38px;'&gt;&lt;b&gt;WHERE: Hotspot Detection (Top/Bottom 5%)&lt;/b&gt;&lt;/span&gt; &lt;br/&gt; &lt;i style='font-size: 32px; font-weight: normal;'&gt;Abs & SPC GPKGs, Synthesis Maps, &lt;br/&gt; & Distribution Plots&lt;/i&gt;"]
        P3["&lt;span style='font-size: 38px;'&gt;&lt;b&gt;WHY: Attribution Gap&lt;/b&gt;&lt;/span&gt; &lt;br/&gt; &lt;i style='font-size: 32px; font-weight: normal;'&gt;LCC Overlap CSVs, Heatmaps, &lt;br/&gt; Scatterplots, & Driver Maps&lt;/i&gt;"]
        P4["&lt;span style='font-size: 38px;'&gt;&lt;b&gt;WHO: Equity & Exposure&lt;/b&gt;&lt;/span&gt; &lt;br/&gt; &lt;i style='font-size: 32px; font-weight: normal;'&gt;KS Test Plots & &lt;br/&gt; Population Exposure CSVs&lt;/i&gt;"]
    end

    %% Logical Connections
    RawGrid ==&gt; IntB
    RawGrid ==&gt; MathLC
    RawGrid ==&gt; MathSoc

    RawES ==&gt; IntA
    RawES ==&gt; IntB

    IntA ==&gt; MathA
    IntB ==&gt; MathB

    MathA ==&gt; P1
    MathB ==&gt; P2

    %% Downstream Analysis from Hotspots (P2)
    P2 ==&gt; P3
    RawLC ==&gt; MathLC
    MathLC ==&gt; P3

    P2 ==&gt; P4
    RawSoc ==&gt; MathSoc
    MathSoc ==&gt; P4

    %% Layout Guides
    RawSoc ~~~ MathSoc
    IntA ~~~ P1

    %% CANONICAL COLOR CLASSES (Matching Circular Diagram)
    classDef c_what fill:#007930,stroke:#004D1E,stroke-width:3px,color:#FFF;
    classDef c_where fill:#7B8327,stroke:#515619,stroke-width:3px,color:#FFF;
    classDef c_why fill:#F07D00,stroke:#A85700,stroke-width:3px,color:#FFF;
    classDef c_who fill:#F5D200,stroke:#B39900,stroke-width:3px,color:#333;

    %% Pillar Assignments
    class RawES,IntA,MathA,P1 c_what;
    class RawGrid,IntB,MathB,P2 c_where;
    class RawLC,MathLC,P3 c_why;
    class RawSoc,MathSoc,P4 c_who;
```

## Repository Structure

The project is organized into the following key directories. For detailed technical documentation, refer to the files within the `/docs` directory.

*   `/analysis/`: Contains the **active** Quarto notebooks for data processing, synthesis, and interpretation. This is where the main narrative of the analysis lives.

*   `/notebooks/`: An **archive** for legacy or completed notebooks from previous analysis phases. This keeps the `/analysis/` directory clean and focused on current work.

*   `/R/`: Contains the core, reusable R functions for the project. This directory is structured like an R package source directory.

*   `/scripts/`: Contains standalone utility scripts for specific, often automated, tasks like map generation or monitoring long-running jobs.

*   `/docs/`: Contains all project documentation, including the detailed methodology, runbooks, and data dictionaries. This is the single source of truth for project context.

*   `/outputs/`: Contains generated plots, maps, and other visual outputs from the analysis. This directory is in `.gitignore`.

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

1.  **Nitrogen Export** – InVEST NDR: kg/hectare/year (Standardized from pixel)
2.  **Sediment Export/Retention** – InVEST SDR: ton/hectare/year (Standardized from pixel)
3.  **USLE** – Soil erosion proxy. Derived from the *Revised Universal Soil Loss Equation* USLE
4.  **Pollination** – InVEST Pollination Model: People fed on habitat
5.  **Coastal Protection** – InVEST Coastal Vulnerability: Unitless vulnerability index
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
&lt;img src="output_maps/OriginalServices_chg_1992_2020.png" width="60%"/&gt;
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

SERV_SRC=/home/jeronimo/projects/global_NCP/summary_pipeline_workspace/&lt;services_file&gt;.gpkg
ogr2ogr -wrapdateline -datelineoffset 180 \
  "$OUT_DIR/10k_grid_synth_serv_${TS}.gpkg" "$SERV_SRC"

BEN_SRC=/home/jeronimo/projects/global_NCP/summary_pipeline_workspace/&lt;beneficiaries_file&gt;.gpkg
ogr2ogr -wrapdateline -datelineoffset 180 \
  "$OUT_DIR/10k_grid_synth_benef_${TS}.gpkg" "$BEN_SRC"

# coastal protection summary (single output in workspace)
COAST_SRC=/home/jeronimo/projects/global_NCP/summary_pipeline_workspace/&lt;coastal_file&gt;.gpkg
ogr2ogr -wrapdateline -datelineoffset 180 \
  "$OUT_DIR/10k_grid_synth_coastal_${TS}.gpkg" "$COAST_SRC"

# Naming convention: synthesis outputs start with "10k_"
```

# Active R Analysis Workflow

The R/Quarto analysis workflow is conducted through a series of notebooks in the `analysis/` directory. These scripts should be **executed in the following order** to ensure data dependencies are met. The workflow implements the **WHAT → WHERE → WHY → WHO** framework (see pipeline architecture above).

## Core R Analysis Chain (Execution Order)

1. **`prepare_data.qmd`** – **Data Preparation & Baseline Setup**
   - Loads raw zonal statistics from Python pipeline outputs
   - Prepares and validates the canonical 10km IUCN AOO grid with subregional attributes
   - Creates baseline data structures for downstream analysis
   - Output: Intermediate processed data files

2. **`process_data.qmd`** – **WHAT: Global Ecosystem Service Trajectories**
   - Consolidates base zonal statistics (1992 & 2020 values)
   - Calculates bi-temporal change: absolute difference and Symmetric Percentage Change (SPC)
   - Produces the canonical `processed/10k_change_calc.gpkg` file (used by all downstream steps)
   - Generates global trajectory summaries and bar charts
   - **Key Output:** `10k_change_calc.gpkg`

3. **`LC_change*.qmd`** – **Land Cover Change Processing** (optional preprocessing)
   - `LC_change_preparation.qmd`: Prepares ESA CCI reclassified land cover data
   - `LC_change.qmd`: Computes land cover transition matrices and derived metrics (gain, loss, persistence)
   - `LC_change_rasters.qmd`, `LC_change_granular.qmd`, `viz_granular_lcc.qmd`: Detailed LCC visualizations

4. **`hotspot_extraction.qmd`** – **WHERE: Hotspot Detection (Top/Bottom 5%)**
   - Reads `10k_change_calc.gpkg` from Process Data step
   - Identifies hotspots using relative thresholds (extreme 5% of SPC distribution)
   - Exports hotspot vector layers to `processed/hotspots/`
   - Generates bar plots, violin plots, and distribution summaries by subregion
   - **Key Outputs:** Hotspot GPKGs, diagnostic plots

5. **`hotspot_synthesis.qmd`** – **WHERE & WHO: Hotspot Intensity & Population Exposure**
   - Calculates hotspot coverage (area statistics), relative intensity, and multi-service "hotness"
   - Integrates socioeconomic data (population density, GDP, HDI)
   - Exports summary tables: `hotspot_area_stats.csv`, `hotspot_pop_exposure.csv`
   - Produces clustering plots and heatmaps
   - **Key Outputs:** Summary statistics tables, clustering visualizations

6. **`KS_tests_hotspots.qmd`** – **WHO: Socioeconomic Profiling & Attribution Analysis**
   - Performs Kolmogorov-Smirnov (KS) tests on hotspot vs. non-hotspot populations
   - Compares socioeconomic profiles (population, GDP, HDI, built area, etc.)
   - Generates KS test plots and statistical summaries
   - Links ecosystem service hotspots to drivers (land cover conversion, urbanization)

7. **`results_interpretation.qmd`** – **Synthesis: Narrative & Interpretation**
   - The final analysis notebook that synthesizes outputs from all prior steps
   - Constructs the narrative answering: **WHERE are hotspots?**, **WHO is affected?**, **WHY (what are the drivers)?**
   - Generates key findings, figures, and tables for manuscript or presentations
   - **Recommended as the source document for presentations and co-author communication**

## Data Flow Summary

```
Python Pipeline (Docker)
    ↓
    → summary_pipeline_landgrid.py
    → Outputs: Zonal summaries (1992 & 2020 rasters × 10km grid)
    ↓
R Analysis Chain (Sequential)
    ↓
    prepare_data.qmd → process_data.qmd [creates 10k_change_calc.gpkg]
    ↓
    hotspot_extraction.qmd [hotspot identification]
    ↓
    hotspot_synthesis.qmd [intensity & exposure]
    ↓
    KS_tests_hotspots.qmd [socioeconomic profiling]
    ↓
    results_interpretation.qmd [narrative synthesis]
    ↓
Final Outputs: Maps, summary tables, KS test plots, manuscript figures
```

## For Complete Technical Details

See the project runbook for detailed methodology and validation notes:

*   **`docs/runbook.md`** – Full execution guide and validation procedures
*   **`docs/methodology.md`** – Technical explanation of two-path analysis structure
*   **`analysis/README.md`** – Archive policy and notebook scoping

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

This project is licensed under the Apache License 2.0.

# Contributors

-   Jeronimo Rodriguez Escobar
-   Richard P. Sharp

For contributions or issues, open a GitHub issue or submit a pull request.