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

## Glossary
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

# R Analysis Workflow

The R analysis workflow is conducted through a series of Quarto notebooks located in the `analysis/` directory. For a detailed guide on the execution order and purpose of each script, please refer to the main project runbook:

*   **`docs/runbook.md`**

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