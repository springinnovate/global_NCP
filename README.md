# README

Jeronimo Rodriguez Escobar

# Overview

Working version of a structured workflow for extracting, analyzing, and visualizing **zonal summary statistics** from global raster datasets including **ecosystem service (ES)**, **land cover (LC)**, and socioeconomic (beneficiary) layers. The analysis synthesizes outputs across multiple spatial aggregation levels (e.g., basins, countries, regions).

The core of the workflow leverages the R package [`exactextractr`](https://github.com/isciences/exactextractr), which enables efficient zonal operations between raster and vector data. Python workflows use `taskgraph` for parallel execution.

These tools support reproducible extraction and visualization of ES trends and change detection across modeled periods. They enable exploratory and comparative analyses of spatial transformations, ES provision, and relationships to beneficiary groups.

# Objectives

-   Extract and standardize zonal summary statistics for ES rasters.
-   Compute bi-temporal changes in ES provision (e.g., 1992–2020).
-   Generate land cover change matrices and synthesize metrics like gain, loss, and persistence.
-   Support hotspot detection using top/bottom thresholds or directional logic.
-   Enable exploratory visualization and plotting using `ggplot2` or `tmap`.

# Input Data

## Polygon Layers

Stored in the `vector/` folder and include:

-   HydroBASINS (Levels 6 and 7)
-   Country boundaries
-   Income groups, World Bank regions, continents
-   WWF Biomes and Ecoregions

Each basin is assigned to the country where the largest area is contained, facilitating non-spatial joins with country-level attributes. (This logic may be revised for long, transboundary basins.)

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

# R Workflow Summary

## 1. Load Vector & Raster Data

-   Vector files: `vector/*.gpkg`
-   Raster files: `input_ES/*.tif`

## 2. Extract ES Statistics

-   Use `exactextractr::exact_extract()`
-   Compute mean, sum, stdev for each year
-   Calculate bi-temporal % change (e.g., `((2020 - 1992)/1992)*100`)

## 3. Identify Hotspots

Hotspots are defined using top/bottom thresholds, with custom logic for distinguishing between **loss of benefits** and **increase in damages**.

-   For Sediment/Nitrogen export: increases are bad → identify top 1–5%
-   For Retention, Pollination, Coastal Protection: losses are bad → identify bottom 1–5%

Binary indicator columns are added for mapping or filtering.

## 4. Merge Beneficiary Data

Socioeconomic variables are added from gridded layers using `exact_extract()`:

-   HDI (2020)
-   Population density
-   Farm size
-   Built-up area
-   GDP

## 5. Visualization

-   Scatterplots: % change in ES vs.beneficiary variables
-   Maps: faceted binary hotspot maps per service (`tmap::tm_facets()`)

::: {.cell layout-align="center"}
<img src="outputs/es_change_barplot.png" width="70%"/>
:::

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
