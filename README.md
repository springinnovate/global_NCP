---
editor_options: 
  markdown: 
    wrap: 72
---

# Overview

This repository provides a structured workflow to systematically
extract, analyze, and visualize **zonal statistics** from global
ecosystem service (ES) raster datasets for specific spatial units. The
process is built around the **R `exactextractr` package** and other
spatial analysis tools. The workflow facilitates extracting summary
statistics, attaching them as attributes to polygon datasets, and
generating meaningful visualizations to interpret spatial patterns.

# Objectives

-   **Extract zonal statistics** from global raster datasets for
    predefined spatial units (e.g., countries, watersheds, biomes, or
    grid cells).
-   **Label and integrate extracted statistics** into vector datasets
    for further geospatial analysis.
-   **Generate visualizations** such as bar plots and maps to represent
    spatial patterns and temporal changes.
-   **Automate processing steps** to ensure scalability and consistency
    across multiple datasets.

# Dependencies

The analysis is conducted using **R** with the following key libraries:

# Workflow

## 1. Load Polygon Data

Polygon datasets can be loaded from local vector files (`.gpkg` format)
or external sources such as **GADM**. Different spatial aggregations are
considered: **continents, subregions, countries, biomes, watersheds**.

## 2. Load Global Raster Data

The script reads multiple global raster datasets (GeoTIFFs) representing
different ecosystem services. Raster file paths are dynamically
extracted and matched with corresponding **service names** and
**years**.

## 3. Compute Raster Differences

Calculates **temporal changes** (e.g., **1992 vs. 2020**) for each
ecosystem service by subtracting raster values. Outputs difference
rasters are stored for later analysis.

## 4. Extract Zonal Statistics

Uses `exactextractr` to compute **mean values** of each ecosystem
service per polygon. Supports additional statistics such as **median,
standard deviation, and quantiles**. Results are merged with the
original polygon dataset and exported.

## 5. Extract Statistics for Raster Differences

Similar to step 4, but applied to the **difference rasters**. Outputs
summarized trends in ES changes over time.

## 6. Standardize and Fix Column Names

Automates renaming of columns to maintain consistency across outputs.
Ensures naming conventions are clear and aligned for visualization and
GIS integration.

## 7. Generate Visualizations

```         
library(ggplot2)
library(dplyr)

plot_ecosystem_services <- function(data, year, col) {
  data_prepped <- data %>%
    filter(!is.na(mean) & mean > 0 & year == year) %>%
    mutate(temp_col = reorder_within(!!sym(col), -mean, service))  
  
  ggplot(data_prepped, aes(x = temp_col, y = mean, fill = color)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    scale_fill_identity() +
    facet_wrap(~ service, scales = "free") +
    scale_x_reordered() +  
    labs(title = paste("Mean Ecosystem Service Values,", year),
         x = col, y = "Mean Value") +
    theme_bw()
}
```

# Usage

## Running the Workflow

```         
git clone https://github.com/springinnovate/global_NCP.git
cd summary-es
```

Open `summary_es.Rmd` in **RStudio** and execute all sections.

## Automating Future Runs

To streamline processing: - Convert reusable steps into **functions**
(e.g., loading rasters, extracting statistics, plotting results). -
Utilize **parameterized reports** for different spatial aggregations. -
Leverage **parallel processing** (`mclapply`) for faster execution on
large datasets.

# Future Improvements

-   Implement dynamic service name detection based on raster filenames.
-   Improve handling of **multi-country territories** and **small island
    states**.
-   Expand statistical options beyond mean values (e.g., quantiles,
    uncertainty metrics).

# License

This project is licensed under the **MIT License**.

# Contributors

-   **Jeronimo Rodriguez-Escobar** (Primary Author)

For any questions or contributions, feel free to open an issue or submit
a pull request.
