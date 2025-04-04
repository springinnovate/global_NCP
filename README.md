README
================
Jeronimo Rodriguez Escobar

# Overview

Structured workflow to systematically extract, analyze, and visualize
zonal summary statistics from global ecosystem service (ES) raster
datasets and track changes in ES provision over time while linking these
changes to meaningful spatial units.

The entire process is built around the
[`exactextractr`](https://github.com/isciences/exactextractr) package in
‘R’, which offers efficient and precise zonal statistics for raster data
using polygon geometries. This workflow builds upon this functionality
and introduces standardized methods to automate extraction, attach
results as attributes, and visualize patterns across time and space.

This still is a preliminary exploratory phase, we’re developing and
refining methods to characterize spatial-temporal patterns in ES
provision using modeled raster outputs (e.g., InVEST). Initial tests
were performed using countries, regions, and income groups to test the
concept. Now focus on hydrological basins—the same spatial framework
used for many of the ES models themselves—to enhance alignment between
model resolution and spatial unit of analysis.

# Workflow

## 1. Load Polygon Data

Polygon datasets can be loaded from local vector files (`.gpkg` format)
or external sources such as **GADM**. Different spatial aggregations are
considered: **continents, subregions, countries, biomes, watersheds**.

Polygon files are stored in the vector/ directory in .gpkg format.
Multiple levels of aggregation are supported: - Countries and
territories - Continents - World Bank regions - Income groups - WWF
Biomes - Hydrosheds Level 6 and 7 watersheds.

Dissolved polygon layers are generated externally (e.g., in QGIS or
ArcGIS) to ensure topology and attribute consistency.

## 2. Load Raster Data

Raster inputs are located in ‘input_ES/’. They represent global ES
values modeled with InVEST, covering two key years (1992 and 2020), plus
multi-year outputs (e.g., 1992–2004 for NDR). Each raster is labeled
with: Ecosystem service name Year of modeling Units (e.g., kg/ha, people
fed)

1.  Nitrogen Export. Derived from the Nitrogen retention modeled using
    the [**InVEST Nutrient Delivery
    Ratio**](http://data.naturalcapitalproject.org/invest-releases/3.5.0/userguide/ndr.html).
    Expressed in kg/pixel/year

2.  Sediment Retention. Derived using [**InVEST SDR: Sediment Delivery
    Ration**](https://storage.googleapis.com/releases.naturalcapitalproject.org/invest-userguide/latest/en/sdr.html).
    Values in ton/pixel/year

3.  Soil Erosion derived using the *Revised Universal Soil Loss
    Equation-USLE*

4.  Pollination. Derived from [**InVEST SDR: Pollinator Abundance
    Model**](https://storage.googleapis.com/releases.naturalcapitalproject.org/invest-userguide/latest/en/croppollination.html).
    Units represent Polllination Change in people fed on HABitat. More
    information in [**Chaplin-Kramer, et
    al. 2022**](https://static-content.springer.com/esm/art%3A10.1038%2Fs41559-022-01934-5/MediaObjects/41559_2022_1934_MOESM1_ESM.pdf)

5.  Coastal Protection. Unitless measure, refers to a derived
    vulnerability index. [**InVEST Coastal Vulnerability
    Model**](https://storage.googleapis.com/releases.naturalcapitalproject.org/invest-userguide/latest/en/coastal_vulnerability.html)

6.  Nature Access represented as [**the number of people within 1 hour
    travel of natural and semi-natural
    lands**](https://github.com/springinnovate/distance-to-hab-with-friction)
    (Chaplin-Kramer et al, 2022).

## Input Services

<p div style="display: flex; gap: 10px;">
<img src="output_maps/OriginalServices_92.png" width="45%" style="margin-right: 10px;"/>
<img src="output_maps/OriginalServices_2020.png" width="48%" />
</div>

## 3. Compute Raster Differences

Calculates **temporal changes** (e.g., **1992 vs. 2020**) for each
ecosystem service by subtracting raster values. Outputs difference
rasters are stored for later analysis.

<p div style="display: flex; gap: 10px;">
<img src="output_maps/OriginalServices_chg_1992_2020.png" style="margin-right: 10px;"/>
</div>

## 4. Extract Zonal Statistics

Uses `exactextractr` to compute different summary statistics (mean,
stdev, sum) of each ecosystem service per polygon for each year.
Supports additional statistics such as **median, standard deviation, and
quantiles**. Result values are joined to the original polygon dataset
and exported.

We apply the same approach to Similar to step 4, but to the bi temporal
**difference rasters**. Outputs summarized trends in ES changes over
time.

Zonal statistics where extracted directly from difference rasters (e.g.,
NDR_diff_1992_2020.tif), or the differneces can be calcualted from the
extracted summary values for the individual years. The result is very
similar but not identical, and i am still figuring out what this
actually means. I suspect extracting the value direcly from the input
rasters is a more robust method, but is more demanding computationally.

## 6. Normalize/Standardize Column Names

Automates renaming of columns to maintain consistency across outputs.
Ensures naming conventions are clear and facilitates data visualization
and map production.

## 7. Generate Visualizations

The workflow supports charting of top/bottom 5 values per service,
faceted plots over time, and basin-level change maps. Example plotting
function:

``` r
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

The % of change in NDR at the hydroshed level for 5 different time
points (1992,1995,1998,2001 and 2004) are mapped here: *All values*:
<p div style="display: flex; gap: 10px;">
<img src="output_maps/Hydrosheds_2_1.png" style="margin-right: 10px;"/>
</div>

*Top/bottom values*

<p div style="display: flex; gap: 10px;">
<img src="output_maps/Hydrosheds_2.png" style="margin-right: 10px;"/>
</div>

# Usage

## Running the Workflow

Clone the repository:

    git clone https://github.com/springinnovate/global_NCP.git
    cd notebooks
    Open zonal_stats.Rmd and zonal_stats_hs.Rmd in RStudio and run each section interactively or knit to HTML. to recreate the extraction. Charting is done in visualizations.Rmd

The raster and vector data required to run the analysis are stored in
the shared OneDrive workspace.

# Automating Future Runs

To streamline processing: - Convert reusable steps into **functions**
(e.g., loading rasters, extracting statistics, plotting results). -
Utilize **parameterized reports** for different spatial aggregations. -
Leverage **parallel processing** (`mclapply`) for faster execution on
large datasets.

# Known (current) Limitations

- Implement dynamic service name detection based on raster filenames.

- Improve handling of **multi-country territories** and **small island
  states**.

- Expand statistical options beyond mean values (e.g., quantiles,
  uncertainty metrics).

- Inconsistent raster file naming sometimes requires manual reordering

- Global models with geosharding currently have issues above 60°N

# Future Work

Support for additional services and years Integration with model output
pipelines (e.g., InVEST workflows) Interactive dashboards for results
Simplified config and UI for non-technical users

# License

MIT License Contributors

# Contributors

- **Jeronimo Rodriguez-Escobar** (Primary Author)

For any questions or contributions, feel free to open an issue or submit
a pull request.
