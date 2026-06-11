# Methodology & Analytical Framework

This document details the overarching methodology, spatial framework, and computational architecture used in the Global NCP project to analyze changes in ecosystem service provision between 1992 and 2020.

---

## 1. Data Preparation & Spatial Foundations

Before calculating any changes or identifying hotspots, the underlying spatial and thematic data must be standardized.

### Unit Standardization (Per Hectare)
To ensure that comparisons of ecosystem service provision are meaningful across the globe, all volumetric services (e.g., Nitrogen Export, Sediment Export) are standardized to a **per-hectare** basis. This step corrects for the geometric distortion of raster pixels in unprojected coordinate systems.

*   **Volumetric Variables:** Converted to `Unit / ha` by dividing the raw pixel value by a corresponding pixel area raster (`esa_pixel_area_ha_...`). This calculation is performed *before* any zonal statistics or spatial aggregation occurs.
*   **Ratios & Indices:** Left in their native units (e.g., 0-1 ratios, unitless indices) as area normalization does not apply.

This ensures that all downstream zonal statistics reflect physical densities that are comparable across regions.

### Canonical Master Grid Generation
Due to severe performance and geometry-validity bottlenecks encountered when performing massive spatial joins natively in R (the `sf` package), the creation of the canonical 100 sq km master grid was migrated to a hybrid QGIS-Python workflow.

1.  **Landmass Masking:**
    *   The original global `AOOGrid` and Biomes layer were loaded into a desktop GIS (QGIS).
    *   The WWF Biomes layer was dissolved to create a single global landmass polygon.
    *   A "Select by Location" operation isolated all grid cells intersecting the landmass.
    *   The selection was exported as `landgrid_1.gpkg`.
2.  **Geometry Cleaning:**
    *   To resolve minor topological errors (e.g., self-intersecting rings caused by clipping at coastlines and the dateline), the geometries were validated and cleaned, resulting in `landgrid_1_clean.gpkg`.
3.  **Attribute Enrichment (Python):**
    *   The final stage is handled by `Python_scripts/enrich_grid.py`.
    *   This script performs a highly optimized, centroid-based spatial join to map WWF Biomes, Country, UN/World Bank Regions, and Income Group attributes (from the `ee_correspondence` dataset) onto the grid.
    *   It inherently handles geometry validation (`buffer(0).make_valid()`) and deduplication, outputting the final, analysis-ready `landgrid_1_clean_enriched.gpkg`.
    
*Note: The legacy R script `analysis/prepare_data.qmd` and standalone cleaning utilities like `clean_grid.py` were fully deprecated in v1.3.4 in favor of this Python workflow.*

### Grid Geometry & Reprojection Effects

*   **Geographic vs. Projected Grids:** The original analysis grid is defined in a geographic coordinate system (WGS 84, EPSG:4326). When reprojected into an equal-area system (like Equal Earth, EPSG:8857) for analysis, the shapes of the cells stretch and tilt to preserve their area on a flat map.
*   **Bounding Box vs. True Area:** As a result, measuring the `width` and `height` of a reprojected grid cell's bounding box will **not** yield 10km x 10km. 
*   **Area is the Invariant:** A validation script (`Python_scripts/verify_grid_area.py`) measured the true geometric area of the reprojected grid cells, definitively confirming that each cell has an area of **100 km²** (10km x 10km).

### Data Preparation: Coastal Risk & Protection
The Coastal Risk and Protection datasets are originally provided as high-resolution vector line geometries representing discrete coastal locations. 

To ensure mathematical robustness, a specialized pre-processing workflow was utilized:

1. **Vector Joining & Ratio Calculation:** The script `Python_scripts/coastal_protection_join.py` merges the 1992 and 2020 coastal point layers based on their exact spatial locations. It calculates the absolute differences and proportional risk reduction ratios natively in the vector domain to prevent floating-point interpolation errors.
2. **Rasterization:** The script `Python_scripts/rasterize_coastal.py` safely "burns" these pre-calculated point values into high-resolution continuous rasters.
3. **Grid Extraction:** The resulting coastal rasters are ingested by the standard pipeline. This calculates the mean coastal risk metrics for each 100 sq km grid cell strictly through raster-vector overlap, completely bypassing C-level geometric intersection crashes.

---

## 2. The Dual-Pathway Analysis Structure

Global spatial analysis often suffers from scale artifacts (like the Modifiable Areal Unit Problem). To accurately answer both "What happened globally?" and "Where are the local hotspots?", this project splits the analysis into two parallel workflows.

### Path A: True Regional Trajectories (The "WHAT")
This path is designed to generate summary statistics for large macro-regions (e.g., Biomes, World Bank Regions) bypassing the 100 sq km grid entirely.

*   **Workflow:** The `zonal_stats_toolkit` extracts the base year (1992) and future year (2020) rasters directly to the large regional polygons.
*   **Metrics:** Once the total regional volume/mean is established for both years, we calculate the Absolute Change and Symmetric Percentage Change (SPC) directly from those regional totals.
*   **Why this matters:** It is mathematically impossible to calculate percentage change from a "difference raster." By extracting the true regional baselines first, we ensure our regional percentage changes are sound. We use the outputs of this path for the main text's regional trajectory charts (the "WHAT" section).

### Path B: Grid-Level Change Calculation (The Hotspot Path)
This is the canonical pathway for identifying localized extremes (hotspots) and assessing population exposure. The key distinction here is that **spatial aggregation to the grid precedes differencing**.

*   **Workflow:** 
    1. **Independent Aggregation:** The Python pipeline aggregates the raw 1992 and 2020 rasters to the 100 sq km grid cells independently.
    2. **Grid-Level Differencing:** `analysis/process_data.qmd` computes Absolute and Symmetric Percentage Change (SPC) between the 1992 and 2020 columns for each grid cell.
    3. **Hotspot Extraction:** `analysis/hotspot_extraction.qmd` identifies hotspots (extreme 5% of change) based on these grid cells.
    4. **Synthesis:** `analysis/hotspot_synthesis.qmd` aggregates these localized hotspots to calculate regional enrichment scores, coverage, and affected populations.
*   **Use Case:** This path answers the "WHERE", "WHY", and "WHO". Its regional averages are reserved for the **Annex** due to MAUP/Simpson's paradox artifacts (explained below).

### Addressing Aggregation Divergence (Simpson's Paradox)
Imn some cases, when averaging the grid-level changes (Path B) up to a broad region, a "sign flip" can occur, where regional bar plots display a negative *Absolute Change* but a positive *Symmetric Percentage Change* (SPC) for the same service. This is related to Simpson's Paradox and the Modifiable Areal Unit Problem (MAUP):

*   **Mean Absolute Change** captures the **Systemic Shift**. It is heavily weighted by a few high-volume grid cells. 
*   **Mean Symmetric Percentage Change** captures the **Local Landscape Shift**. Because percentage change treats every 100 sq km community equally regardless of its baseline volume, it highlights widespread but low-intensity dynamics.

A sign flip reveals a specific geographic narrative: The *total volume* of the service in the region is decreasing, but the *spatial footprint* of minor expansions or gains is spreading across a large number of low-baseline cells. This is why we use **Path A** to report the definitive top-level volumetric changes in the main text.

### Validation Analysis: Path C (The Difference Rasters)
To explicitly quantify the difference between the two methodologies, a validation path ("Path C") was implemented. **This path was used strictly for validation to check how much the results differ, not for the final analysis or hotspot extraction.**
*   We used `Python_scripts/batch_raster_diff.py` to create pixel-level "difference rasters" ($T_{2020} - T_{1992}$). 
*   We aggregated these differences to the 100 sq km grid. This confirmed that the results are mathematically consistent at the aggregate level with Path B ("difference of the aggregates"), while allowing us to calculate sub-cell standard deviations. 

---

## 3. Spatial Extraction Architecture

### Architectural Decision: `exactextract` vs `zonal_stats_toolkit`
Through testing and methodological validation, a distinct performance and stability divergence was identified between different zonal statistics engines based on spatial scale and geometry complexity. The project employs a hybrid extraction strategy:

1.  **For Large Regional Groupings (Complex Multipolygons): `zonal_stats_toolkit` (Rasterized)**
    *   **Use Case:** Path A (aggregating global data by World Bank Regions, Income Groups, or Biomes).
    *   **Rationale:** These groupings form sprawling, highly complex multipolygons. Precise fractional extraction tools (`exactextract` / C++ GEOS) cause memory leaks and segmentation faults on these complex geometric intersections. Rasterizing the polygons first (the `zonal_stats_toolkit` approach) bypasses C++ geometry bottlenecks and reduces the problem to highly efficient matrix math.

2.  **For High-Resolution Grids (Simple Polygons): `exactextract` (Exact Fractional)**
    *   **Use Case:** Path B (aggregating global data into the 1.5 million 100 sq km grid cells).
    *   **Rationale:** The grid consists of uniform, simple square geometries. `exactextract` is preferred here, processing through millions of simple shapes without hitting geometry complexity limits.

### Multi-Level Grouping via the 10km Grid (The "Squash" Strategy)
Standard zonal statistics tools typically aggregate by a single geographic boundary attribute at a time (e.g., only by Region, or only by Biome). Running independent extractions for every demographic boundary requires multiple heavy, time-consuming raster passes.

To achieve high analytical flexibility for population exposure without re-running spatial overlays, we use a "Pandas Squash" strategy:
1. We use `exactextract` on the 10km canonical master grid (`landgrid_1_clean_enriched_4326.gpkg`). Because this grid comprises 1.5 million simple squares, it completely avoids the C++ GEOS memory leaks associated with complex multipolygons.
2. The extraction returns a massive dataframe mapping pixels to their parent grid cells, which is instantly grouped in Pandas by multiple spatial attributes simultaneously (`['country', 'region_wb', 'income_grp', 'WWF_biome']`).
3. This instantly "squashes" millions of spatial records down into a single, lightweight CSV containing all possible dimensional intersections. 

**Analytical Advantage:** This allows downstream visualization and reporting scripts to perform complex cross-tabulations on the fly (e.g., isolating "Hotspot Exposure in Low-Income countries within Sub-Saharan Africa") instantly via tabular filtering, delivering a highly responsive analytical tool for stakeholders.

### Modular Extraction and the `fid` Backbone
Because every Path B extraction is performed against the exact same canonical master grid (`landgrid_1_clean_enriched_4326.gpkg`), every output shares an identical, stable `fid` (Feature ID) backbone. 

To add new variables (e.g., a new year, a new modeled service, or a new socioeconomic raster) in the future:
1. Run the Python extraction for the new rasters.
2. The R consolidation script (`analysis/process_data.qmd`) automatically loads the most recent extraction runs.
3. It performs a rapid, geometry-free tabular `left_join` across all tables using the `fid`.

This architecture completely prevents the need to re-run heavy, memory-intensive spatial joins when expanding the analysis.

#### Application: Multi-Level Hotspot Beneficiary Analysis

A key application of this "squash" strategy is the analysis of populations benefiting from ecosystem service hotspots. Standard zonal statistics would require separate, time-consuming runs for every demographic grouping (e.g., by country, then by income group, then by region). The `Python_scripts/extraction_script.py` bypasses this limitation entirely.

1.  **Dual Exposure Pathways:** The script processes two distinct types of population exposure rasters for each hotspot category:
    *   **Hydrological Exposure:** Populations living directly downstream of hotspots (`...downstream_50k_population.tif`).
    *   **Access-Based Exposure:** Populations living within a defined travel time of hotspots (`...within_travel_time_population.tif`).

2.  **Grid-Based Extraction:** For each of these population rasters, `exactextract` is run against the canonical `landgrid_1_clean_enriched_4326.gpkg`. This is highly efficient because the grid contains simple, uniform geometries, avoiding the C++ crashes that occur with complex multipolygons.

3.  **Multi-Dimensional Grouping:** The script is configured to include multiple attribute columns from the grid (`country`, `region_wb`, `income_grp`, `WWF_biome`) in the extraction output.

4.  **Instantaneous Aggregation:** Immediately after extraction, a `pandas.groupby()` operation aggregates the total population sums across all dimensions simultaneously.

This produces a single, lightweight CSV (`exposure_comparison_compiled.csv`) that contains the total exposed population for every possible intersection of hotspot category, exposure type, country, region, income group, and biome. This file is the direct input for the "Multiplier Effect" dumbbell plots and summary tables, allowing for rapid, on-the-fly filtering without ever re-running a spatial process.

### Spatial Alignment and The Fragment Bug
A challenge in multi-stage spatial pipelines is maintaining exact 1:1 row integrity between extracted statistics and the canonical master grid.

*   **The Fragment Bug:** To bypass C++ GEOS bottlenecks during Python zonal statistics extraction, complex master grid cells were exploded into simpler fragments (e.g., `gdf.explode()`). If joined directly to downstream datasets, this causes severe data duplication (e.g., impossible overlapping hotspot counts per cell) and geographic striping.
*   **The Re-aggregation Solution (v1.3.1):** The R pipeline (`process_data.qmd`) implements a recovery step. It uses an `st_intersects` spatial join to trace every fragment back to its original 100 sq km parent cell. It then performs a re-aggregation (`group_by %>% summarise`), collapsing the fragments back together. 

---

## 4. Change Metrics & Hotspot Definition

### Key Analysis Parameters (What is a Hotspot?)
The threshold for identifying hotspots is defined centrally in `HOTS_CFG` (`analysis/hotspot_extraction.qmd`) using the parameter `pct_cutoff = 0.05`.

*   **Relative Extreme:** A cell is cosndierd a "hotspot" if its change sits in the most extreme 5% *within that specific service's own distribution*. It is a ranking label, not an absolute physical threshold. A cell can enter or leave the top 5% even if its raw change isn't huge in absolute terms, simply because it is relative to the rest of the globe.
*   **Comparability:** Comparisons are strictly within-service. A top 5% decline in Service A isn't necessarily comparable in absolute magnitude to a top 5% decline in Service B.
*   **Not Evidence of Cause:** Being a hotspot flags that "this cell's change is unusually large," but it does not inherently prove *why* the value is extreme. To discuss drivers, we use additional robust analyses (LCC attribution and KS profiling).

### Symmetric Percentage Change (SPC)
To address mathematical artifacts where the sign of percentage change differs from absolute change (common when baselines are negative or near-zero), this analysis uses a **symmetric percentage change** calculation (`pct_mode="symm"`). This ensures that the direction of the percentage change always aligns with the absolute difference ($t_1 - t_0$).

**Distribution Limits:** The Symmetric Percentage Change (SPC) metric is bounded between **-200%** (Total Loss) and **+200%** (New Emergence). Consequently, extreme values and clustering at these boundaries, as well as bi-modal distributions (e.g., in Sediment Export), are expected features of the metric rather than data artifacts.

#### Why SPC over Absolute Change for Attribution?
When assessing the relationship between Land Cover drivers and Ecosystem Service declines, **Symmetric Percentage Change (SPC)** is strictly preferred over Absolute Change.

*   **Absolute Change** is inherently biased by the *baseline size* of the local ecosystem. For exsample a large , dense forest that loses just 5% of its area might show a huge absolute drop in Carbon simply because of its initial size. Conversely, a small patch of forest that is 100% destroyed would show a tiny absolute drop. Analyzing Absolute Change creates highly skewed, heteroskedastic outputs that largely just map "where the largest baseline ecosystems are."
*   **Symmetric Percentage Change** normalizes this scale effect. It isolates the *intensity of the ecological shock* relative to the local baseline. This ensures that a severe multi-service decline in a small grid cell is properly recognized as a severe impact, making it mathematically appropriate for correlating against land cover conversion percentages.

### Aggregation Logic: Sum vs. Mean
A question regarding Path B is the comparability of variables aggregated via **sum** (extensive variables like Nitrogen Export) versus those aggregated via **mean** (intensive variables like Risk Indices).

**Justification:**
1.  **Physical Correctness:** It is physically correct to sum total loads and average representative conditions.
2.  **Equal-Area Grid:** The analysis uses the IUCN equal-area grid. Since cell area is constant, $Sum$ and $Mean$ are perfectly proportional ($Sum = Mean \times Area$).
3.  **Mathematical Identity:** For relative metrics used in this analysis (percent change, percentile rankings, KS test statistics), the results are identical regardless of whether sum or mean is used.
4.  **Comparability:** Comparing relative magnitudes of change (e.g., percentage change) strips away the units, allowing valid comparisons between total loads and average indices.

---

## 5. Analytical Modules

### Socioeconomic Profiling (KS Tests)
To understand the socioeconomic context of ecosystem service hotspots (e.g., Population, GDP, HDI), we utilize two-sample Kolmogorov-Smirnov (KS) tests.

**Balanced Sampling Methodology:**
A direct comparison of hotspots (the top/bottom 5% of pixels) against the entire non-hotspot background (the remaining 95%) suffers from severe sample size imbalance and includes pixels undergoing extreme changes in the *opposite* direction. To ensure a fair and stable statistical comparison, the pipeline implements a "median background" sampling strategy:
* Hotspots are compared strictly against the "business-as-usual" median 5% of the landscape (the 47.5th to 52.5th percentiles of change). This isolates the specific socioeconomic profile of extreme decline against typical, stable baseline conditions.

### Land Cover Change Attribution
To explain *why* hotspots occur, we integrate Land Cover Change (LCC) metrics derived from ESA CCI (1992) and C3S (2020) maps.

**Methodology:**
Instead of simple "Net Change" (which masks simultaneous loss and gain), we use an **approach based on square contingency matrices** (Pontius et al., 2014) to calculate:
*   **Gross Loss:** The specific area of natural land lost to transformation.
*   **Gross Gain:** The area of natural land recovered.
*   **Exchange:** Shifts that don't affect the net total but represent dynamic turnover.

These metrics are aggregated to the 100 sq km master grid and overlaid with ES hotspots to quantify the **"Attribution Gap"** (i.e., how much ES decline is directly linked to land conversion vs. degradation). The analysis produces both a single, global attribution map showing the overall footprint of degradation, as well as a series of detailed maps breaking down the specific drivers for each of the 8 individual ecosystem service hotspots.

**Granular Models:**
To move beyond binary "Natural vs. Transformed" analysis, we implement two specific driver models:
1.  **Forest Loss Model:**
    *   **Reclassification:** Maps ESA classes to **Forest** vs. **Non-Forest**. Flooded Trees (classes 160, 170) are mapped to Forest to capture mangrove/swamp forest dynamics.
    *   **Metric:** Tracks Gross Loss of Forest cover.
2.  **Expansion Model:**
    *   **Reclassification:** Maps ESA classes to **Urban**, **Cropland**, and **Other**.
    *   **Metric:** Tracks the specific expansion of Urban and Cropland areas into other land cover types.

**Note on Rangelands:** Logic is updated to explicitly track Forest-to-Grassland transitions as a loss of primary natural cover. Categorizing Grasslands/Shrublands as `Transformed (Rangeland/Pasture)` prevents these critical conversions from being masked as 'Natural-to-Natural' exchange.

---

## 6. Outputs & Visualization

### Output Directory Structure
The project's graphical outputs are organized into specialized subdirectories within `outputs/plots/` (e.g., `maps/`, `drivers/`, `ks/`, `boxplots_unified/`, `signed_bars/`). 

1. **Thematic Modularity:** Plots are strictly grouped by the analytical phase that generated them. For example, all Kolmogorov-Smirnov distribution testing plots live in `ks/`, while spatial attribution heatmaps live in `drivers/`.
2. **Scalability:** Because the analysis crosses 8 ecosystem services, 2 change metrics (Absolute and Symmetric Percentage Change), and 4 geographic grouping levels, a flat directory would result in hundreds of indistinguishable files. 
3. **Asset Portability:** This modular categorization allows researchers to easily isolate and package specific visual assets (e.g., extracting "just the spatial maps" or "just the socioeconomic profiles") for stakeholders and presentations without sifting through unrelated charts.

### Visualization Semantic Rules (Maps)
To maintain a consistent narrative across all presentations and figures, spatial maps of Ecosystem Service change adhere to a strict semantic color rule:
*   **Universal Diverging Scale:** All maps use a diverging color ramp anchored at zero (`midpoint = 0`).
*   **Semantic Meaning:** Red always indicates ecological or social damage (loss of a good service, or increase in a detrimental risk). Green always indicates improvement or healthy service provision.
*   **Sequential vs. Diverging Data:** Even if a regional dataset does not cross zero (e.g., all regions experience a decline), the diverging scale is maintained to preserve the semantic meaning of the colors. 

### Hotspot Rasterization Workflow
While the primary outputs of the hotspot extraction pipeline are compact vector GeoPackages, certain downstream analyses and external visualizations require these hotspots in a continuous raster format.

To ensure perfect 1:1 geometric alignment and leverage C-level performance, the project relies on a native GDAL workflow documented in `scripts/gdal_rasterize_hotspots.sh`.
1. **Reprojection:** The vector file is first reprojected to Equal Earth (EPSG:8857) via `ogr2ogr` to guarantee metric 10km x 10km geometries.
2. **Rasterization:** `gdal_rasterize` is then used to burn the specific attributes into a 10km resolution GeoTIFF using LZW compression.