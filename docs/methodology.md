# Methodology & Analytical Framework

This document details the overarching methodology, spatial framework, and computational architecture used in the Global NCP project to analyze changes in ecosystem service provision between 1992 and 2020.

---

## 1. Data Preparation & Spatial Foundations

Before calculating any changes or identifying hotspots, the underlying spatial and thematic data must be standardized.

### Unit Standardization (Per Hectare)
To ensure that comparisons of ecosystem service provision are meaningful across the globe, all volumetric services (e.g., Nitrogen Export, Sediment Export) are standardized to a **per-hectare** basis. This step corrects for the geometric distortion of raster pixels in unprojected coordinate systems.

*   **Volumetric Variables:** Converted to `Unit / ha` by dividing the raw pixel value by a corresponding pixel area raster (`esa_pixel_area_ha_...`). This calculation is performed *before* any zonal statistics or spatial aggregation occurs.
*   **Ratios & Indices:** Left in their native units (e.g., 0-1 ratios, unitless indices) as area normalization does not apply — the area term cancels in ratio calculations, and applying it to index variables would introduce spurious latitudinal gradients.
*   **Coastal Risk — resolved.** Shore-point outputs converted to 300m raster via `Python_scripts/rasterize_coastal.py` (spatial mean per pixel). InVEST produces: `Rt` (energy reaching shore with habitat), `Rt_nohab_all` (without habitat), `Rt_service` (habitat contribution), `Rt_ratio` (proportional risk reduction). Only `Rt` → **`C_Risk`** and `Rt_ratio` → **`C_Risk_Red_Ratio`** enter the hotspot analysis. `C_Risk` is a per-linear-metre shore metric (e.g. MJ/m) — not area-based — so per-hectare correction does not apply. `C_Risk_Red_Ratio` is dimensionless. `Rt_service` and `Rt_nohab_all` are rasterised but unused in the main analysis.

This ensures that all downstream zonal statistics reflect physical densities that are comparable across regions.

### Aggregation Statistic: Mean for All Variables at Grid-Cell Level

**Decision:** All raster variables are extracted to the 10km grid using the **mean** aggregation statistic (`op_stats: [mean]` in `analysis_configs/services_slim.yaml`). This applies uniformly to volumetric services, ratios, and indices.

**Rationale:** Since all input rasters are already in per-hectare units (sourced from `base_years_ha/`), the mean within each grid cell represents the average per-ha rate or concentration across that 100km² cell. This is:
- A valid, comparable metric across cells regardless of latitude or baseline volume
- Consistent across all service types, eliminating the need to manage mixed `_sum`/`_mean` column naming in downstream R code
- Appropriate for hotspot detection, which ranks relative rates, not absolute volumes

**Implication:** Grid-cell mean values represent per-ha rates, not cell-level totals. Regional totals for volumetric services (e.g., total N export from Sub-Saharan Africa) are computed separately via Path A, which extracts directly from per-ha rasters to large regional polygons without passing through the grid.

**Resolved technical debt** (was open when this section was first written, fixed since): `services_slim.yaml`'s `op_stats` now specifies `[mean]` only, and `Python_scripts/calculate_bitemporal_change.py`'s rename_map no longer contains `_sum` column references.

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
    *   The final stage is handled by `Python_scripts/build_master_grid.py`.
    *   This script performs a highly optimized, centroid-based spatial join to map WWF Biomes, Country, UN/World Bank Regions, and Income Group attributes (from the `ee_correspondence` dataset) onto the grid.
    *   It inherently handles geometry validation (`buffer(0).make_valid()`) and deduplication, outputting the final, analysis-ready `landgrid_1_clean_enriched.gpkg`.

#### Grid Cell Attrition (1.5M to 1.3M cells)
While the canonical master grid initially contains 1,522,073 terrestrial cells, the final number of evaluated cells in the hotspot pipeline drops to approximately 1,302,099. This ~14% reduction is intentional and expected. During the spatial extraction phase, any grid cells that lack complete underlying raster data coverage across all baseline variables (for both 1992 and 2020) are systematically dropped when calculating temporal changes. Common causes for this lack of data include:
*   **Coastal Mismatch:** Raster boundaries from different sources (e.g., ESA CCI vs InVEST models) often handle complex coastlines and small island archipelagos differently, leading to NoData (`NA`) values in those cells.
*   **High Latitude Extents:** Some rasters do not cover extreme polar latitudes.
*   **Modeling Constraints:** Certain biophysical models may fail to resolve valid outputs in edge-case topographies.
By dropping these cells, we guarantee that the final analysis compares a mathematically sound, 1-to-1 footprint of cells that contain complete, valid, non-NA data for all required services across both time periods.

*Note: The legacy R script `analysis/prepare_data.qmd` and standalone cleaning utilities like `clean_grid.py` were fully deprecated in v1.3.4 in favor of this Python workflow.*

### Grid Geometry & Reprojection Effects

*   **Geographic vs. Projected Grids:** The original analysis grid is defined in a geographic coordinate system (WGS 84, EPSG:4326). When reprojected into an equal-area system (like Equal Earth, EPSG:8857) for analysis, the shapes of the cells stretch and tilt to preserve their area on a flat map.
*   **Bounding Box vs. True Area:** As a result, measuring the `width` and `height` of a reprojected grid cell's bounding box will **not** yield 10km x 10km. 
*   **Area is the Invariant:** A validation script (`Python_scripts/verify_grid_area.py`) measured the true geometric area of the reprojected grid cells, definitively confirming that each cell has an area of **100 kmÂ²** (10km x 10km).

### Data Preparation: Coastal Risk & Protection
The Coastal Risk and Protection datasets are originally provided as high-resolution vector line geometries representing discrete coastal locations. 

To ensure mathematical robustness, a specialized pre-processing workflow was utilized:

1. **Vector Joining & Ratio Calculation:** The script `Python_scripts/coastal_protection_join.py` merges the 1992 and 2020 coastal point layers based on their exact spatial locations. It calculates the absolute differences and proportional risk reduction ratios natively in the vector domain to prevent floating-point interpolation errors.
2. **Rasterization:** The script `Python_scripts/rasterize_coastal.py` safely "burns" these pre-calculated point values into high-resolution continuous rasters.
3. **Grid Extraction:** The resulting coastal rasters are ingested by the standard pipeline. This calculates the mean coastal risk metrics for each 100 sq km grid cell strictly through raster-vector overlap, completely bypassing C-level geometric intersection crashes.

---

## 2. The Dual-Pathway Analysis Structure

The two questions this project addresses — *how much did ES change regionally?* and *where is change most concentrated?* — require different aggregation strategies. Path A compares pixels first, then aggregates results to regions. Path B aggregates pixels to grid cells first, then computes change. Using the wrong order for either question introduces systematic bias (detailed below).

Note on the grid: the IUCN AOO 10km grid is an **equal-area** projection, not a lat/lon grid. Every cell covers exactly 100 km² regardless of latitude, making cross-regional comparisons fair without polar distortion.

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

*   **Relative Extreme:** A cell is considered a "hotspot" if its change places it among the 5% of grid cells with the most extreme changes *within that specific service's own distribution*. It is a ranking label (the worst 5% of cells), not an absolute physical threshold. A cell can enter or leave the top 5% even if its raw change isn't huge in absolute terms, simply because it is relative to the rest of the globe.
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
To characterise the socioeconomic context of hotspot cells, we compare distributions of four covariates (population density, GDP, HDI, Gini coefficient) inside hotspot cells against a background of typical stable conditions using two-sample Kolmogorov-Smirnov (KS) tests, complemented by Cliff's Delta (δ) effect sizes.

**Why the median background?**
Hotspot cells are compared against the *median 5%* of each service's change distribution (47.5th–52.5th percentile), not the full non-hotspot set. Reason: comparing against all 95% of non-hotspot cells would include cells at the opposite extreme (large service gains), which would confound the comparison. The median background represents typical, stable conditions — the right reference for "what's distinctive about acute decline?"

**Why Cliff's Delta, not just p-values?**
With ~1.5 million grid cells, even a negligibly small difference between hotspot and background distributions will produce a statistically significant p-value. This doesn't mean the difference is practically meaningful. Cliff's Delta (δ) measures the *probability* that a randomly drawn hotspot cell has a higher covariate value than a randomly drawn background cell, independently of sample size. δ = 0 means complete overlap; δ = ±1 means complete separation.

**Why FDR correction — and what 39/40 means:**
Running 40 tests at once (8 services × 5 covariates) means roughly 2 would appear significant by chance alone at a standard 5% threshold. Benjamini-Hochberg False Discovery Rate correction adjusts the significance bar across all 40 tests together, limiting the proportion of significant results that are likely to be false alarms. That **39 of 40 combinations remain significant after correction** means the socioeconomic signal is robust — the correction barely changed anything. The one non-significant result (Coastal Risk Reduction Ratio × agricultural plot intensity, $p_{adj}$ = 0.48, δ ≈ 0.001) also makes ecological sense: coastal protection hotspots are structurally decoupled from small-plot agricultural landscapes.

### Population Exposure and the Serviceshed Multiplier Effect
To assess the human impact of ecosystem service hotspots, the pipeline quantifies both direct and indirect population exposure, establishing a "Serviceshed Multiplier Effect."

**Methodology:**
1. **Baseline In-Situ Exposure (Local Residents):** We extract the total 2020 population (from GHSL-POP) residing directly within the 100 sq km grid cells identified as ecosystem service decline hotspots.
2. **Connected Beneficiaries (The Multiplier):** We trace exposure beyond the immediate degraded zone using two specialized geospatial delivery pathways:
   * **Hydrological Footprint:** Populations living down-gradient that rely on the upstream landscape for water purification, sediment retention, and flow regulation.
   * **Access-Based Travel Footprint:** Populations living within physical travel distances that rely on local nature for recreation, wild pollination, and access to natural capital.
3. **Compound Risk Analysis:** To evaluate how this exposure behaves under escalating ecological failure, populations are grouped by their localized **Compound Risk** — the number of simultaneous overlapping hotspots in a single cell, recorded as `hotspot_count` in `hotspots_global_pct.gpkg` (ranging from 1 to 8). Compound hotspot subsets are stored in `data/processed/hotspots/hotspot_beneficiaries/` (subfolders: `all hotspots/`, `2 or more overlapping/`, `3 or more overlapping/`, `4 or more overlapping/`).

**Service-weighted vs. distinct population counts**

The intermediate table `hotspot_pop_exposure.csv` stores population stratified by (service × income group × HDI × GDP × GINI). Summing `exposed_population` across all 8 services yields ~5,286 million — a **service-weighted sum** where a person in a cell qualifying as a hotspot for k services is counted k times. This is the correct input for socioeconomic stratification plots (e.g., exposure by income group per service) but is not a count of distinct individuals.

To obtain **distinct people** in at least one hotspot cell, join `hotspot_count ≥ 1` rows from `hotspots_global_pct.gpkg` to `GHS_POP_E2020_GLOBE_sum` from `10k_change_calc.gpkg` and sum once per unique cell: **~3,065 million** (verified 2026-06-22).

**Verified numbers (audited 2026-06-22, branch task/housekeeping)**

All figures derived from: `hotspots_global_pct.gpkg` (`hotspot_count` field) joined to `10k_change_calc.gpkg` (`GHS_POP_E2020_GLOBE_sum`), and beneficiary CSVs in `hotspot_beneficiaries/`.

| Population tier | Filter | Cells | GHS-POP in-situ | Connected (union) | Multiplier |
|---|---|---|---|---|---|
| Any hotspot (1+ services) | `hotspot_count ≥ 1` | 225,113 | **3,065 M** | 7,584 M | **2.5×** |
| Compound (2+ services) | `hotspot_count ≥ 2` | 85,599 | **1,212 M** | 6,011 M | **~5×** |
| High compound (3+ services) | `hotspot_count ≥ 3` | 41,025 | **445 M** | 3,756 M | **~8×** |

Beneficiary source files: `jeronimo_hotspots_all_beneficiaries_2026_06_01_12_09_23.csv` (all hotspots), `jeronimo_2hotspot_beneficiaries_2026_06_02_11_17_05.csv` (2+), `jeronimo_3hotspot_beneficiaries_2026_06_02_10_54_22.csv` (3+).

The `hotspot_count` distribution in the current GeoPackage: 1 service = 139,514 cells; 2 = 44,574; 3 = 19,211; 4 = 12,835; 5 = 7,427; 6 = 1,504; 7 = 47; 8 = 1.

**Analytical Purpose:** 
This framework allows us to test whether intense, compounding environmental crises remain geographically contained. By plotting the exposed populations across escalating compound risk tiers, we measure the multiplier gap between *Local Residents* and total *Connected Beneficiaries*. This mathematically tracks how highly localized environmental degradation cascades into systemic regional vulnerabilities.

### Land Cover Change Attribution
To explain *why* hotspots occur, we integrate Land Cover Change (LCC) metrics derived from ESA CCI (1992) and C3S (2020) maps.

**Methodology:**
Instead of simple "Net Change" (which masks simultaneous loss and gain), we use an **approach based on square contingency matrices** (Pontius et al., 2014) to calculate:
*   **Gross Loss:** The specific area of natural land lost to transformation.
*   **Gross Gain:** The area of natural land recovered.
*   **Exchange:** Shifts that don't affect the net total but represent dynamic turnover.

These metrics are aggregated to the 100 sq km master grid and overlaid with ES hotspots to quantify the **"Attribution Gap"**.

> **Numbers below current as of 2026-07-08** (via `scripts/compute_attribution_true_union.R`, see
> `docs/runbook.md` step 5 and the LCC grid crosswalk prerequisite it depends on). This section
> previously cited a stale 24%/76% split computed before a grid-identity bug was fixed — see
> `analysis/WORKLOG.md`'s 2026-07-07/2026-07-08 entries for the full incident. Verify against the current
> book (`docs/manuscript/chapters/05-drivers-WHY.qmd`) or paper before citing if this file is more than a
> few weeks old.

**Symmetric threshold design (critical for correct interpretation)**

The co-occurrence analysis uses a **symmetric 5%/5% threshold**: ES hotspot cells are defined as the top 5% of SPC change per service; LCC driver hotspot cells are defined as the top 5% of gross conversion magnitude per driver, across **five drivers** (Forest Loss, Cropland Expansion, Urban Expansion, Grassland Loss, Grassland Gain — see Granular Models below). Both thresholds operate within the same 10km equal-area grid.

**34.5% of ES hotspot cells co-occur** with at least one of the five LCC driver hotspots (the union across drivers) — a **strong, highly significant positive association** (odds ratio 12.17 for the union; risk ratios 3.9–36.6 per individual driver), far above what spatial independence would predict.

The **Attribution Gap of 65.5%** means that most ES hotspot cells do **not** co-occur with any extreme (top 5%) LCC driver cell. This must not be read as "65.5% of cells had no land cover change" — it means those cells did not co-occur with the *most intense* conversion cells. Moderate or low-level land cover change may still be present in those cells but below the top-5% threshold. Framed as a *stronger* version of the chapter's thesis, not a weaker one: where land-cover conversion is detected at this intensity, it is a reliable indicator of ES-hotspot co-occurrence — but categorical monitoring alone misses the majority of cases.

This is a **spatial co-occurrence analysis, not causal attribution**. An important structural constraint is that ESA CCI land cover data serves simultaneously as a primary input to InVEST biophysical models and as the basis for the LCC overlay. This endogeneity means the gap cannot be treated as an independent empirical partition between degradation-driven and conversion-driven change.

The analysis produces both a single, global attribution map showing the overall footprint of degradation, as well as a series of detailed maps breaking down the specific drivers for each of the 8 individual ecosystem service hotspots.

**Granular Models:**
To move beyond binary "Natural vs. Transformed" analysis, we implement driver-specific models across five drivers:
1.  **Forest Loss Model:**
    *   **Reclassification:** Maps ESA classes to **Forest** vs. **Non-Forest**. Flooded Trees (classes 160, 170) are mapped to Forest to capture mangrove/swamp forest dynamics.
    *   **Metric:** Tracks Gross Loss of Forest cover.
2.  **Expansion Model:**
    *   **Reclassification:** Maps ESA classes to **Urban**, **Cropland**, and **Other**.
    *   **Metric:** Tracks the specific expansion of Urban and Cropland areas into other land cover types (Urban Expansion and Cropland Expansion, tracked as separate drivers).
3.  **Grassland Model:**
    *   **Metric:** Tracks both Grassland Loss (conversion away from grassland/shrubland) and Grassland Gain (conversion into grassland/shrubland, e.g. from cleared forest) as two separate drivers — see the Rangelands note below for why these are tracked distinctly rather than folded into "Natural-to-Natural" exchange.

**Note on Rangelands:** Logic is updated to explicitly track Forest-to-Grassland transitions as a loss of primary natural cover. Categorizing Grasslands/Shrublands as `Transformed (Rangeland/Pasture)` prevents these critical conversions from being masked as 'Natural-to-Natural' exchange.

---

## 6. Subregional & Filtering Infrastructure

### Pre-computed subsets

After `hotspot_synthesis.qmd` produces the global summary tables, its final chunk
(`regional-subsets-export`) splits `hotspot_area_stats.csv` and
`hotspot_multiservice_stats.csv` into per-group CSV files:

```
data/processed/tables/regional_subsets/
+-- region_wb/
|   +-- hotspot_area_stats_region_wb.csv        (all 7 regions combined)
|   +-- hotspot_area_stats_Sub_Saharan_Africa.csv
|   +-- ...
+-- income_grp/
|   +-- hotspot_area_stats_income_grp.csv
|   +-- ...
+-- WWF_biome/
+-- nev_name/
```

These files need regenerating only when `hotspot_area_stats.csv` itself changes (new
services, updated InVEST inputs, or a changed hotspot threshold). Run the chunk
interactively in RStudio â€” it does not require `plt_long` or the GPKGs.

### R filtering functions (`R/get_hotspots.R`)

Two functions support on-demand filtering beyond the pre-computed groupings:

**`extract_hotspots_by(plt_long, grouping_col, services, exclude_vals, ...)`**

Wraps `extract_hotspots()` across all levels of a grouping column. Returns a named list
(one entry per group value) of `extract_hotspots()` result lists. This formalizes the
inline loop in `hotspot_extraction.qmd` as a reusable function for custom runs.

**`filter_multidim(data, region_wb, income_grp, WWF_biome, country)`**

Filters a data frame (typically `plt_long` or the raw grid `sf`) by any combination of
the four geographic/socioeconomic dimensions. Columns that are `NULL` are ignored, so
partial specifications work naturally. Use this when you need a cross-cut that is not
available in the pre-computed CSVs (e.g., "Sub-Saharan Africa cells classified as Low income"):

```r
subset_df <- filter_multidim(plt_long,
                             region_wb  = "Sub-Saharan Africa",
                             income_grp = "5. Low income")
hs <- extract_hotspots(subset_df, ...)
```

### Regional report template

`docs/templates/regional_report_template.qmd` is a parameterized Quarto document that
produces a self-contained HTML for any single grouping-variable value. It reads from the
pre-computed `regional_subsets/` CSVs (with a fallback to the full table if subsets have
not been generated yet). See `docs/runbook.md` for render commands.

---

## 7. Outputs & Visualization

### Output Directory Structure
The project's graphical outputs are organized into specialized subdirectories within `outputs/plots/` (e.g., `maps/`, `drivers/`, `ks/`, `boxplots_unified/`, `signed_bars/`). 

1. **Thematic Modularity:** Plots are strictly grouped by the analytical phase that generated them. For example, all Kolmogorov-Smirnov distribution testing plots live in `ks/`, while spatial attribution heatmaps live in `drivers/`.
2. **Scalability:** Because the analysis crosses 8 ecosystem services, 2 change metrics (Absolute and Symmetric Percentage Change), and 4 geographic grouping levels, a flat directory would result in hundreds of indistinguishable files. 
3. **Asset Portability:** This modular categorization allows researchers to easily isolate and package specific visual assets (e.g., extracting "just the spatial maps" or "just the socioeconomic profiles") for stakeholders and presentations without sifting through unrelated charts.

### Visualization Semantic Rules (Maps)
To maintain a consistent narrative across all presentations and figures, spatial maps of Ecosystem Service change adhere to a strict semantic color rule:
*   **Universal Diverging Scale:** All maps use a diverging color ramp anchored at zero (`midpoint = 0`).
*   **Semantic Meaning:** As of 2026-07-09, **orange** (`#F07D00`) always indicates ecological or social damage (loss of a good service, or increase in a detrimental risk); **teal** (`#009191`) always indicates improvement or healthy service provision. This replaced an earlier red/green scheme (still visible in any figure rendered before 2026-07-09) that was not colorblind-safe — see `scripts/mapping/make_faceted_maps.R`'s `compute_service_limits()`/fill-scale logic for the current implementation. The semantic *direction* (damage vs. improvement) is unchanged, only the color pair.
*   **Sequential vs. Diverging Data:** Even if a regional dataset does not cross zero (e.g., all regions experience a decline), the diverging scale is maintained to preserve the semantic meaning of the colors. 

### Hotspot Rasterization Workflow
While the primary outputs of the hotspot extraction pipeline are compact vector GeoPackages, certain downstream analyses and external visualizations require these hotspots in a continuous raster format.

To ensure perfect 1:1 geometric alignment and leverage C-level performance, the project relies on a native GDAL workflow documented in `scripts/gdal_rasterize_hotspots.sh`.
1. **Reprojection:** The vector file is first reprojected to Equal Earth (EPSG:8857) via `ogr2ogr` to guarantee metric 10km x 10km geometries.
2. **Rasterization:** `gdal_rasterize` is then used to burn the specific attributes into a 10km resolution GeoTIFF using LZW compression.
