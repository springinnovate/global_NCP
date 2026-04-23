# Worklog — Global NCP Hotspots (v1.3.1)

## Project Overview & Goals

**Goal:** Quantify global change in multiple ecosystem services (ES) at ~10-km resolution over 1992–2020, identify **hotspots** of concerning change, and attribute those changes to environmental and socioeconomic drivers.

**Key Objectives:**
1.  Robust spatial extraction of ES changes bypassing Modifiable Areal Unit Problem (MAUP) artifacts.
2.  Identification of ES hotspots using Symmetric Percentage Change (to handle zero-baselines and capture local vulnerability).
3.  Integration of Land Cover Change (LCC) metrics to attribute ES decline to Land Conversion vs. Degradation.
4.  Socioeconomic characterization of hotspots via Kolmogorov-Smirnov (KS) tests.

## Current State: Version 1.3.1 (Final Synthesis & Report Generation)

**Status:** The core pipeline architecture is mathematically validated, cleaned, and finalized. The project has shifted entirely from building the data infrastructure to generating final insights, interpreting results, and drafting the manuscript.

**Active Focus & The Final Wrap-Up Plan:**
*   **Geographic Clustering:** Finalizing the geographic narrative of "Compound Risk" (Hotness) and "Disproportionate Burden" (Relative Intensity) using the consolidated `hotspot_synthesis.qmd` pipeline.
*   **Land Cover Change (LCC) Interpretation:** Reviewing the outputs of the "Drivers of Change" chunks in `hotspot_extraction.qmd` to define our "Attribution Gap" (conversion vs. degradation).
*   **Socioeconomic Interpretation (KS Tests):** Interpreting KS Test heatmaps and Cliff's Delta plots to profile the socioeconomic context of extreme ES decline.
*   **Presentation & Handoff:** Sharing final exported plots and datasets with co-authors via OneDrive, and drafting the final Key Takeaways and methodology sections.

## Key Challenges & Architectural Solutions (For Final Report)

This section highlights the major technical and methodological hurdles overcome during the pipeline's development, serving as a direct outline for the Methods paper.

*   **The Fragment Bug & Spatial Alignment:** *Challenge:* Bypassing C++ GEOS bottlenecks by exploding complex multipolygons into 1.67M fragments caused striping and duplicated data. *Solution:* Reverted to a mathematically safe `st_intersects` spatial joining and re-aggregation process (`group_by %>% summarise`), collapsing fragments back into pristine 10km parent cells to perfectly align data (v1.3.1).
*   **Spatial Extraction Scaling:** *Challenge:* `exactextract` memory leaks and C++ segmentation faults when processing massive, jagged regional multipolygons (e.g., Biomes). *Solution:* Adopted a "Hybrid Extraction" architecture—using `exactextract` for simple grids (10km) and rasterized `zonal_stats_toolkit` for complex regional polygons.
*   **Simpson's Paradox & MAUP:** *Challenge:* Observing "sign flips" where a region showed negative Absolute Change but positive Percentage Change. *Solution:* Documented the distinct spatial narratives. Mean Absolute Change captures systemic volume shifts (weighted by huge baselines), while Mean Symmetric Percentage Change captures widespread landscape footprint shifts.
*   **Zero-Baselines & Scale Bias:** *Challenge:* Absolute change is heavily biased by the size of the baseline ecosystem, and standard percentage change fails on zero-baselines. *Solution:* Transitioned to **Symmetric Percentage Change (SPC)** to normalize the data, capturing the true *intensity* of ecological response for Land Cover Change attribution.

## Reference Information
*   **Environment Notes:** Local machine: Lenovo (Windows 11) | Remote: lilling (VS Code Remote SSH) | AI assistant: Gemini Code Assist / Copilot
*   **Active Entry Points:** `analysis/process_data.qmd`, `analysis/hotspot_extraction.qmd`, `analysis/hotspot_synthesis.qmd`, `analysis/KS_tests_hotspots.qmd`
*   **Known Issues / Gotchas:** Hotspot rules (loss vs gain services) must remain centralized in `HOTS_CFG`. Be careful not to mix interpretive direction (good/bad change) with magnitude summaries.

---

## 🛠️ Merged Worklog: Zonal Stats Toolkit (Pre-Integration)

*This section consolidates the historical worklog from the `zonal_stats_toolkit` repository. Moving forward, all notes for both the Python extraction engine and the R/Quarto synthesis pipeline will be tracked in this single document.*

### Key Methodological Milestones (Toolkit)
*   **Spatial Dissolve vs Tabular Grouping:** Proved that geographic dissolves prior to extraction cause massive OOM errors and slowdowns. The optimized design uses a high-res grid and tabular aggregations post-extraction.
*   **Pollination Discrepancy:** Identified that $\text{Mean}_{2020} - \text{Mean}_{1992}$ diverges from $\text{Mean}_{\Delta}$ for Pollination due to NoData mask misalignments (shifting agricultural footprints).
*   **Legacy vs Optimized Validation:** Achieved 0.9975 Pearson Correlation between legacy GDAL rasterize and the optimized `exactextract` pipeline. Variance is strictly due to boundary-pixel handling (`ALL_TOUCHED` artifacts). Optimized pipeline safely calculates exact fractional overlap.
*   **Raster Conversion Overhaul:** Refactored `convert_to_ha.py` to use `rasterio` and `WarpedVRT` in small blocks (sequential `max_workers=1` with `BIGTIFF=YES`), definitively resolving memory and write failures on global rasters.

### Chronological Toolkit Notes (Jan - Mar 2026)
*   **Mar 24:** Visualization refactor for bitemporal difference plots. Switched to SEM for error bars and filtered bottom 10% micro-states to prevent variance skewing. Developed `append_ratios.py` for missing data.
*   **Mar 20:** Runner config enhancements (skip jobs).
*   **Mar 13 (Bi-Temporal Math):** Implemented `calculate_bitemporal_change.py` using `osgeo.ogr` directly on the GPKG. This calculates Absolute and Symmetric Percentage Change (SPC) via raw SQL updates, explicitly bypassing memory-intensive `geopandas` operations and `sqlite3` limitations to prevent OOM crashes on global grids.
*   **Mar 13 (Validation):** Built validation framework `compare_gpkg_columns.py` (NRMSE metrics). Enforced runner determinism.
*   **Jan 28-29:** Coastal protection vector attribute integration (`Rt`, `Rt_ratio`).
*   **Jan 20-22:** Disk space management, permission fixes, and visualization layout refinements.
*   **Jan 12:** Docker execution bypassing host permissions, fixing NaN handling, and output column filtering.

---

## Chronological Log (Newest to Oldest)

### 2026-04-23
*   **Workspace Integration**: Configured a VS Code Multi-root Workspace bringing `global_NCP` and `zonal_stats_toolkit` side-by-side for unified development.
*   **Documentation Unification**: Merged the historical worklog from the `zonal_stats_toolkit` repository into the central `WORKLOG.md` to officially centralize project tracking.
*   **Aesthetic Unification & Fixes**: Applied universal canonical color palettes for Biomes, WB Regions, and Income Groups across the `global_NCP` and `zonal_stats_toolkit` plotting scripts. Fixed exact string matching issues for Income Groups with numeric prefixes.
*   **Contextual Mapping**: Developed `generate_context_groupings_map()` to produce a 4-facet overview map of all geographic groupings, providing a clean visual baseline for the slide deck introduction.

### 2026-04-13
*   **Data Alignment Bugfix:** Resolved a fatal desynchronization bug in `hotspot_synthesis.qmd` where missing `fid` identifiers in the master attribute grid caused silent Quarto crashes during attribute joins.
*   **Technical Debt Documentation:** Formally documented the "Fragment Bug" spatial join bypass as technical debt across `process_data.qmd`, `README_Methodology.md`, and `README_pipeline.md`. Outlined the V1.4.0 plan to replace it with a robust `orig_fid` tabular join.
*   **Data Packaging:** Created a lean 2.2GB final data archive (`global_ncp_data_archive.tar.gz`) for co-author handoff. It strictly includes the analysis-ready `processed/` datasets, `outputs/` plots, `vector_basedata/` grids, and a standalone `README`. Excluded all raw/intermediate raster data to ensure easy sharing.
*   **Presentation Strategy:** Outlined the final slide deck structure for co-authors, focusing heavily on Compound Risk (Hotness), Disproportionate Burden (Relative Intensity), and the "Attribution Gap" (Land Conversion vs. Degradation).
*   **Housekeeping:** Cleaned up residual Git artifacts and removed deprecated scratch scripts.

### 2026-04-10
*   **Final Synthesis & Key Takeaways:** Successfully consolidated Intensity, Share, Relative Intensity, and Multi-service "Hotness" (Compound Risk) into a single, bulletproof pipeline (`hotspot_synthesis.qmd`).
*   **Codebase Grooming:** Officially deprecated `hotspot_intensity.qmd` and `hotspot_multiservice.qmd`, removed dead code in Python utilities, and prepared the repository for co-author handoff via secure, read-only OneDrive sharing.
*   **Visualization Polish:** Re-engineered compound risk and relative intensity bar charts to automatically loop over all canonical groupings, generating presentation-ready outputs for the final report.

### 2026-04-08
*   **Pipeline Fixes (The Fragment Bug):** Discovered that Python's `gdf.explode()` was fragmenting the 1.5M grid cells into 1.67M jagged pieces to bypass GEOS bottlenecks. This caused severe striping (dropped cells) and impossible hotspot counts (up to 180) due to duplicated data.
*   **Spatial Join & Re-aggregation Patch:** Implemented a robust `st_intersects` spatial join and re-aggregation (`group_by %>% summarise`) in `process_data.qmd`. This mathematically collapses all fragments back into their pristine 10km parent cells, ensuring perfect 1:1 data alignment. Striping is completely eliminated, and max hotspot counts are strictly capped at 8 (the total number of services). Stable extraction pipeline version tagged as `v1.3.1`.
*   **V2 Technical Debt Documentation:** Formalized the "V2 Simplification Plan" to use `orig_fid` (preserved from Python prior to explosion) to bypass spatial joins entirely in future analysis updates (`v1.4.0`).

### 2026-03-24
*   **LCC Driver Correlation Improvements:** Upgraded Land Cover Change (LCC) vs Ecosystem Service scatterplots to use 2D density heatmaps (`geom_bin2d`) with a logarithmic viridis scale to solve massive overplotting. Restructured plots to a faceted 3x3 canonical layout and removed deprecated `USLE`/`N_retention` metrics.
*   **Methodological Documentation (Absolute vs. Percentage Change):** Documented the critical decision to use Symmetric Percentage Change (SPC) rather than Absolute Change for attributing ES declines to drivers. Absolute change is heavily biased by the baseline ecosystem size (e.g., a 5% loss in a massive forest yields a larger absolute drop than a 100% loss in a tiny forest). SPC properly normalizes the data to reveal the *intensity* of the ecological response relative to the local baseline.
*   **KS Socioeconomic Analysis Validation:** Verified the `KS_tests_hotspots.qmd` pipeline. Confirmed the successful execution of balanced sampling (`comparison_mode = "median"`) to correct the 5% vs 95% class imbalance. Updated KS Heatmaps and Cliff's Delta plots to enforce the canonical 3x3 service ordering for presentation consistency.
*   **Hotspot Visual Polish:** Unified boxplot aesthetics in `hotspot_extraction.qmd` to remove arbitrary color maps, using a consistent clean `gray95` fill. Added horizontal, ranked (Top 10/Bottom 10) boxplots for country-level aggregations to drastically improve legibility.
*   **Automated Faceted Mapping:** Developed `make_faceted_maps.R` to fully automate the generation of spatial maps across 4 groupings (World Bank Region, Income Group, Biome, Country). Implemented a dynamic Cartography Rule Engine for automatic color ramp selection (diverging/sequential, goods/damages) and utilized `patchwork` for complex multi-scale layout stitching. Applied Equal Earth projection (`EPSG:8857`) and 1st/99th percentile outlier trimming to ensure high-quality visualization of absolute change.
*   **Documentation Refinement (Conceptual Framing):** Harmonized `README_Methodology.md` with explicit definitions of a "hotspot" (framing it as a *relative extreme* ranking label rather than an absolute threshold or evidence of cause). This analogy (the "marathon finisher") will directly support the framing of the final methods paper.

### 2026-03-20
*   **Architectural Validation (Spatial Extraction Strategies):** Ran a test using `exactextract` in Python for large regional groupings (Biomes/WB Regions) by exploding them into 85,000 fragments. It ran for over 33 hours without finishing. Definitively proved `exactextract` is unscalable for massive regional groupings. Permanently adopted hybrid approach (`zonal_stats_toolkit` for regions, `exactextract` for 10km grids). Drafted open source feature request for C++ level `groupby`.
*   **Methodological Pivot (True Regional Baselines):** Configured regional base-year extraction to strictly bypass the 10km grid. By summarizing directly from the per-hectare rasters to the large spatial units, we bypass MAUP and grid-level division-by-zero artifacts.
*   **Pipeline & Cache Fixes:** Resolved Quarto caching trap in `KS_tests_hotspots.qmd` and re-enabled hotspot export chunks.
*   **Housekeeping:** Archived legacy QA/QC validation scripts.

### 2026-03-18
*   **Difference Analysis Pipeline Fixes & Completion:** Resolved persistent C++ `Segmentation fault` crashes in exactextract backend caused by microscopic topological errors. Implemented aggressive pre-processing. Completed "Mean of Differences" (Path C) extraction for regional groupings. Created `aggregate_yaml_outputs.py` for mathematical recombination.

### 2026-03-16
*   **Bi-temporal Change Validation:** Successfully cross-validated Symmetric Percentage Change (SPC) calculations between the R pipeline (`process_data.qmd`) and the Python SQLite pipeline (`calculate_bitemporal_change.py`). Both produced mathematically identical results. Confirmed R pipeline as primary workflow due to in-memory speed.

### 2026-03-10
*   **Data Consolidation (Path B):** Finalized primary base-year services dataset (`interim/10k_grid_services_base.gpkg`) using per-hectare corrected base year rasters. Bumped analysis version to **v1.2.1**.
*   **Difference Analysis (Path C):** Completed `summary_pipeline_landgrid.py` run on hectare-normalized difference rasters to establish the "Mean of Differences" dataset. Created `analysis/Consolidation.qmd` to load and validate outputs from Path B vs Path C.

### 2026-03-09
*   **Final Base Year Extraction (Per Hectare):** Initiated fresh run of summary statistics extraction for 1992 & 2020. Corrected volumetric variables to "per hectare" basis for global consistency. Configured `services_diff_ha.yaml` for Path C analysis.

### 2026-03-04
*   **V2 Pipeline Optimization:** Implemented caching for `plt_long`, added fallback logic for LCC driver column names, optimized GPKG export, and corrected export loops in `hotspot_extraction.qmd`. Synchronized continent/biome filters in KS tests.

### 2026-03-03
*   **V2 Pipeline Debugging:** Identified and fixed corrupt `grid_fid` issue causing NAs in `10k_change_calc_v2.gpkg`. Launched full V2 hotspot extraction. Created `compare_hotspots_v1_v2.qmd` to compute Jaccard Index overlaps between methodologies.

### 2026-02-27
*   **V2 Pipeline Implementation:** Created `analysis/process_zonal_stats_v2.qmd` to calculate Symmetric Percentage Change. Updated extraction/KS notebooks to be version-aware (`input_gpkg` and `output_suffix` params).

### 2026-02-24
*   **Refined Granular LCC Workflow:** Created `LC_change_preparation.qmd` for raw ESA/C3S extraction and 9-class reclassification. Updated `LC_change_granular.qmd` and removed testing limits. Launched 48h global extraction in `screen` session. Created `viz_granular_lcc.qmd`.

### 2026-02-20
*   **Granular LCC Analysis:** Created `LC_change_granular.qmd` to implement specific driver models (Forest Loss, Expansion) using `diffeR` metrics. Parameterized input GPKG path in `hotspot_extraction.qmd` for workflow flexibility.

### 2026-02-17
*   **LCC Pipeline Finalization:** Fixed `fid` vs `grid_fid` conflict in `LC_change.qmd`. Fixed grouping aggregation logic to generate `lcc_summary_by_group.csv`. Implemented chunked processing (50k cells/chunk).

### 2026-02-13
*   **LCC Integration & Documentation:** Validated `hotspot_extraction.qmd` logic for LCC overlap (Drivers of Change). Updated documentation to formally include the LCC pipeline and `diffeR` methodology.

### 2026-02-11
*   **Strategic Narrative / Pitch:** Defined the "Drivers of Change" strategy to attribute hotspots to Land Conversion (via `diffeR`) versus Degradation/Intensification.

### 2026-02-10
*   **Land Cover Change Integration:** Shifted focus to attributing hotspots. Created `analysis/land_cover_change.qmd` to compute binary transitions (Natural/Transformed) from ESA 300m maps.

### 2026-02-04
*   **KS Analysis Finalization & Methodology Refinement:** Optimized data pivoting in KS tests, implemented "signed power" transformations for plots, centralized configurations, refined groupings (removed `region_un` and `continent`), and documented Sum vs. Mean aggregation logic.

### 2026-02-02
*   **Hotspot Intensity & Multi-service Analysis Fixes:** Updated `hotspot_intensity.qmd` to calculate against total area and implemented Relative Intensity metric. Fixed setup chunks and alphabetical ordering in `hotspot_multiservice.qmd`.

### 2026-01-31
*   **Refactoring and Scope Refinement:** Initiated refactoring of `Consolidation.qmd` into `prepare_data.qmd` and `process_data.qmd`. Focused groupings on `income_grp`, `region_wb`, and `WWF_biome`.

### 2026-01-21
*   **Technical Issue Resolution:** Confirmed "fat tail" and bi-modal distributions are inherent properties of Symmetric Percentage Change (SPC). Investigated persistent sign flips (MAUP artifact). Added "Hotspot Area Analysis".

### 2026-01-19
*   **Ratio Calculations:** Created `calculate_ratios.py` to generate reliable sediment and nitrogen retention ratios with parallelized, tiled processing and `BIGTIFF=YES` support. Added automated statistical checks.

### 2026-01-16
*   **Repository Cleanup:** Archived legacy R zonal stats workflows (`zonal_stats.qmd`, `asign_ids_grid.qmd`).

### 2026-01-09
*   **Difference Rasters:** Implemented `batch_raster_diff.py` to calculate 2020-1992 difference rasters to support the transition to `zonal_stats_toolkit`.

### 2026-01-07
*   **Sign Flip Resolution:** Resolved absolute vs percent polarity issues by centralizing logic and normalizing service names.

### 2026-01-06
*   **Bug Fixes & Handoff:** Fixed `c_fid` drop bug in `Consolidation.qmd`, normalized service names, bumped to v1.0.1. Extracted pipeline overview to `README_pipeline.md`.

### 2026-01-05
*   **AI Context Migration:** Created `ai_context.md`, migrated to AI assistant (Copilot / Gemini).
