# Worklog — Global NCP Hotspots (v1.3.0)

## Project Overview & Goals

**Goal:** Quantify global change in multiple ecosystem services (ES) at ~10-km resolution over 1992–2020, identify **hotspots** of concerning change, and attribute those changes to environmental and socioeconomic drivers.

**Key Objectives:**
1.  Robust spatial extraction of ES changes bypassing Modifiable Areal Unit Problem (MAUP) artifacts.
2.  Identification of ES hotspots using Symmetric Percentage Change (to handle zero-baselines and capture local vulnerability).
3.  Integration of Land Cover Change (LCC) metrics to attribute ES decline to Land Conversion vs. Degradation.
4.  Socioeconomic characterization of hotspots via Kolmogorov-Smirnov (KS) tests.

## Current State: Version 1.3.0 (Synthesis & Interpretation Phase)

**Status:** The core pipeline architecture is mathematically validated and finalized. The project has shifted entirely from building the data infrastructure to generating final insights and interpreting results.

**Active Focus & The Final Wrap-Up Plan:**
*   **Metric Justification & Consolidation:** We officially use **Relative (Symmetric Percentage) Change** to define hotspots and run socioeconomic KS tests. Absolute change remains in the regional bar plots for global volume accounting.
*   **Land Cover Change (LCC) Interpretation:** Reviewing the outputs of the "Drivers of Change" chunks in `hotspot_extraction.qmd`. What percentage of ES hotspots directly overlap with the top 5% of "Forest Loss" or "Urban Expansion" cells? This defines our "Attribution Gap."
*   **Socioeconomic Interpretation (KS Tests):** Reviewing KS Test heatmaps and Cliff's Delta plots. Are hotspots of decline systematically occurring in poorer areas? Or are they driven by rapid development in wealthier areas?
*   **Presentation Assembly:** Moving final exported plots into the "Living PowerPoint" for presentation to co-authors. Drafting a 1-page "Key Takeaways" document summarizing attribution and KS findings.

## Reference Information
*   **Environment Notes:** Local machine: Lenovo (Windows 11) | Remote: lilling (VS Code Remote SSH) | AI assistant: Gemini Code Assist / Copilot
*   **Active Entry Points:** `analysis/process_data.qmd`, `analysis/hotspot_extraction.qmd`, `analysis/KS_tests_hotspots.qmd`
*   **Known Issues / Gotchas:** Hotspot rules (loss vs gain services) must remain centralized in `HOTS_CFG`. Be careful not to mix interpretive direction (good/bad change) with magnitude summaries.

---

## Chronological Log (Newest to Oldest)

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
*   **Hotspot Intensity & Multi-service Analysis Fixes:** Updated `hotspot_intensity.qmd` to calculate against total area and implemented Enrichment metric. Fixed setup chunks and alphabetical ordering in `hotspot_multiservice.qmd`.

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
