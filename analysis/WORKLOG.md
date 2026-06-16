# Worklog — Global NCP Hotspots (v1.3.4)

### 2026-06-16
*   **Repository Restructuring & Cleanup:** Conducted a major repository cleanup to align with FAIR principles and good industry practices. Transitioned the project from a standard R package structure to a broader reproducible research project structure, acknowledging its evolution into a large-scale analytical pipeline.
*   **Data Consolidation:** Unified data directories, ensuring that `C:\projects\global_NCP\data` contains the most recent canonical data, while deprecating redundant `home/` directories.
*   **Git Cleanup:** Removed a large number of temporary and untracked files from the `home/` directory (e.g., temporary Rscript runs and libloc files) from the git repository to ensure a clean and reproducible state.

### 2026-06-12
*   **Population Exposure Milestone:** Calculated the total 2020 GHSL population captured across the 1.3 million evaluated 10km grid cells (7,855,519,292 people).
*   **Near-Universal Exposure:** Verified that the 7.6 Billion "Connected Beneficiaries" represent **96.7%** of the evaluated global population. This massive share confirms that almost the entire global population is connected to at least one ecosystem service loss hotspot via downstream hydrological pathways or travel-access footprints.

### 2026-06-11
*   **Methodological Optimization (Population Exposure Extraction):** Bypassed the single-attribute limitation of `zonal_stats_toolkit` for the multi-level hotspot beneficiaries analysis. 
*   **Multi-Dimensional Slicing:** By extracting raw population data directly to the 1.5 million 10km grid cells (`landgrid_1_clean_enriched_4326.gpkg`) using `exactextract` (with `strategy="raster-sequential"` to prevent GEOS C++ crashes) and subsequently grouping in Pandas by `['country', 'region_wb', 'income_grp', 'WWF_biome']` simultaneously, we squash millions of rows into a lightweight, highly flexible CSV.
*   **Analytical Power Unlocked:** This structural decision allows downstream R scripts to effortlessly filter and cross-tabulate complex intersections (e.g., "Exposure in Low-Income countries within Sub-Saharan Africa") on the fly, without needing to re-run expensive spatial intersections.

### 2026-06-08
*   **Ground-Truth Narrative Audit:** Conducted a comprehensive, data-driven audit of all high-level claims in the synthesis chapters and manuscript draft using exact values from `hotspot_area_stats.csv` and the attribution scripts.
*   **Narrative Corrections:** Purged several "echo chamber" inaccuracies in the text. Verified that Lower-Middle Income countries face the highest relative intensity (1.19x absolute, 1.6x OECD), Latin America and East Asia are the true regional epicenters, and Mangroves are the most severely impacted biome (nearly 5x expected intensity). Excluded micro-states (area < 10,000 sq km) from country-level rankings, revealing South Korea, Jamaica, Malaysia, and Guatemala as top intensity spots.
*   **Hotspot Definition Refinement:** Clarified manuscript language to explicitly define hotspots based on the "extreme 5% of relative change values (Symmetric Percentage Change)", correctly identifying approx. 250,000 unique cells with at least one hotspot.
*   **Output Audit Artifact:** Established a permanent logging mechanism (`outputs/audit_summary.txt`) to maintain a paper trail of the core ground-truth statistics for peer review and manuscript defense.

### 2026-06-04
*   **Pipeline Robustness & Zombie Data Fix:** Identified and resolved a critical bug where `process_data.qmd` ingested a stale, misaligned coastal GPKG because the file loading was hardcoded to grab the top 3 files by date. Updated the script to dynamically load *all* GPKGs present in `summary_pipeline_workspace_ha`.
*   **Coastal Extraction Canonical Path:** Restored `analysis_configs/c_protection_synth.yaml` to point to the archived coastal risk rasters (`Rt_1992.tif`, etc.). Confirmed that calculating ratios natively on vectors and *then* rasterizing them is the only stable path, bypassing C-level crashes.
*   **Dateline Artifact Resolution:** Added `sf::st_wrap_dateline()` with `DATELINEOFFSET=180` to the final export step of `process_data.qmd` to prevent horizontal tearing artifacts when rendering the EPSG:4326 output in QGIS.
*   **Future-Proofing the Pipeline:** Refactored year-detection regex from hardcoded `"1992|2020"` to dynamic `"[0-9]{4}"`. Added prominent `[MANUAL UPDATE REQUIRED]` templates directly into the `process_data.qmd` script to guide future users on exactly how to drop specific years (for multi-year comparisons) or add new variables without breaking the analysis.

### 2026-06-03
*   **Geometry Crashes Finally Conquered:** After a grueling two-week struggle involving `GEOSException` crashes, memory leaks, and exploded geometries, we have finally established a mathematically sound and highly performant vector-extraction workflow.
*   **The Breakthrough:** The root cause of the crashes in Python/GEOS was isolated to a small number of malformed "poison polygons" during the C-level EPSG:4326 reprojection phase.
*   **Solution Implementation:** We consolidated the grid creation into a single, robust Python script (`build_master_grid.py`). It uses chunked reprojection (processing the 1.5M cell grid in blocks of ~7500). If a chunk fails the fast C-level reprojection, the script falls back to an isolated row-by-row projection, safely discarding the few mathematically impossible geometries while preserving the rest.
*   **Pipeline Success:** `summary_pipeline_landgrid.py` was successfully run against this new master grid (`landgrid_1_clean_enriched_4326.gpkg`) for both the 1992/2020 Services and the Socioeconomic Beneficiaries. The pipeline finished in ~13 minutes with zero crashes and zero duplicated/exploded fragments.
*   **Housekeeping:** Deleted redundant scratch scripts (e.g., `clean_grid_4326.py`, deprecated in favor of `build_master_grid.py`).
*   **R Consolidation:** `process_data.qmd` was overhauled to simply merge the pristine output GPKGs from the Python workspace based on the reliable `fid`.

### 2026-06-02
*   **Vector Data Enrichment Pipeline Stabilized:** After being blocked for over a week by intractable geometry and performance issues in the R-based `prepare_data.qmd` script, a robust Python-based solution has been successfully developed and executed.
*   **Problem:** The original R script was unacceptably slow and consistently failed with obscure `GEOSException` errors when performing spatial joins on the 1.5M-cell grid.
*   **Solution:** A new script, `Python_scripts/enrich_grid.py`, was created to handle this critical data preparation step.
    1.  **Performance:** The initial polygon-intersection approach was too slow. The script was re-engineered to use a much faster and more stable **centroid-based spatial join**. This reduced processing time from hours to minutes.
    2.  **Robustness:** Iteratively debugged a series of `KeyError` and `ValueError` exceptions related to inconsistent column names (`WWF_BIOME` vs. `WWF_biome`, `country` vs. `nev_name`) and internal `geopandas` state (`index_right` conflicts).
    3.  **Final Output:** The script successfully produced `landgrid_1_clean_enriched.gpkg`, a clean, attribute-rich vector grid containing all necessary biome and country/regional information. This file now serves as the canonical input for the main zonal statistics pipeline, unblocking all downstream analysis.

---

### 2026-05-27 (cont. 7)
*   **Final Strategic Pivot & Course Correction:** The `GEOSException: ...closed linestring` error continues to be completely intractable in the vector-based Python pipeline (`summary_pipeline_landgrid.py`), even with multiple aggressive cleaning patches. This confirms that the vector file's geometry issues are too severe to be reliably fixed on-the-fly in a multiprocessing environment.
*   **Definitive Solution:** The project is now fully reverting to the **hybrid raster-vector workflow** that was prototyped on 2026-05-26. This is the only robust path forward.
    1.  **Deprecate Vector Pipeline:** The `summary_pipeline_landgrid.py` script and its associated vector-based logic are now considered deprecated. All efforts will focus on the raster-based workflow.
    2.  **Create Zone Raster:** The `analysis/create_zone_raster.R` script provides the stable "zone" input needed for Python.
    3.  **Implement Raster Pipeline:** A new configuration (`analysis_configs/services_raster.yaml`) has been created to drive `summary_pipeline_rasterzones.py`. This script performs all zonal statistics using the zone raster, completely avoiding vector geometry processing in Python and thus eliminating the `GEOSException`.
    4.  **Simplify R Consolidation:** The `analysis/process_data.qmd` script has been overhauled. It no longer needs to perform complex spatial joins or aggregations to fix "exploded" fragments. It now reads the clean CSV output from the raster pipeline and performs a simple, fast `left_join` by `fid` against the master grid.
*   This new workflow is not only more robust and error-free but also significantly simpler and faster. The `README.md` has been updated to reflect this as the new canonical procedure.

---

### 2026-05-27 (cont. 6)
*   **Python Pipeline Failure (`GEOSException` Persists):** The `closed linestring` error continues to occur in the `zonal_stats` worker process during reprojection, even after the `buffer(0)` patch was applied.
*   **Root Cause Analysis:**
    1.  This confirms that this dataset contains exceptionally stubborn geometry invalidities.
    2.  The file I/O cycle where the main process writes a temporary GeoPackage and the worker process reads it is the most likely source of re-introducing these subtle errors.
    3.  The single `buffer(0)` call in the worker is insufficient. It may even be creating empty or invalid sliver polygons from highly malformed inputs, which are not being filtered out before the `to_crs()` call.
*   **Resolution:**
    1.  **Aggressive Just-in-Time Cleaning:** The `zonal_stats` function in `summary_pipeline_landgrid.py` has been patched with a much more robust cleaning sequence. It now performs a `buffer(0).make_valid()` and then explicitly filters out any empty or near-zero-area geometries that may have been created. This mirrors the extensive cleaning performed in the main process and ensures the data is as clean as possible immediately before the sensitive reprojection step. This should finally resolve the recurring geometry exceptions.

---

### 2026-05-27 (cont. 5)
*   **Python Pipeline Failure (`GEOSException`):** The pipeline is now running past the `FileNotFoundError` but fails during zonal statistics with a `shapely.errors.GEOSException: IllegalArgumentException: Points of LinearRing do not form a closed linestring`.
*   **Root Cause Analysis:**
    1.  This error occurs during the `gdf.to_crs()` reprojection step inside the `zonal_stats` function.
    2.  This is a classic geometry validity issue. Although the `main()` function performs extensive cleaning (`buffer(0)`, `make_valid()`) before writing a temporary GeoPackage, this error indicates that either the cleaning was insufficient, or that the file I/O cycle and/or the reprojection operation itself is re-introducing or exposing subtle invalidities.
    3.  The history of this project (`WORKLOG.md`) shows a recurring theme of geometry issues when passing data from R's `sf` package to Python's `geopandas`.
*   **Resolution:**
    1.  **Pipeline Hardening:** A patch has been applied to `summary_pipeline_landgrid.py`. The `zonal_stats` function will now re-apply the `gdf.geometry.buffer(0)` cleaning trick immediately after reading the vector data. This ensures that any geometry issues are fixed just-in-time before the reprojection is attempted, making the process more robust against these recurring data integrity problems.

---

### 2026-05-27 (cont. 4)
*   **Recurring Python Pipeline Failure (`RasterioIOError`):** The pipeline failed again with a `No such file or directory` error, this time for `n_retention_ratio_2020.tif`.
*   **Root Cause Analysis:**
    1.  The error message (`/data/base_years_ha/n_retention_ratio_2020.tif: No such file or directory`) is identical in nature to the previous failure. It indicates the Python script is looking for a file in a path that is missing the `/raw/` subdirectory.
    2.  The fix applied in the previous step (updating all paths in `analysis_configs/services_slim.yaml` to include `/raw/`) correctly resolves this issue for all raster layers.
    3.  The fact that the pipeline failed again on a *different* file but with the *same* path issue strongly indicates that the pipeline was executed using the original, un-patched YAML configuration file.
*   **Resolution:**
    1.  **Action Required:** The user must ensure they are running the Python pipeline using the version of `analysis_configs/services_slim.yaml` that was corrected in the previous step. No new code or configuration changes are needed.
    2.  The `summary_pipeline_landgrid.py` script remains robust enough to handle individual task failures if the YAML is partially correct, but the root cause of the `FileNotFoundError` must be addressed by using the fully corrected configuration file.

---

### 2026-05-27 (cont. 3)
*   **Python Pipeline Failure (`RasterioIOError`):** The Python pipeline failed during zonal statistics with a `No such file or directory` error for `sed_retention_ratio_2020.tif`.
*   **Root Cause Analysis:**
    1.  The immediate error is a `FileNotFoundError`, indicating the path in the `services_slim.yaml` config is incorrect or the file is missing.
    2.  The user confirmed with an `ls` command that all required raster files, including the ratio files, exist in a single directory: `.../raw/base_years_ha/`.
    3.  A review of `analysis_configs/services_slim.yaml` revealed that the paths were missing the `/raw/` subdirectory (e.g., pointing to `${GLOBAL_NCP_DATA}/base_years_ha/...` instead of `${GLOBAL_NCP_DATA}/raw/base_years_ha/...`). This path mismatch is the direct cause of the `FileNotFoundError`.
*   **Resolution:**
    1.  **Primary Fix:** All raster paths in `analysis_configs/services_slim.yaml` have been updated to include the correct `/raw/` subdirectory, ensuring they point to the actual file locations.
    2.  **Robustness Patch (Retained):** A patch was previously applied to `summary_pipeline_landgrid.py` to make it more robust. It now checks if a task returns `None` (indicating failure) and skips it, preventing the main script from crashing with an `AttributeError`. This remains a useful improvement.

---

### 2026-05-27 (cont. 2)
*   **Definitive Root Cause & Solution:** The user discovered that the Python `GEOSException` could be bypassed by using `geopandas.read_file(..., on_invalid="ignore")`. This confirms the root cause is a small number of features with invalid geometries being written by R's `sf` package that `geopandas` cannot read by default.
*   **Pipeline Hardening:** Instead of creating more intermediate "cleaned" files, the core Python script (`summary_pipeline_landgrid.py`) has been updated to use the `on_invalid="ignore"` flag. This makes the pipeline itself resilient to these minor upstream errors, providing a much more robust and direct solution. The separate `patch_add_biomes.R` script is no longer necessary, as the main `prepare_data.qmd` can be run in its complete form, and the Python script will now correctly ignore any problematic geometries it produces.

---

### 2026-05-27 (cont.)
*   **Pipeline Unblocking Strategy:** The `prepare_data.qmd` script continues to fail on complex geometry operations. To unblock the pipeline without losing more time, we've adopted a two-stage approach:
    1.  **Generate Base Grid:** Run a simplified version of `prepare_data.qmd` that intentionally excludes the problematic biome attribute join. This is expected to succeed and produce a clean grid with country/regional attributes.
    2.  **Patch Biomes:** Created a new, standalone script (`analysis/patch_add_biomes.R`) that takes the output from step 1 and performs only the biome join. This script uses a robust `st_join` followed by a `distinct(ID)` call to handle any duplicates created if a grid cell touches multiple biomes.
*   This strategy allows us to get a complete, analysis-ready grid file (`AOOGrid_10x10km_land_4326_clean.gpkg`) so the downstream Python pipeline can finally proceed.

---

### 2026-05-27
*   **Data Pipeline Failure & Strategic Rollback:**
    *   **Breaking Change Identified:** After multiple failed attempts to fix the `IllegalArgumentException: Invalid number of points in LinearRing` error, a comparison with the last known working version of `prepare_data.qmd` was performed.
    *   **Root Cause:** The error was introduced when the data preparation logic was changed to accommodate biome attributes. The original, working script used a direct `st_join` on polygons. The new, failing script introduced a call to `st_point_on_surface()` before joining, which is much less tolerant of minor geometric invalidities created during the `st_transform()` reprojection step.
    *   **Resolution:** The `prep-grid-aoo-land` chunk in `prepare_data.qmd` has been reverted to the simpler, more robust logic from the last working version. This removes the dependency on `st_point_on_surface` and the complex, multi-source attribute join, which was the source of the instability. The pipeline should now be able to generate the base grid successfully, as it did before these changes. The addition of biome data will be re-evaluated in a separate, more robust manner after the core pipeline is restored.

---

### 2026-05-26
*   **Data Pipeline Crisis & Strategic Pivot:**
    *   **Root Cause Re-confirmed:** The Python pipeline (`summary_pipeline_landgrid.py`) consistently fails with a `GEOSException: Invalid number of points in LinearRing` when reading the master grid GeoPackage (`AOOGrid_10x10km_land_4326_clean.gpkg`). This indicates a subtle geometry validity issue created by the R `sf` package that Python's `geopandas/shapely` cannot tolerate.
    *   **Failed Repair Attempts:** A standalone patch script (`patch_fix_grid_geom.R`) using `st_buffer(dist = 0)` was created to aggressively repair the geometries. However, this process proved to be unacceptably slow, running for over 4.5 hours without completion, making it an unviable solution.
    *   **New Strategy: Hybrid Raster-Vector Workflow:** A new, more robust strategy has been adopted to permanently solve this issue.
        1.  **Create Zone Raster:** A new script (`create_zone_raster.R`) was created to perform a fast, one-time conversion of the vector grid into a "zone raster" where each pixel's value is its corresponding `fid`.
        2.  **Raster-Based Zonal Stats:** A new Python script (`summary_pipeline_rasterzones.py`) will perform the zonal statistics using the new zone raster and the service rasters. This completely bypasses the need for Python to read the problematic vector file, eliminating the geometry errors.
        3.  **Attribute Join in R:** The main R script (`process_data.qmd`) will be updated to read the simple CSV output from the new Python script and join it back to the canonical vector grid, which contains all the rich attribute data (country, biome, etc.).
    *   This new hybrid approach is faster, more robust, and preserves the methodological integrity of the analysis by separating the geometry-heavy processing from the statistical calculation.

---

### 2026-05-22
*   **Major Data Pipeline Overhaul & Rerun:**
    *   **Root Cause Identified:** The critical `orig_fid not found` error in `process_data.qmd` was traced back to a stale base grid file (`AOOGrid_10x10km_land_4326_clean.gpkg`). This old grid was missing key attributes (like country names) and contained geometric artifacts (dateline wraparound), which were causing cascading failures in the Python pipeline.
    *   **Robust Solution Implemented:**
        1.  The `prepare_data.qmd` script was updated with a new, authoritative chunk (`prep-grid-aoo-land`) to generate a clean master grid from the original sources. This new grid correctly joins all attributes and fixes the dateline artifact.
        2.  The `process_data.qmd` script was simplified by removing the redundant and error-prone "Robust Attribute Assembly" logic, as the pipeline can now trust its clean input.
    *   **Full Data Regeneration Initiated:** A full rerun of the data pipeline has been started on the `lilling` server. This involves:
        1.  Running `prepare_data.qmd` to create the new master grid (a long process, ~1.5 hours).
        2.  Running the upstream Python pipeline (`summary_pipeline_landgrid.py`) for both services and beneficiaries using the new clean grid.
        3.  Running `process_data.qmd` to generate the final analysis-ready datasets.
    *   **YAML Fix:** Corrected a `YAMLException` in `chapters/01-problem.qmd` caused by improper indentation in the YAML header.
*   **Next Steps:** While the data regeneration runs, the focus will shift to refining the narrative, language, and presentation of the final Quarto book. A transfer prompt has been created to start a new chat session for this purpose.

---

### 2026-05-21
*   **Dashboard Layout Debugging Saga:** Spent significant time debugging the layout of `analysis/eda_dashboard.qmd`.
    *   **Initial Problem:** Plots were rendering too small to be readable in the dashboard format.
    *   **Attempt 1:** Switched the document format from `html` (page) back to `dashboard` and enabled `scrollable: true` to allow tall plots to render at their full height.
    *   **Attempt 2:** Implemented a side-by-side `columns` layout for the main plot sections to improve readability and use of space.
    *   **Core Issue Identified:** An unclosed `div` block (caused by a missing `:::` to close a `columns` section) was making all subsequent dashboard tabs appear empty.
    *   **Resolution & Final Layout:** Correctly structured the `columns` blocks for all sections, which fixed the empty tabs. After experimentation with side-by-side layouts (e.g., `width="50%"`), the decision was made to lock in a vertically stacked layout (`width="100%"` for all columns) within each major section. This provides a consistent, readable, top-to-bottom flow for all plots and tables in the dashboard. The layout is now considered stable.

---

### 2026-05-17 (cont.)
*   **Rasterization Workflow Template:** Created `scripts/gdal_rasterization_template.sh` to formalize and document the robust `gdal_rasterize`-based workflow. This template includes steps for GeoPackage reprojection and rasterization of both continuous and binary columns, ensuring easy reusability and preventing loss of this critical methodological knowledge.

---

### 2026-05-17
*   **Output Naming Convention:** Standardized raster output filenames to include the change metric (`_abs` or `_pct`) for clarity and consistency. For example, `hotspot_count.tif` is now `hotspot_count_abs.tif`. This ensures that all raster files can be distinguished by their filename alone.

---

### 2026-05-16
*   **Methodological Reflection:** Acknowledged that the extensive time spent debugging Python-based rasterization was inefficient. The direct use of `gdal_rasterize` from the command line proved to be a faster, more powerful, and more reliable solution from the beginning. Future rasterization tasks should default to using the core GDAL command-line tools to avoid similar issues with high-level library wrappers.

---

### 2026-05-15
*   **Rasterization & Grid Validation Saga:**
    *   Spent significant time debugging a persistent and subtle rasterization issue. Initial attempts to rasterize hotspot counts using the `vector_to_raster.py` script resulted in "ghost rasters" (tiny file sizes, empty when loaded in QGIS/R) and severe spatial misalignment artifacts (a single vector grid cell producing up to four raster pixels).
    *   After exhausting multiple fixes within the Python `rasterio` library (grid snapping, nodata value changes, removing compression/tiling), the root cause was identified as a deep incompatibility within the library stack in the server's Python environment.
    *   **Definitive Solution:** Abandoned the Python script in favor of the core `gdal_rasterize` command-line tool. This immediately produced a correctly aligned raster with a 1-to-1 mapping between vector cells and raster pixels. This will be the standard procedure for all future rasterizations.
    *   **Grid Geometry Verification:** A subsequent check of the reprojected vector grid's geometry (`hotspots_global_abs_epsg8857.gpkg`) initially caused confusion, as the bounding box of individual cells was not 10km x 10km.
    *   **Final Validation:** Developed a new verification script (`verify_grid_area.py`) to measure the true geometric **area** of the reprojected polygons, not just their bounding box. This definitively confirmed that each grid cell has an area of **100 km²**, validating the integrity of our equal-area grid and resolving a long-standing point of uncertainty. The project's core spatial foundation is now fully verified.

---

### 2026-05-12
*   **Finalize `hotspot_synthesis.qmd` & Prepare for Interpretation:**
    *   Completed a major debugging and refinement pass on `analysis/hotspot_synthesis.qmd` to ensure it runs locally and produces clean, final outputs.
    *   Resolved numerous rendering errors, including TeX installation failures (by switching to HTML output), missing `kable()` function errors (by adding `library(knitr)`), and data type mismatches in summary tables.
    *   Significantly improved the population exposure plots by:
        *   Correctly handling and filtering income group categories to remove "NA" values from plots.
        *   Enforcing a canonical service order for facets.
        *   Switching to a fixed y-axis scale for better comparability across services.
    *   Enhanced the report's clarity by replacing the raw configuration code chunk with a clean, formatted summary table.
    *   The `hotspot_synthesis.qmd` notebook is now stable and produces all necessary summary tables and visualizations, paving the way for the final interpretation phase.
    *   Prepared a transfer prompt and a git commit message to checkpoint this progress before moving to a new chat session focused on `analysis/results_interpretation.qmd`.

---

## Project Overview & Goals

**Goal:** Quantify global change in multiple ecosystem services (ES) at ~10-km resolution over 1992–2020, identify **hotspots** of concerning change, and attribute those changes to environmental and socioeconomic drivers.

**Key Objectives:**
1.  Robust spatial extraction of ES changes bypassing Modifiable Areal Unit Problem (MAUP) artifacts.
2.  Identification of ES hotspots using Symmetric Percentage Change (to handle zero-baselines and capture local vulnerability).
3.  Integration of Land Cover Change (LCC) metrics to attribute ES decline to Land Conversion vs. Degradation.
4.  Socioeconomic characterization of hotspots via Kolmogorov-Smirnov (KS) tests.

## Current State: Version 1.3.2 (Visual Unification & Presentation Polish)

**Status:** The core pipeline architecture is mathematically validated, cleaned, and finalized. We have successfully unified the visual styling using canonical WWF colors across all flowcharts and plots, preparing everything for the final presentation.

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

### 2026-05-08 (Urgent Task: Rasterization)
### 2026-05-08 (Major Conceptual & Analytical Refinements)
*   **Nuanced Driver Mapping:** Finalized the land cover change driver analysis by significantly refining the classification logic in `make_lcc_driver_map.R`. Replaced the generic "Multiple Overlapping Drivers" category with specific, policy-relevant transitions like "Deforestation for Cropland," "Savannization / Pasture," and "Grassland to Cropland."
*   **Grassland Dynamics:** Fully integrated both "Grassland Loss" and "Grassland Expansion" as distinct drivers into the mapping scripts, ensuring these critical rangeland dynamics are no longer masked.
*   **Refined Attribution Terminology:** Replaced the presumptive "Degradation-driven (Stable Land Cover)" category in `make_attribution_map.R` with the more accurate and defensible term **"Attribution Gap (Change without Conversion)"**. This new label correctly describes ES hotspots that are spatially decoupled from major land conversion.
*   **Clarified Metric Interpretation:** Added a detailed methodological note to `make_lcc_driver_map.R` to explicitly state that the land conversion metrics are calculated as a percentage of the *total 10km cell area*, clarifying the interpretation of "landscape transformation intensity".
*   **Expanded Socioeconomic Analysis:** Enhanced `hotspot_synthesis.qmd` to include absolute population exposure analysis by GDP and Gini quartiles. Removed the redundant 'Built Area' variable from the `KS_tests_hotspots.qmd` analysis.
*   **Visualization & Documentation Polish:** Increased map output resolution to 600 DPI for clarity, improved color palettes for distinguishability, and updated `Key_Takeaways.md` and other documentation to reflect all conceptual shi*   **Hotspot Count Rasterization:** Developed a new Python script `Python_scripts/convert_hotspot_gpkg_to_raster.py` to convert the vector-based hotspot count maps (from `hotspots_global_pct.gpkg`) into a GeoTIFF raster. This provides a raster-based output of hotspot frequency, as urgently requested.
*   **Hotspot Count Rasterization:** Developed a new Python script `Python_scripts/convert_hotspot_gpkg_to_raster.py` to convert the vector-based hotspot count maps (from `hotspots_global_pct.gpkg`) into a GeoTIFF raster. This provides a raster-based output of hotspot frequency, as urgently requested.

---

### 2026-05-11
*   **General-Purpose Rasterization Utility:** Refactored the specialized `convert_hotspot_gpkg_to_raster.py` script into a flexible, general-purpose command-line tool named `vector_to_raster.py`.
    *   The new script is no longer hardcoded to specific "hotspot" columns. It now accepts a list of columns to rasterize via a required `--columns` argument.
    *   Enhanced flexibility by adding command-line arguments to control the output `--resolution`, target `--crs`, `--nodata` value, and raster `--dtype` (e.g., `float32`, `int16`).
    *   This provides a robust and reusable utility for converting any attribute from a vector file into a GeoTIFF, addressing the need for a more reliable rasterization method than manual QGIS operations for our various grid-level summary files.

*   **Pipeline Cleanup & Housekeeping:**
    *   Disabled the creation of large intermediate synthesis files (`10k_grid_synth_all.gpkg`, `10k_grid_ES_change_benef.gpkg`) in `process_data.qmd` by default. These files were useful for debugging but are not required for the final analysis and can be recreated if needed. This change will keep the `processed/` directory cleaner.
    *   Documented the legacy status of `prepare_data.qmd` with a note explaining it is a one-time setup script for the base grid and not part of the routine analytical workflow.
    *   Confirmed that existing intermediate files can be safely deleted to free up disk space.

*   **Documentation Consolidation:** Overhauled the project's documentation to eliminate redundant `README` files and establish a clear, maintainable structure.
    *   Consolidated all high-level information into a single, comprehensive root `README.md`.
    *   Created a central `docs/` directory to house detailed, long-form documentation.
    *   Moved content from various `README_*.md` files into `docs/methodology.md`, `docs/data_dictionary.md`, and `docs/runbook.md`.
    *   Explicitly documented the key methodological distinction between the project's "difference of aggregates" approach (Path B) and the alternative "aggregate of differences" (Path A) in `docs/methodology.md`.

---

## Chronological Log (Newest to Oldest)

### 2026-05-05
*   **Visualization Consistency:** Updated `hotspot_synthesis.qmd` to ensure all "hotness" and "exposure" bar charts use a consistent red intensity color scale (`scale_fill_distiller`) mapped to the value, rather than categorical colors for the groups. This improves visual coherence across the analysis.
*   **Code Health:** Added the `group_palettes` object definition to `hotspot_synthesis.qmd` to resolve a missing object error that was causing rendering to fail.
*   **Visual Unification & Cleanup:** Systematically removed redundant "main report" plotting blocks and ensured the "High income: nonOECD" category is consistently and globally filtered out from all visualizations in `hotspot_extraction.qmd` and `hotspot_synthesis.qmd` to reduce noise.
*   **LCC Grasslands Integration:** Integrated "Model 3: Grassland Loss" into the `LC_change_granular.qmd` pipeline, adding a specific reclassification matrix to explicitly track the conversion of grasslands to other uses.
*   **Narrative Refinement:** Updated `Key_Takeaways.md` to incorporate the "Spatial Attribution / Degradation" findings and highlight the new Grassland Loss model, aligning with the latest feedback.
*   **Plotting Iteration (Synthesis & Volumetric Plots):** Reverted the combined volumetric plots in `hotspot_extraction.qmd` back to separate figures for absolute and percent change. Fixed the y-axis labels in the `hotspot_synthesis.qmd` bar charts to display the numeric key instead of being blank, improving readability.

### 2026-05-06
*   **Boxplot Unification & Refinement:** Refactored the entire boxplot generation logic in `hotspot_extraction.qmd` into a single, unified function. This ensures all boxplots (volumetric, ratio, coastal) have a consistent aspect ratio, a universal numeric legend with a key at the bottom, and larger, more readable fonts. This resolves previous inconsistencies and simplifies future maintenance.
*   **Data Dictionary Updates:** Improved the data dictionaries in `KS_tests_hotspots.qmd` and `hotspot_synthesis.qmd` to provide clearer, more accessible definitions for key statistical terms and output table columns, enhancing the project's usability for collaborators.
*   **Granular LCC Integration:** Verified and finalized the integration of the "Grassland Loss" model into `LC_change_granular.qmd`, ensuring its results are correctly consolidated into the final output GeoPackage.

### 2026-05-04
*   **Infrastructure & Environment:** Resolved persistent VS Code Remote SSH synchronization and connection hangs that have been occurring since last week on `lilling`.
    *   *Diagnosis:* The VS Code server backend was fragmenting and leaving behind orphaned `node` processes for language servers (Pylance, Quarto) and the core RPC server, which blocked new connections.
    *   *Troubleshooting:* Implemented a targeted process-kill command (`pkill -u jeronimo -f .vscode-server`) via terminal to forcefully clean up the hung background processes. This successfully resets the remote connection state without requiring physical or system-level reboots of the server by IT.
*   **Plotting Refinement:** Updated `compare_and_plot_changes.R` to exclude the "High income: nonOECD" group from the main report's bar plots to remove outliers and clarify the primary trends, as discussed in the last review meeting.
*   **Housekeeping:** Identified and removed a redundant, outdated copy of `hotspot_extraction.qmd` that was incorrectly located in the `R/` directory. Confirmed `analysis/hotspot_extraction.qmd` is the correct, canonical version.

### 2026-05-02
*   **Infrastructure & Sync:** Diagnosed and bypassed silent VS Code Remote SSH hangs on `lilling` without a hard reboot (safely wiped corrupted `~/.vscode-server`). Established a `tar`-over-SSH sync workaround to bypass strict Windows IT firewalls lacking `rsync`.
*   **Python Engine Optimization:** Refactored `zonal_stats_toolkit/runner.py` to concurrently schedule both raster and vector tasks in the execution graph, significantly improving parallelism ahead of the v1.4.0 merger.
*   **Visual Polish (Boxplot Color Ramps):** Solved the `ggplot2` global scale dominance issue in the plotting scripts (`hotspot_extraction.qmd`). Implemented localized data normalization (`scales::rescale`) so canonical intensity colors (Reds) dynamically scale from 0 to 1 strictly within their respective facets.
*   **Methodology Documentation:** Updated `README_Methodology.md` to formally transition "Path C" from a hypothetical "Future Analysis" into a completed "Validation Analysis," explicitly confirming that the grid-level hotspots mathematically align with pixel-level differences.
*   **Feedback Manifesto Audit:** Cross-referenced meeting notes to finalize terminology ("Multi-service Decline" over "collapse"), prepared the biome-faceted scatterplots for the "Attribution Gap", and confirmed non-OECD outlier exclusions for main report boxplots.
*   **Next Steps Planned:** Ready to implement "Model 3: Grassland Loss" in `LC_change_granular.qmd` to accurately track Forest-to-Grassland and pristine Grassland-to-Cropland transitions.

### 2026-05-01
*   **Post-Meeting Debrief & Cleanup:** Successfully presented the "Drivers of Change" (LCC Attribution) and "Who is Affected" (Socioeconomic / KS Tests) sections to Steve and Becky. The compound risk mapping, red-intensity boxplots, and LCC driver correlations resonated strongly. 
*   **De-escalating "Rescue Mode":** Safely stripped out local hardcoded `here("home", "jeronimo", ...)` fallback paths from all R mapping scripts (`make_socieconomic_maps.R`, `make_attribution_map.R`, `make_lcc_overview_map.R`, `make_hotspot_count_map.R`). Returned the pipeline to universally use `data_dir()` for server-side processing on `lilling`.
*   **Server Stability & Repackaging:** Remotely rendered `hotspot_extraction.qmd` on `lilling` to establish the final, single source of truth. Repackaged the `global_ncp_data_archive.tar.gz` archive with the updated Data Dictionary, preparing the data outputs for distribution without plot files.
*   **Path to v1.4.0 (The Merger):** With Pillar 4 and Pillar 5 validated, the repository is officially ready for the massive architectural merge. The upcoming `v1.4.0` will natively integrate the Python `zonal_stats_toolkit` directly into the `global_NCP` repo, creating a single, unified pipeline repository.

### 2026-04-30
*   **Visual Unification (4+ Hotspot Cap):** Standardized the compound risk narrative by capping overlapping hotspot counts at "4+" across both spatial maps and regional stacked barplots. Applied a unified semantic color ramp (Yellow to Dark Red) across `make_hotspot_count_map.R` and `hotspot_synthesis.qmd` to ensure immediate visual recognition of extreme compound risk.
*   **"First Look" Overview Maps:** Created minimalist, high-resolution global overview maps (solid red, no heatmaps) for both absolute and percentage hotspots to serve as clean anchor visuals for the presentation slide deck.
*   **Server Rendering & Single Source of Truth:** Pushed all visualization updates to the remote repository and successfully re-rendered the canonical `hotspot_synthesis.qmd` pipeline on the Lilling server, ensuring all plots and CSVs remain perfectly in sync.
*   **Next Immediate Step:** Diving into the "Attribution Gap" (Pillar 4) by analyzing Land Cover Conversion (LCC) overlaps using `lcc_es_hotspot_overlap_pct.csv` to build out the narrative for Coastal Risk (driven by Urban Expansion) and Pollination (driven by Forest Loss/Cropland Expansion).

### 2026-04-28
*   **Spatial Alignment Crisis Averted**: Diagnosed and eliminated a critical `seq_len()` reassignment bug in variable-length datasets across `process_data.qmd`, `hotspot_extraction.qmd`, and `hotspot_synthesis.qmd` that was scrambling downstream spatial joins and creating "striped" artifacts in maps. Enforced strict `stop()` fallbacks to prevent silent spatial corruption.
*   **Emergency "Rescue Mode" Implementation**: Successfully extracted and utilized a 2.2GB data archive (`global_ncp_data_archive.tar.gz`) to bypass long-running spatial joins under a strict deadline, temporarily routing scripts to safely read local `plt_long.rds` and GPKGs.
*   **Visualization Overhaul (Barplots & Intensity)**: Replaced confusing categorical color ramps in signed change bars and intensity plots with strict, semantic "Good (Green) / Bad (Red)" logic. Implemented an automatic alphanumeric `[ID]` key system on the y-axis to perfectly map subregions to legends.
*   **Dual-Metric Driver Analysis**: Upgraded `make_attribution_map.R` and the `hotspot_extraction.qmd` land-cover driver overlap chunks to loop over both Absolute (`abs_chg`) and Percentage (`pct_chg`) metrics. Programmatically recreated the massive `global_attribution_gap_map.png` directly in R to eliminate QGIS bottlenecks.
*   **Upcoming Priorities (Next 48 Hours)**:
    *   **Drivers**: Review and compare the newly generated `abs_chg` vs `pct_chg` scatterplots and heatmaps to finalize the "Attribution Gap" narrative.
    *   **Equity**: Review the "Absolute Population Exposure" (affected people) outputs generated by `hotspot_synthesis.qmd`.
    *   **Socioeconomics**: Perform a final validation pass on the KS analysis results.
    *   **Presentation**: Finalize the "Why" (drivers) and "Who" (people) sections of the presentation slide deck.
*   **Version 1.3.2 Release**: Unified pipeline visual styling (Mermaid flowcharts, spatial maps, and plots) to strictly use canonical WWF colors. Cleaned up redundant documentation and finalized the narrative methodology structure for the presentation slide deck.

### 2026-04-27
*   **Hotspot Boxplot Pipeline Overhaul**: Resolved critical "silent failures" in `hotspot_extraction.qmd` where Quarto intercepted error messages and skipped plot generation due to missing `plt_long` attributes. Implemented a robust on-the-fly attribute join from the master grid (`AOOGrid_10x10km_land_4326_clean.gpkg`), fixed `dplyr` dynamic scoping issues (`across(all_of())`), and added aggressive `stderr()` diagnostic logging.
*   **Coastal Visualization Fix**: Refactored coastal service boxplots to use pre-calculated 1.5*IQR whiskers (`stat="identity"`) instead of `outlier.shape=NA`. This permanently solves the issue of invisible outliers stretching the y-axis and causing scattered point artifacts.
*   **PDF Image Resolution**: Fixed LaTeX pathing during document rendering to ensure the freshly rendered, canonical-colored plots are correctly embedded into the final PDF.
*   **KS Test Enhancements**: Deprecated legacy `cfg$paths` in favor of `data_dir()` across the pipeline, and successfully integrated **Built Area** (`GHS_BUILT_S_E2020_mean`) into the socioeconomic covariate analysis.
*   **Methodology Flowchart (`workflow.qmd`)**: Developed a presentation-ready, high-resolution Mermaid.js flowchart documenting the end-to-end analytical pipeline. Mapped the dual-path extraction architecture (Regional Zonal Summaries vs. 10km Grid Analysis) and perfectly aligned the final deliverables with the slide deck's narrative structure (WHAT, WHERE, WHY, WHO). Bypassed strict parsing bugs in Mermaid v11.6.0 to implement custom WWF color palettes, transparent overlays, and thick routing arrows.

### 2026-04-23
*   **Workspace Integration**: Configured a VS Code Multi-root Workspace bringing `global_NCP` and `zonal_stats_toolkit` side-by-side for unified development.
*   **Documentation Unification**: Merged the historical worklog from the `zonal_stats_toolkit` repository into the central `WORKLOG.md` to officially centralize project tracking.
*   **Aesthetic Unification & Fixes**: Applied universal canonical color palettes for Biomes, WB Regions, and Income Groups across the `global_NCP` and `zonal_stats_toolkit` plotting scripts. Fixed exact string matching issues for Income Groups with numeric prefixes.
*   **Contextual Mapping**: Developed `generate_context_groupings_map()` to produce a 4-facet overview map of all geographic groupings, providing a clean visual baseline for the slide deck introduction.
*   **Equity Analysis (Impact Tier)**: Audited codebase for population metrics and implemented the `Absolute Population Exposure` module in `hotspot_synthesis.qmd`. This calculates the total number of people living in top 5% ES hotspots, segmenting the vulnerable populations by HDI bin and Income Group.
*   **Narrative Consistency**: Enforced standard terminology: "Relative Socioeconomic Shift" for KS statistical testing and "Absolute Population Exposure" for raw population counts.

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

### 2026-03-20 (Pre-v1.3.3)
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
