# Worklog — Global NCP Hotspots

## Current focus
- **Completed** KS Analysis, Hotspot Intensity, and Multi-service workflows (v1.0.2).
- **Completed** Re-calculating core ecosystem service ratios and difference rasters.
- **Completed** LCC Pipeline Development: Finalized `analysis/LC_change.qmd` using `diffeR` for robust transition metrics (1992-2020).
- **Completed** Synthesis: Updated `analysis/hotspot_extraction.qmd` with the "Drivers of Change" section to calculate ES/LCC overlaps.
- **Active** Documentation: Updating READMEs to reflect the `diffeR` methodology and attribution workflow.
- **Completed** Versioning: Bumped analysis version to **v1.1.0** (Binary Land Cover Change).
- **Active** Versioning: Bumped analysis version to **v1.2.0** (Granular Land Cover Change).
- **Completed** Coastal Risk Reduction: Resolved calculation issue, incorporated into analysis, and updated charts.
- **Completed** Execution: `analysis/LC_change.qmd` finished successfully (generated `processed/10k_lcc_metrics.gpkg`).
- **Active** Interpretation: Analyzing "Drivers of Change" (LCC vs ES correlations) and rendering final reports.
- **Completed** Zonal Stats: Calculated summary statistics for base years (1992 & 2020) independently using per-hectare corrected rasters.
- **Active** Difference Analysis: Running zonal statistics on pre-calculated difference rasters (Path C) to compare "Difference of Means" vs "Mean of Differences".
- **Active** Granular LCC: Setting up and running `analysis/LC_change_granular.qmd` for Forest Loss and Urban/Cropland Expansion.

## 2026-02-20

*   **Granular Land Cover Change Analysis:**
    *   Created `analysis/LC_change_granular.qmd` to implement specific driver models:
        *   **Forest Loss:** Binary classification (Forest vs. Non-Forest) to track deforestation/reforestation.
        *   **Expansion:** Multi-class (Urban, Cropland, Other) to track anthropogenic expansion.
    *   Implemented `diffeR` metric extraction (Gain, Loss, Persistence, etc.) for these specific models.
    *   Added safety logic to rename `fid` to `grid_fid` in the output GPKG to prevent driver conflicts.
*   **Hotspot Workflow Flexibility:**
    *   Refactored `analysis/hotspot_extraction.qmd` to parameterize the input GPKG path (`params$input_gpkg_path`). This allows easy switching between the standard consolidated data and new experimental outputs without code changes.

## 2026-02-24

*   **Refined Granular LCC Workflow:**
    *   **Data Preparation:** Created `analysis/LC_change_preparation.qmd` to handle raw ESA CCI/C3S zip extraction and reclassification. This ensures consistent mapping (e.g., Flooded Trees -> Forest) and separates heavy I/O from analysis.
    *   **Granular Analysis:** Updated `analysis/LC_change_granular.qmd` to consume the pre-processed 9-class rasters, removing redundant processing logic.
    *   **Visualization:** Created `analysis/viz_granular_lcc.qmd` for quick inspection of Forest Loss and Urban/Cropland Expansion maps.
    *   **Documentation:** Updated READMEs to reflect the new modular LCC pipeline and granular methodology.
    *   **Optimization:** Modified `analysis/LC_change_granular.qmd` test mode to sample **contiguous** grid cells instead of random ones, significantly reducing raster I/O overhead during testing.
    *   **Execution:** Aborted initial render and restarted `analysis/LC_change_granular.qmd` inside a `screen` session to ensure stability for the long-running process (>24h expected).
    *   **Completion:** Granular LCC analysis finished (~48h runtime). Output: `processed/10k_lcc_granular_metrics.gpkg`.

## 2026-02-27

*   **V2 Pipeline Implementation (Symmetric Change):**
    *   Created `analysis/process_zonal_stats_v2.qmd` to ingest new zonal stats and calculate **Symmetric Percentage Change** (handling division-by-zero artifacts).
    *   Updated `analysis/hotspot_extraction.qmd` to be **version-aware** (accepts `input_gpkg` and `output_suffix` params).
    *   Updated `analysis/KS_tests_hotspots.qmd` to accept V2 inputs/outputs.
    *   **Execution:** Launched `process_zonal_stats_v2.qmd` to generate the V2 dataset.
    *   **Debugging:** Fixed `fid` column not found error in `process_zonal_stats_v2.qmd` by adding a safety check to create the column if missing after `st_read`.

## 2026-03-03

*   **V2 Pipeline Debugging & Fixes:**
    *   **Diagnosis:** Identified that `10k_change_calc_v2.gpkg` contained `NA` values in all change columns. Created `analysis/debug_v2_data.R` to confirm the issue, which stemmed from a `fid` mismatch (NULL `grid_fid` in the new zonal stats file).
    *   **Fix:** Modified `analysis/process_zonal_stats_v2.qmd` to ignore the corrupt `grid_fid` and instead force `fid` creation based on sequential row numbers (`seq_len(nrow)`). This ensures correct alignment since row order is preserved.
    *   **Verification:** Re-ran debug script; confirmed V2 data is now fully populated with valid change metrics.
*   **V2 Execution:**
    *   Launched full V2 hotspot extraction: `quarto render analysis/hotspot_extraction.qmd -P input_gpkg:10k_change_calc_v2.gpkg -P output_suffix:_v2`.
*   **Comparison Framework:**
    *   Created `analysis/compare_hotspots_v1_v2.qmd` to compute the Jaccard Index (spatial overlap) between V1 and V2 hotspots, allowing quantification of how the Symmetric Percentage Change method alters results.

## 2026-03-04

*   **V2 Pipeline Optimization & Fixes:**
    *   **Hotspot Extraction:**
        *   Implemented caching for `plt_long` to speed up iterative rendering.
        *   Added robust fallback logic for Land Cover driver column names (handling variable year suffixes).
        *   Optimized GPKG export: Added checks to skip `st_write` if output files already exist, reducing runtime.
        *   Fixed `pivot_longer` errors in the "Drivers of Change" section by sanitizing named vectors.
        *   Corrected the Land Cover driver export loop to ensure all drivers are included in the grouped GPKGs.
    *   **KS Tests:**
        *   Synchronized filters: Added Continent (Antarctica/Seven Seas) and Biome (Lakes/Rock & Ice) filters to match the extraction pipeline.

## 2026-03-09

*   **Final Base Year Extraction (Per Hectare):**
    *   Initiated a fresh, clean run of the summary statistics extraction for all base year services (1992 & 2020).
    *   **Methodology Update:** Volumetric variables (e.g., Nitrogen Export, Sediment Export) have been corrected from a "per pixel" to a "per hectare" basis to ensure consistency across varying pixel sizes globally.
    *   **Unchanged Variables:** Ratios (Retention Ratios) and Indices (Coastal Protection, Nature Access) remain unchanged as the per-hectare conversion is not applicable or neutral for these metrics.
    *   **Goal:** Establish the definitive baseline dataset for the "Difference of Means" pathway.

*   **Difference Raster Analysis (Path C):**
    *   Configured `analysis_configs/services_diff_ha.yaml` to process the pre-calculated difference rasters (1992-2020) located in `2020_1992_ch_ha`.
    *   **Correction:** Updated filenames in the YAML to match the actual output from the difference calculation script (e.g., `n_export_diff_1992_2020.tif` instead of `global_n_export...`).
    *   **Execution:** Launched the summary pipeline for these difference rasters to generate the "Aggregate of Differences" dataset (`summary_pipeline_workspace_diff_ha`).

## 2026-03-10

*   **Difference Analysis (Path C) Complete:**
    *   The `summary_pipeline_landgrid.py` run on the hectare-normalized difference rasters (`2020_1992_ch_ha`) has successfully completed.
    *   This provides the "Mean of Differences" dataset, which is a critical component for methodological validation.

*   **Initiate Consolidation & Validation Phase:**
    *   Created `analysis/Consolidation.qmd` as the new central script for the final analysis push.
    *   **Goal:** This script will load the outputs from both Path B ("Difference of Means") and Path C ("Mean of Differences"), compare them to quantify any divergence, and then integrate the chosen primary dataset with Coastal Protection and socioeconomic data.
    *   The first step is to populate the paths in the script and run the validation comparison plots.

*   **Data Consolidation (Path B):**
    *   Finalized the primary base-year services dataset (`interim/10k_grid_services_base.gpkg`). This dataset is the result of the `summary_pipeline_landgrid.py` run on the **per-hectare corrected** base year rasters.
    *   Manually joined the vector-based coastal protection data to this file in QGIS, creating the final input for the change calculation step.
    *   Refactored and cleaned `analysis/process_data.qmd` to use this new consolidated file, removing legacy code and updating paths.
    *   **Versioning:** Bumped the analysis version in `process_data.qmd` to **v1.2.1** to formally track the inclusion of per-hectare normalized data.
    *   Updated `README_Methodology.md` and `README_pipeline.md` to clearly document the rationale for per-hectare unit standardization.

## Upcoming Tasks (The Final Push)

*   **Data Integration:**
    *   Join Coastal Protection vector data (base years and differences) to the main 10km grid datasets.
*   **Validation:**
    *   Compare "Nature Access" statistics between the main pipeline and the alternative pipeline to ensure consistency.
    *   Calculate absolute and relative change for the base year summaries (Path B).
    *   Compare "Difference of Means" (Path B) vs "Mean of Differences" (Path C) to quantify methodological divergence.
*   **Final Analysis:**
    *   Run the full Hotspot Extraction and Statistical Analysis (KS tests, etc.) on the final, validated dataset.

## Strategic Narrative / Pitch (2026-02-11)

**Subject:** Status Update: Integrating Land Cover Drivers into Global NCP Hotspots

**Where we are:**
We have successfully stabilized the core **Hotspot Identification Pipeline (v1.0.2)**. We can now confidently pinpoint *where* ecosystem services are changing most drastically across the globe, with breakdowns by Biome, Income Group, and Region.

**The Missing Piece:**
Knowing *where* hotspots are is only half the battle. We need to explain *why* they are there. Specifically, we need to distinguish between hotspots driven by **Land Conversion** (e.g., deforestation, urbanization) versus those driven by **Degradation/Intensification** (changing conditions within the same land use).

**What we are doing:**
We have implemented a robust Land Cover Change (LCC) module using the `diffeR` methodology (Pontius et al.).
1.  **Methodology:** Instead of simple overlaps, we are calculating precise transition metrics (Gross Loss, Gross Gain, Persistence) for every 10km grid cell between 1992 and 2020.
2.  **Integration:** We are treating "Gross Loss of Natural Land" as a primary driver.
3.  **Synthesis:** We are currently computing these metrics. Once complete, we will overlay them with our ES hotspots to quantify the "Attribution Gap"—e.g., *"X% of Nitrogen Export hotspots are explained by direct land conversion."*

**Next Deliverable:**
A "Drivers of Change" visualization that ranks ecosystem services by their sensitivity to land cover change.

## Environment notes
- Local machine: Lenovo (Windows 11)
- Remote: lilling (VS Code Remote SSH)
- AI assistant: GitHub Copilot + Copilot Chat
- Personal MacBook may still retain Codex history and could be used later to recover past context.

## Active entry points
- analysis/process_data.qmd
- analysis/hotspot_extraction.qmd
- analysis/KS_tests_hotspots.qmd

## Known issues / gotchas
- Hotspot rules (loss vs gain services) must remain centralized in `HOTS_CFG`.
- Be careful not to mix interpretive direction (good/bad change) with magnitude summaries.
- Do not use ChatGPT Codex Connector on lilling (auth persists after uninstall).

## Next steps (short horizon)
1. **Render Hotspots Report**: Run `analysis/hotspot_extraction.qmd` to generate the consolidated PDF with correlation plots.
2. **Interpretation**: Analyze scatterplots to distinguish "Conversion-driven" vs "Degradation-driven" hotspots.
3. **Reporting**: Summarize the "Attribution Gap" findings for the manuscript/presentation.

## Future Tasks (Long-term)
1.  **Adapt analysis for multi-temporal data:** Adapt analysis to handle updated modeled ES layers and multiple points in time (beyond bi-temporal T0, T1). Strategize for incorporating multi-temporal data.
2.  **Quantify hotspot vs. non-hotspot change:** Develop a method to quantify and visualize the share of total change (from bar plots) that occurs within hotspots versus outside of them, possibly using stacked bar plots.

## Session notes
- 2026-01-05: Created `doc/ai_context.md` and `doc/ai_context.min.md`. Migrated AI workflow to Copilot after Codex auth failure. Sign flip issue remains unresolved and explicitly tracked here.
- 2026-01-06: Fixed critical bug in `Consolidation.qmd` where `c_fid` was dropped, causing "No hotspots found" errors downstream. Resolved file casing conflict (`Consolidation.Qmd` vs `.qmd`). Regenerated `10k_change_calc.gpkg` and verified ID consistency.
- 2026-01-06 (cont): Resolved "No hotspots found" by normalizing service names in `hotspot_extraction.qmd` (lowercase -&gt; canonical lookup) to match `HOTS_CFG`. Confirmed successful export with diagnostic logs.
- 2026-01-06 (cont): Configured `hotspot_extraction.qmd` for PDF generation by disabling heavy computation chunks (`hotspots_export`, `pivot`, plot generation) to rely on cached outputs from the HTML run.
- 2026-01-06 (cont): Bumped analysis version to v1.0.1 in `Consolidation.qmd` and `hotspot_extraction.qmd` to mark the stable hotspot release.
- 2026-01-06 (cont): **Hand-off to Agent**:
    - **State**: Pipeline stable (v1.0.1). Hotspots exported.
    - **Immediate Goal**: Optimize and run `analysis/KS_tests_hotspots.qmd`.
    - **Key Context**:
        - Input: `processed/10k_change_calc.gpkg` (Canonical).
        - Config: Ensure KS config matches `HOTS_CFG` in `hotspot_extraction.qmd` (loss/gain services).
        - Optimization: `KS_tests_hotspots.qmd` currently re-pivots data. Should reuse `outputs/tables/plt_long.rds` if available to save time.
- 2026-01-06 (cont): Cleaned up `Consolidation.qmd` and `hotspot_extraction.qmd` for readability (removed legacy comments, formalized text) without altering code logic.
- 2026-01-06 (cont): Extracted pipeline overview and methods text from `Consolidation.qmd` to a new `README_pipeline.md` to serve as a central methods draft.
- 2026-01-06 (cont): Manually added Executive Summary to `analysis/README_pipeline.md`.
- 2026-01-07: Resolved the sign flip issue in change metrics (absolute vs percent) by normalizing service names and centralizing logic. All downstream analyses (hotspot extraction, KS tests) now use consistent, canonical service definitions. Successfully re-ran the KS analysis with updated data and code; outputs are reproducible and ready for interpretation. Added explicit documentation and export of the pivoted long table (plt_long) and KS results for external review and reproducibility. Project is now in a stable, review-ready state.
- 2026-01-09: Implemented `Python_scripts/batch_raster_diff.py` to recursively calculate difference rasters (2020 - 1992) for all services. This supports the transition to using the `zonal_stats_toolkit` (located at `/home/jeronimo/projects/zonal_stats_toolkit`) for zonal synthesis. Updated `analysis/zonal_reanalysis.qmd` to initialize the Python environment (`coastal_snap_env`) and prepare for the new analysis workflow.
- 2026-01-16: **Repository Cleanup & Archival**:
    - Archived `analysis/zonal_stats.Rmd` to `analysis/legacy_zonal_stats.qmd` (deprecated R `exactextractr` workflow).
    - Archived `analysis/asign_ids_grid.qmd` to `analysis/legacy_asign_ids_grid.qmd` (deprecated rasterization workflow).
    - Updated `README.md` to document helper scripts (`restore_checkpoint.R`, `save_checkpoint.R`) and the deprecated legacy scripts.
    - **Checkpoint**: Committing current state (v1.0.1 cleanup) to prepare for merge to `main`.
- 2026-01-19: **Created a new script for robust ratio calculations.**
    - Investigated the codebase to find existing raster ratio calculations.
    - Created `Python_scripts/calculate_ratios.py` to generate reliable sediment and nitrogen retention ratios for 1992 and 2020, as well as their bi-temporal differences.
    - Corrected the nitrogen retention formula to the more standard `retention / (retention + export)` to avoid artifacts present in previous calculations.
    - Engineered the script to be highly performant and robust by:
        1.  Adding `BIGTIFF=YES` support to handle large output files.
        2.  Implementing a parallelized, tiled processing approach for speed and memory efficiency.
        3.  Debugging and resolving thread-safety issues in the parallel implementation related to reading compressed TIFFs.
    - Updated `environment.yml` to include `tqdm` as a dependency and advised on building a custom Docker image for a reproducible environment.
    - **Fixes during execution**:
        - Encountered `ValueError: operands could not be broadcast together` when processing edge tiles (e.g., 3x256 vs 256x256).
        - **Fix**: Added `boundless=True` to `rasterio.read` calls in `calculate_ratios.py` to ensure consistent tile shapes at image boundaries.
    - **Status**: Ratio calculations (Sediment/Nitrogen 1992 & 2020) and difference rasters completed successfully.
    - **Next**: Added automated statistical checks to `calculate_ratios.py` to scan difference rasters for atypical values (outliers outside [-1, 1] range).
- 2026-01-21: **Technical Issue Resolution & Next Steps**:
    - **Percentage Change Artifacts**: [RESOLVED / DOCUMENTED]. Confirmed that the "fat tail" at ±200% and bi-modal distributions are inherent properties of the Symmetric Percentage Change (SPC) metric (bounded [-200%, +200%]). Documented in README.
    - **Sign Flip / Polarity Issues**: [INVESTIGATING]. Persistent "sign flips" remain in specific grid cells. Hypothesis: Inherent noise in original models (InVEST/Spring) or edge-effects during 10km aggregation.
    - **Action Items**:
        1. Locate "Flip Identification Report" (CSV).
        2. Conduct Data Quality Audit on raw source rasters.
    - **Plan**: Postpone flip investigation. Prioritize "enhanced violins" on a new feature branch.
    - **New Feature**: Added "Hotspot Area Analysis" to `hotspot_extraction.qmd`. Implemented modular blocks for calculating and visualizing the percentage of land area classified as a hotspot per grouping variable..
- 2026-01-31: **Refactoring and Scope Refinement**:
    - **Goal**: Simplify the analysis pipeline for better clarity and maintenance.
    - **Action**: Initiated the refactoring of `analysis/Consolidation.qmd` by splitting it into `analysis/prepare_data.qmd` (preprocessing) and `analysis/process_data.qmd` (data processing).
    - **Discussion**: Explored merging `analysis/process_data.qmd` with `analysis/hotspot_extraction.qmd` to create a single, streamlined hotspot generation script.
    - **Scope Refinement**: Reviewed grouping variables in `hotspot_extraction.qmd`, deciding to focus on `income_grp`, `region_wb`, and `WWF_biome`, deferring country-level analysis (`nev_name`).
    - **Status**: Paused refactoring to consolidate the work plan. The immediate next step is to re-verify data structures, starting with the main processed file (`10k_change_calc.gpkg`), to ensure a stable base before proceeding with code changes.
 - 2026-02-02: **Hotspot Intensity & Area Calculation Fixes**:
    - Updated `analysis/hotspot_intensity.qmd` to correctly calculate hotspot intensity against the **total area** of spatial units (including pixels with no service data), ensuring accurate representation for regions like Tundra.
    - Implemented **Enrichment** metric (Observed Share / Expected Share) to highlight disproportionate hotspot concentrations.
    - Fixed bugs in enrichment calculation (grouping by service) and plotting logic for large groups.
    - Updated notebook documentation to reflect the three key metrics: Intensity, Global Share, and Enrichment.
 - 2026-02-02 (cont): **Multi-service Hotspot Analysis Fixes**:
    - Fixed `analysis/hotspot_multiservice.qmd` by adding the missing setup chunk to initialize project paths and functions.
    - Corrected typos in the overview text.
    - Verified that the notebook renders correctly and produces the "Hotness" and "Distribution" plots with alphabetical ordering.

## 2026-02-04: KS Analysis Finalization & Methodology Refinement

**Focus:** Finalizing the Kolmogorov-Smirnov (KS) analysis pipeline, refining visualizations, and synchronizing documentation.

**Key Changes:**
*   **KS Analysis (`analysis/KS_tests_hotspots.qmd`):**
    *   Optimized data pivoting to handle `_sum` vs `_mean` suffixes robustly, preventing memory bloat and missing variables.
    *   Implemented "signed power" transformations for Cliff's Delta and Heatmap plots to improve visibility of small but significant effects.
    *   Added narrative descriptions for all major plot types (Heatmap, ECDF, Directionality).
    *   Centralized configuration for service suffix patterns.
*   **Refined Groupings:**
    *   Removed `region_un` and `continent` from analysis loops to focus on `income_grp`, `region_wb`, and `WWF_biome`.
    *   Filtered "Antarctica" and "Seven seas" from all extraction and plotting steps.
*   **Documentation:**
    *   Updated `README.md` and `README_Methodology.md` to explicitly explain the robustness of Sum vs. Mean aggregation on equal-area grids.
    *   Synced `docs/codex_context.md` with the current file structure.
- 2026-02-10: **Land Cover Change Integration**:
    - Shifted focus to attributing hotspots with Land Cover dynamics.
    - Created `analysis/land_cover_change.qmd` to compute binary transitions (Natural/Transformed) from ESA 300m maps.
    - Updated `analysis/hotspot_extraction.qmd` to join LCC metrics to the hotspot geometry during export.

- 2026-02-13: **LCC Integration & Documentation**:
    - Validated `analysis/hotspot_extraction.qmd` logic for LCC overlap (Drivers of Change).
    - Waiting for `analysis/LC_change.qmd` extraction to complete.
    - Updated documentation (`README.md`, `README_Methodology.md`) to formally include the LCC pipeline and `diffeR` methodology.

- 2026-02-17: **LCC Pipeline Finalization**:
    - Fixed `fid` vs `grid_fid` conflict in `analysis/LC_change.qmd` preventing GPKG export.
    - Fixed grouping aggregation logic (joining attributes to long-format metrics) to successfully generate `lcc_summary_by_group.csv`.
    - Removed testing limits to allow full global extraction.
    - **Optimization**: Implemented chunked processing (50k cells/chunk) in `analysis/LC_change.qmd` to handle global scale (2.5M cells) without memory exhaustion and to enable checkpointing/resuming.
    - **Status**: `analysis/LC_change.qmd` is now stable and running for the full dataset.
