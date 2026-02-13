# Worklog — Global NCP Hotspots

## Current focus
- **Completed** KS Analysis, Hotspot Intensity, and Multi-service workflows (v1.0.2).
- **Completed** Re-calculating core ecosystem service ratios and difference rasters.
- **Completed** LCC Pipeline Development: Finalized `analysis/LC_change.qmd` using `diffeR` for robust transition metrics (1992-2020).
- **Active** Execution: Running LCC extraction on the 10km grid (currently processing).
- **Active** Synthesis: Integrating LCC metrics into `analysis/hotspot_extraction.qmd` to visualize the overlap between ES Hotspots and Land Conversion.

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
- analysis/Consolidation.qmd
- analysis/hotspot_extraction.qmd
- analysis/KS_tests_hotspots.qmd

## Known issues / gotchas
- Hotspot rules (loss vs gain services) must remain centralized in `HOTS_CFG`.
- Be careful not to mix interpretive direction (good/bad change) with magnitude summaries.
- Do not use ChatGPT Codex Connector on lilling (auth persists after uninstall).

## Next steps (short horizon)
1. **Await LCC Results**: Wait for `analysis/LC_change.qmd` to finish generating `processed/10k_lcc_metrics.gpkg`.
2. **Render Hotspots Report**: Run `analysis/hotspot_extraction.qmd` to generate the new "Drivers of Change" overlap plots.
3. **Reporting**: Incorporate the LCC overlap findings into the final slide deck.
4. **Coastal Risk Reduction**: Follow up on the pending calculation issue (waiting for pipeline fix).

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
