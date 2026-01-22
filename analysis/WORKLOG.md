# Worklog — Global NCP Hotspots

## Current focus
- Re-calculating and verifying core ecosystem service ratios (sediment, nitrogen) for the 1992-2020 period.
- Re-establishing continuity using explicit context and worklog files.
- **Completed** Generating difference rasters (2020-1992) for all services to support updated zonal statistics.
- **Completed** integration of `zonal_stats_toolkit` for synthesis (replacing legacy pipelines).
- Current/complementary analysis: Running zonal synthesis with the new toolkit.
- Revisiting Hotspots Analysis and KS tests: fixing violin limits,, adjusting sample sizes, calcualte percentages of areas and statistical analysis
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
1. Finalize and verify the newly calculated ratio rasters (Sediment and Nitrogen Retention) and their corresponding difference rasters. This is the current task.
2. Configure `zonal_stats_toolkit` to process all difference rasters against the 10km grid.
3. Update the analysis configuration YAML files (in `analysis_configs/`) to use the new difference rasters as inputs for the zonal statistics pipeline.

## Future Tasks (Long-term)
1.  **Adapt analysis for multi-temporal data:** Adapt analysis to handle updated modeled ES layers and multiple points in time (beyond bi-temporal T0, T1). Strategize for incorporating multi-temporal data.
2.  **Quantify hotspot vs. non-hotspot change:** Develop a method to quantify and visualize the share of total change (from bar plots) that occurs within hotspots versus outside of them, possibly using stacked bar plots.

## Session notes
- 2026-01-05: Created `doc/ai_context.md` and `doc/ai_context.min.md`. Migrated AI workflow to Copilot after Codex auth failure. Sign flip issue remains unresolved and explicitly tracked here.
- 2026-01-06: Fixed critical bug in `Consolidation.qmd` where `c_fid` was dropped, causing "No hotspots found" errors downstream. Resolved file casing conflict (`Consolidation.Qmd` vs `.qmd`). Regenerated `10k_change_calc.gpkg` and verified ID consistency.
- 2026-01-06 (cont): Resolved "No hotspots found" by normalizing service names in `hotspot_extraction.qmd` (lowercase -> canonical lookup) to match `HOTS_CFG`. Confirmed successful export with diagnostic logs.
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
    - **New Feature**: Added "Hotspot Area Analysis" to `hotspot_extraction.qmd`. Implemented modular blocks for calculating and visualizing the percentage of land area classified as a hotspot per grouping variable.