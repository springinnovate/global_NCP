# Data versions: what is current, what is superseded

Started 2026-10-07 after the population-exposure numbers turned out to come from a superseded run
that sat next to the current one. Rule: anything a current result reads is listed under **Current**;
everything else moves to `data/archive/superseded/<date>_<label>/` with a README saying what it was
and what replaced it. Cell counts and denominators: `docs/cell_universe.md`.

## Current (2026-10-07)

| What | Path | Date | Notes |
|---|---|---|---|
| Grid with change metrics | `data/processed/10k_change_calc.gpkg` | 08-31 | Input to everything on the 10 km grid |
| Production hotspots | `data/processed/hotspots/pct/global/hotspots_global_pct.gpkg` | 09-03 | 189,932 cells, five services, SPC, top 5% |
| Long table cache | `data/processed/plt_long.rds` | 09-03 | Written by `hotspot_extraction.qmd` |
| Beneficiary run (connected people) | `data/processed/beneficiaries/run_2026-07-29_5service/` | 07-29 | Rich's run, LandScan 2023. Built on the July five-service hotspots, which match production (189,918 of 189,932 cells; the rest are pollination ties). Has its own README. Was `hotspots_5service/rasters_5_var/` until 10-07. |
| Hotspot rasters sent for that run | `.../run_2026-07-29_5service/_input_hotspot_rasters_2026-07-28/` | 07-28 | Provenance of the beneficiary run |
| Prevalence table | `data/processed/tables/hotspot_area_stats.csv` | 10-07 | `analysis/rebuild_hotspot_area_stats.R`, on the evaluated universe |
| Exposure tables | `outputs/tables/exposure_by_overlap.csv`, `exposure_by_income.csv` | 10-07 | From the 07-29 run + LandScan 2023 |
| Within-hotspot change | `outputs/tables/hotspot_spc_within_hotspots.csv` | 10-06 | |
| KS tests (hotspot vs background) | `data/processed/tables/ks_results_hot_vs_non.csv` | 09-08 | |
| Beneficiary-mask KS (phase 4) | `data/processed/tables/ks_results_beneficiary_masks*.csv`, `beneficiary_mask_coverage_10km.csv` | 08-07/10 | Masks come from the 07-29 run, so the hotspot set is current; covariates unchanged since. Recheck before citing, but a rerun is likely unnecessary. |
| LCC overlap (Supplement) | `data/processed/tables/lcc_*.csv`, `lc_grid_fid_to_master_fid_crosswalk.csv` | 09-03 / 07-08 | |
| Population (exposure) | `~/data/global_ncp/Raw/Beneficiaries/Landscan/landscan-global-2023-assets/landscan-global-2023.tif` | | World total 7,982 M |

## Superseded: archived 2026-10-07

Moved (nothing deleted). Each folder has a README with what it was and what replaced it.

| What | Now in |
|---|---|
| June beneficiary run, 8-variable hotspots (225,113 cells) | `data/archive/superseded/2026-06_beneficiaries_8var/` |
| June exposure tables and figures (`exposure_comparison*.csv`, `multiplier_summary_*.csv`, dumbbells, `pop_exposure_*.png`, `multiplier_effect.png`) and their script `plot_multiplier_effect.R` | `archive/outputs/2026-06_exposure_8var/` and `archive/scripts/` (tracked) |
| July five-service hotspot gpkg (backup; provenance of the 07-29 run) | `data/archive/superseded/2026-07_hotspots_5service_july/` |
| Retention/protection-era hotspots (`hotspots_5service/{pct,abs}`, `rasters_for_rich`, index) | `data/archive/superseded/2026-09_retention_era_hotspots/` |
| May hotspot rasters (`hotspot_count*.tif`, `binary_hotspots`, both `service_rasters`) | `data/archive/superseded/2026-05_hotspot_rasters/` |
| 08-31 backups (`10k_change_calc_BACKUP`, `hotspots_BACKUP`) | `data/archive/superseded/2026-08-31_backups/` |
| 08-31 dry runs (`10k_change_calc_DRYRUN_*`) | `data/archive/superseded/2026-08-31_dryruns/` |
| Old prevalence tables | `data/archive/superseded/tables/` |

Still pointing at archived files (update or retire): book chapters 02 and 06,
`scripts/mapping/make_annex_figures.R`, `scripts/audit_claims.R`,
`Python_scripts/extract_book_data_fills.py`, `scripts/mapping/make_paper_supplement_maps.py` (May
`service_rasters`, so the July supplement PDF is stale), `scripts/mapping/make_5service_overlap_*.R`
and `scripts/extract_hotspots.R` (retention-era `hotspots_5service`), `analysis/hotspot_synthesis.qmd`
(`pop_exposure_*` from the June `tables/hotspot_pop_exposure.csv`).

## To check before deciding

`10k_change_calc_epsg8857.gpkg` (05-14), `hotspots_global_pct_epsg8857.gpkg` (09-03),
`10k_grid_synth_all.gpkg` (03-18), `10k_lcc_metrics.gpkg` (02-18), `10k_lcc_granular_metrics.gpkg`
(05-07), `grid_10km_land_synth_zonal_2026_06_03_15_33_39.gpkg` (06-04),
`hotspots/{abs,drivers,drivers_by_group,rasters}/`, the per-group hotspot folders under
`hotspots/pct/`, `tables/_deprecated/`, `tables/regional_subsets/`, the Colombia tables (move with the
Colombia work to the personal repo). For each: grep the scripts and qmds for the path; unreferenced
-> archive, referenced only by superseded scripts -> archive both.

## Procedure

1. For each superseded item, grep for its path across `analysis/`, `scripts/`, `R/`, `Python_scripts/`
   and `docs/`. Repoint or retire any current script that still reads it.
2. Move (not delete) to `data/archive/superseded/<date>_<label>/` with a README: what it was, which
   run or definition produced it, what replaced it, and why it is kept.
3. Rename the current beneficiary run to a name that says what it is (e.g.
   `data/processed/beneficiaries/run_2026-07-29_5service/`) and update the scripts that read it.
4. Rerun `analysis/verify_cell_universe.R` and the exposure scripts to confirm nothing broke.
