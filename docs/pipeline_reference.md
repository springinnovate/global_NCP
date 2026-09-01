---
title: "Pipeline Reference — Full Step List"
status: "Written 2026-08-31 during the 5-service sediment/coastal rerun, to make sure a long
  multi-tool chain doesn't silently drop a step. Update the Status column as you go; this is a
  living checklist, not a one-time snapshot."
---

One row per pipeline stage: what it does, what tool runs it, what it reads and writes, and where
it currently stands. `docs/runbook.md` has the narrative/gotcha detail for each step (Docker flags,
known bugs, partial-rerun rules) — this file is the at-a-glance sequence so nothing gets skipped.

## Legend
✅ done this session · 🔄 running / in progress · ⬜ not started · ⛔ blocked · ⏭️ intentionally
skipped for this rerun (see note)

## A. Raw data → canonical `10k_change_calc.gpkg`

| # | Step | Tool / script | Input | Output | Status |
|---|---|---|---|---|---|
| A1 | Raw zonal extraction (Docker) | `Python_scripts/summary_pipeline_landgrid.py` via Docker (`docs/runbook.md` Step 0) | Raw rasters + `analysis_configs/*.yaml` | `grid_10km_land_synth_zonal_<timestamp>.gpkg` | ⏭️ skipped this round — used an existing intermediate instead (A2) |
| A2 | Sediment + nitrogen raw levels via legacy-grid crosswalk | `scripts/merge_sediment_and_coastal_via_crosswalk.py` | `10k_grid_synth_all.gpkg` (Mar 2026, legacy grid) + `lc_grid_fid_to_master_fid_crosswalk.csv` | `10k_change_calc_DRYRUN_all3.gpkg` | ✅ done, validated, then **corrected and re-run**: found (via B1's LC-driver crash) that 1,609 of 155,131 many-to-one crosswalk groups have a clearly-worse secondary match (4,600-5,900m away) that a plain AVG would blend in; refined to keep only each group's best match(es) before aggregating, re-promoted to canonical. Same fix applied in `hotspot_extraction.qmd`'s own LC-driver join. |
| A3 | Backup current canonical file + hotspot outputs | manual copy | `10k_change_calc.gpkg`, `data/processed/hotspots/`, `data/processed/hotspots_5service/` | `*_BACKUP_2026-08-31.*` | ✅ done |
| A4 | Promote dry-run to canonical | manual rename/copy | `10k_change_calc_DRYRUN_all3.gpkg` | `10k_change_calc.gpkg` (overwritten, 1,522,073 rows confirmed, new columns present) | ✅ done |

## B. Path B (10km grid) downstream — hotspots, stats, attribution

| # | Step | Tool / script | Input | Output | Status |
|---|---|---|---|---|---|
| B1 | Hotspot extraction & export | `analysis/hotspot_extraction.qmd` (`run_hotspot_export: true`) | `10k_change_calc.gpkg`, `HOTS_CFG` | `data/processed/hotspots/**`, `_hotspots_index.csv` | ✅ done, verified: 219,138 unique 5-service hotspot cells (pct metric) — Sed_retention 52,315, N_retention 68,632, Nature_Access 67,731, C_Prot_service 2,658, Pollination 68,632. (Prior 8-service figure was 225,113 — different definition, not directly comparable.) 2 real bugs fixed along the way, both logged above (A2) and in `docs/runbook.md`. |
| B2 | Hotspot rasterization | `scripts/gdal_rasterize_hotspots_5service.R` | hotspot vector layers from B1 | hotspot rasters (for mapping + the Rich handoff in C1) | ⛔ **paused, needs a decision, not a bug fix**: the script's input (`data/processed/hotspots_5service/{metric}/global/hotspots_global_5service_{metric}.gpkg`) is a stale **July 28** file — old export/risk columns (`C_Risk, Sed_export, N_export`) and old water/access beneficiary categories (`count_water, count_access, combined_cross`), unrelated to today's retention/protection rerun. B1's real output lives in a different directory (`data/processed/hotspots/`) with different columns (`Sed_retention, N_retention, Nature_Access, C_Prot_service, Pollination, hotspot_count, count_deg_combo, count_rec_combo`). Needs a decision on which columns Rich actually needs before writing/adapting a rasterization step — not guessed at unilaterally, given past confusion from ambiguous Rich handoffs (see WORKLOG's pct/abs incident). Skipped ahead to B3, which doesn't depend on this. |
| B3 | Spatial clustering & synthesis | `analysis/hotspot_synthesis.qmd` | B1 output | `hotspot_area_stats.csv`, `hotspot_multiservice_stats.csv`, `hotspot_pop_exposure.csv`, `regional_subsets/*` | ✅ done, verified: `hotspot_area_stats.csv` confirmed showing the correct 5-service set (`C_Prot_service, N_retention, Nature_Access, Pollination, Sed_retention`) after fixing a 3rd independent stale-config copy (this qmd recomputes hotspots from its own separate `HOTS_CFG`, not B1's results). |
| B4 | Socioeconomic / KS tests | `analysis/KS_tests_hotspots.qmd` | B1/B3 output | KS-test summary tables (feeds paper's Table on hotspot vs. background covariates) | ✅ done, verified: all 40 service×covariate combinations have real data (no more `n_hot=0`), 38/40 (95%) significant at p_adj<0.05. Both non-significant are coastal (amount and ratio) vs. agricultural plot size — consistent with the old paper's own narrative that coastal hotspots are decoupled from agricultural landscapes, not a red flag. |
| B5 | Attribution gap (true union) | `scripts/compute_attribution_true_union.R` | B1 output + LC grid crosswalk (Prerequisite) | `lcc_driver_magnitude_summary.csv`, `lcc_es_hotspot_per_driver_risk.csv`, `lcc_es_hotspot_true_union.csv` | ✅ done, verified — no config drift here (reads B1's output directly, already had the crosswalk many-to-one fix applied from an earlier session). New attribution gap: **63.77%** (36.23% overlap with LCC drivers), risk ratio 8.23, odds ratio 12.34. Old figure was 65.8% — different definition, same ballpark. |
| B6 | Native-grid mapping figures | `scripts/mapping/make_global_change_5panel.R` (the actual generator for `global_change_5panel_en.png`, the paper's "Global Pattern of Change" figure) | `10k_change_calc.gpkg` directly (Path B, 10km — **not Path A**, corrected an earlier mischaracterization in this doc and the paper's own placeholder callout) | `outputs/plots/colombia_report/global_change_5panel_{en,es}.png` | ✅ done: fixed stale service list (export/risk → retention/protection amounts), regenerated. **User decision 2026-09-01**: coastal protection dropped from this panel — data is correct (49,291 valid cells, full range) but a 1-cell-wide coastline fringe is invisible at full-globe scale next to the other 4 continental-footprint services (confirmed, not a rendering bug). Now a clean 4-panel (2×2) figure; caption updated to note the coastal omission is a visibility choice, not an analysis exclusion. Regional zoom insets for coastal hotspot concentration flagged as a possible future Annex addition, not built. Paper re-rendered. `make_native_change_figure.R` (the export/ratio-paired all-8-service reference figure) still needs a scope decision from the user — pending. `make_colombia_report_maps.R` not yet checked. |
| B7 | **New**: centralize service config | none yet — proposed 2026-09-01 | current 5+ independent copies (B1/B3/B4/B6 and one more mapping script) | a single `R/service_config.R` all scripts source | ⬜ proposed, not started — direct fix for the config-drift pattern that caused 3 of today's bugs (B1/B3/B4). See memory `project_pipeline_flexibility.md` for full writeup and the related "self-service dashboard" and "direction-flexibility" future-work notes. |

## C. External round-trip (Rich)

| # | Step | Tool / script | Input | Output | Status |
|---|---|---|---|---|---|
| C1 | Send new hotspot rasters to Rich | manual (Drive/Slack), per `docs/hotspots_rasters_data_dictionary_for_rich.md`-style handoff | B2 output | shared folder | ⬜ — **new dependency identified this session**, not in the old handoff checklist |
| C2 | Rich's beneficiary/downstream buffer rerun | Rich's own pipeline | C1 rasters | water/access/combined beneficiary rasters | ⬜ external, waiting |
| C3 | Recompile beneficiary exposure | `Python_scripts/extraction_script.py`, `analysis/plot_multiplier_effect.R` | C2 output | `exposure_comparison_compiled.csv`, Figure 9 | ⬜ blocked on C2 |

## D. Path A (300m pixel-level) — NOT reachable this round, but the tool is now identified

| # | Step | Tool / script | Input | Output | Status |
|---|---|---|---|---|---|
| D0 | Identify the Path A generator | — | — | Found 2026-08-31: it's a **separate sibling repo**, `c:\projects\zonal_stats_toolkit` (not part of `global_NCP`) — `process_regional_diff.py`, `compare_and_plot_changes.R`, `compare_paths_bars.R`, configs (`global_ncp_diff_consolidated.ini` etc.), and its own `output_plots_diff/` that mirrors exactly what's copied into `global_NCP/outputs/plots/output_plots_diff/`. This resolves the "generator not found" note from earlier this session. | ✅ found |
| D1 | Path A biome/region/country/income-group aggregation | `zonal_stats_toolkit` (see D0) | raw 300m rasters — need to re-request from Rich: USLE (1992/2020), sediment export (1992/2020), N export (2020; 1992 already local), N retention (1992/2020) | `output_plots_diff/*_map_data.csv` → copy into `global_NCP/outputs/plots/output_plots_diff/` → Figure 2 + Annex trajectory figures | ⛔ blocked on raw rasters from Rich — **user will re-request, this time explicitly for the zonal_stats_toolkit Path A rerun, not just the Path B sediment fix** |
| D2 | Load-weighted ratio aggregation fix | (extends D1, in `zonal_stats_toolkit`) | raw numerator/denominator per pixel | corrected ratio panels in Figure 2/Annex | ⛔ blocked on D1; also pending the Becky/Steve decision on whether ratios stay in the paper at all |
| D4 | **Scope decision, 2026-08-31**: drop export/risk panels from Figure 2 | `paper_draft_5service.qmd` (Methods text) | already updated to say export/risk are computational inputs only, never reported directly | Figure 2 currently pairs each service's export/risk panel next to its ratio panel (per an earlier WORKLOG entry) — conflicts with this decision, needs to change **when Figure 2 is eventually rebuilt** (whatever generates the paired-row layout in `zonal_stats_toolkit`/its consuming script). Not urgent while D1 is blocked, but don't let the old paired layout regenerate unchanged. | ⬜ apply at D1 rebuild time |
| D3 | Paper placeholder callout | `paper_draft_5service.qmd` | — | explicit note that Figure 2/trajectory maps are placeholders pending D1 | ✅ done 2026-08-31 |

## E. Paper updates

| # | Step | What | Status |
|---|---|---|---|
| E1 | Remove "sediment blocked" language from the two Methods callouts (~line 97, ~line 291 of `paper_draft_5service.qmd`) | text edit | ⬜ do after B-chain succeeds |
| E2 | Update Results section numbers (hotspot counts, KS tests, attribution gap) with real Path-B output | text edit | ⬜ blocked on B1-B5 |
| E3 | Figure 2 / regional trajectory maps and their caveat callout | — | ⛔ stays on prior export/risk data until D1 is solved; caveat callout should say so explicitly, not be removed |
| E4 | Ratio-in-paper decision (keep with load-weighted fix, or drop to the book) | needs Becky/Steve input | ⬜ email drafted, not yet sent |
| E5 | Re-render paper | `quarto render docs/manuscript/paper_draft_5service.qmd --to html` | ⬜ after E1/E2 |

---

**Rule of thumb for using this file**: update a row's Status the moment a step finishes or a
decision changes — don't batch it for later, that's exactly how a step gets lost. If a new
dependency turns up mid-run (like C1 did today), add a row rather than mentioning it only in chat.
