# Repository reorganization plan (HANDOFF milestone step 2)

Working doc for the `housekeeping` branch. Inventory taken 2026-10-06; target layout and move
stages are proposals until decided. Every move: grep for references, update paths, re-render or
re-run the affected file, then the next stage.

## 1. Inventory (2026-10-06)

### Tracked files

664 tracked files, **442 MB on disk** (the earlier 134 MB estimate was wrong). Git history packs to
990 MB. By extension: `.html` 199 MB, `.png` 181 MB, `.docx` 31 MB, `.pdf` 13 MB, everything else
under 15 MB. Rendered documents, not figures, are the largest single block.

| Top level | Files | Role |
|---|---|---|
| `outputs/` | 281 | figures (`plots/` 199, `maps/` 27), tables (14), `data_package/` (15, duplicates most of `tables/`), `workflow_files/libs` (17) |
| `docs/` | 121 | paper + book (`manuscript/` 39), external reports (`reports/` 33), presentations (11), archive (20), applications (5), reference docs (12) |
| `analysis/` | 82 | 10 pipeline qmds + their rendered HTML and `_files/` (51 files), paper patch scripts, scratch, notes |
| `scripts/` | 49 | `mapping/` 30 figure scripts (paper, book and Colombia/presentation mixed), 15 one-off scripts, archive |
| `man/`, `R/`, `DESCRIPTION`, `NAMESPACE`, `inst/` | 68 | the `EcoZonal` R package; notebooks load it with `devtools::load_all()`, so this layout stays at the root |
| `Python_scripts/` + `_tests/` | 27 | zonal pipeline (`summary_pipeline_landgrid.py`), bitemporal change pilot, one-off reruns |
| `analysis_configs/` | 12 | YAML/INI configs for the Python pipeline and Path A reruns |
| `workflow_files/` | 16 | `libs/` only (Quarto HTML dependencies, regenerable) |

### Who uses the tracked non-source files

Each tracked non-source file (figures, tables, rendered docs) was matched by basename against all
tracked source files (qmd, R, py, sh, md, yaml). The top consumer wins:

| Consumer | Files | MB |
|---|---|---|
| Paper (`paper_draft_5service.qmd`) | 30 | 16 |
| Book (`docs/manuscript/` other than the paper) | 56 | 34 |
| Presentations | 26 | 17 |
| External reports (Colombia/CLEC, phase 4) | 29 | 4 |
| Only a script writes it, no document reads it | 23 | 23 |
| Only docs/markdown mention it | 40 | 21 |
| **No source mentions it** | **124** | **313** |

Basename matching overcounts use where the same name repeats (e.g. `boxplots_pct.png` in five
folders), so treat the categories as approximate. The 124 unreferenced files are listed in the
appendix. The large ones are rendered documents, not figures.

### Specific findings

- **Rendered docs are tracked although the repo's own `.gitignore` excludes them.**
  `docs/presentations/presentation.html` (31 MB) and `sandra_valenzuela_colombia_case.html`
  (11 MB) match `docs/presentations/*.html`, and the six `outputs/plots_archive_mar20/signed_bars/`
  files match `outputs/plots_archive_mar20`. They were added before the rules. Also tracked and
  regenerable: `docs/methodology.html` (77 MB), `docs/archive/manuscript_pre_5service/paper_draft.html`
  (50 MB) + `.docx` (30 MB), the four `analysis/*.html` and their 51 `_files/` assets, the report
  HTMLs. Untracking these removes ~200 MB from the working tree (history keeps them).
- **The book reads figures from another repo**: chapters reference
  `../../../../zonal_stats_toolkit/output_plots_diff/*_combined_diffs.png`. The paper uses the copies
  in `outputs/plots/output_plots_diff/`. The book should point there too.
- **`outputs/data_package/` duplicates `outputs/tables/`** for 8 of 14 CSVs (multiplier summaries,
  exposure comparison, reclassification table). Which copy is current needs a diff before choosing.
- **`home/` is R's `HOME` on this machine**, set by `.Renviron` (`HOME`, `R_USER`), not a stray
  copy. It holds 229 `Rtmp*`/`Rscript*` temp dirs (safe to delete, already gitignored) and
  `home/jeronimo/data/global_ncp/` (8.8 GB, files dated 2026-02 to 2026-04): an older snapshot of
  `data/` (`10k_change_calc.gpkg` there is 602 MB vs 978 MB in `data/processed/`, dated 08-31).
  Nothing in the repo reads `home/jeronimo` relative to the repo; the hardcoded
  `/home/jeronimo/...` paths in `hotspot_extraction.qmd`, `KS_tests_hotspots.qmd`,
  `inst/config/paths.yml` and `inst/python/DEM_Mask.py` are leftovers from the Linux machine.
  Candidate for deletion after a check that no file in it is the only copy.
- **`inst/config/` and `inst/python/` look unused**: no source file references `inst/config`,
  `paths.yml` or `inst/python`. `inst/python/` holds older versions of scripts that now live in
  `Python_scripts/` (`rasterize_coastal.py`, `summary_pipeline.py`) plus a file named
  `import rasterio.py`.
- **`analysis/` mixes five kinds of file**:
  pipeline notebooks (`process_data`, `hotspot_extraction`, `hotspot_synthesis`,
  `KS_tests_hotspots`, `KS_tests_beneficiary_masks`, `LC_change*` x4, `viz_granular_lcc`);
  paper patch scripts that bypass the notebooks (`rebuild_hotspot_area_stats.R`,
  `hotspot_metric_overlap.R`, `attribution_null_model.R`, `update_n_ret_ratio_map_data.R`,
  `plot_multiplier_effect.R`); audit (`paper_numbers_inventory.py`); monitoring
  (`monitor_lcc.R`, `monitor_lcc_progress.sh`); scratch (`scratch_abstract_stats.R`,
  `scratch_by_*_pct_chg.csv`); notes (`WORKLOG.md`, `running_notes.md`, `LCC_column_dictionary.md`).
- **`scripts/mapping/` mixes paper and external-venue scripts**: 10 of the 30 are
  Colombia/CLEC-specific (`make_colombia_*`, `make_lac_*`).
- **Five `archive/` folders** (`docs/archive`, `R/archive`, `Python_scripts/archive`,
  `scripts/archive`, `analysis_configs/archive`).
- **Untracked clutter at the root**: `NCP_Global_Hotspots_Deliverables.zip` (212 MB, June),
  `tmp_power_test.nc`, `_book/` (480 MB, June, a stale render of the book), `summary_pipeline_workspace/`
  (0.6 MB), `summary_pipeline_workspace_ha/` (587 MB), `MONTHLY_REPORT.md` (ignored on purpose),
  `Python_scripts/swy_borneo_run.zip`.
- **Python environment is undocumented locally**: the README describes the Docker image for the
  zonal pipeline but not the project `.venv` used for local scripts
  (`.venv/Scripts/python.exe`; there is no system `python` on this machine).

## 2. Proposed target layout

```
R/ man/ DESCRIPTION NAMESPACE inst/   EcoZonal package (unchanged place; prune inst/)
Python_scripts/  Python_scripts_tests/ zonal pipeline + Python tools (rename deferred to step 6)
analysis_configs/                      pipeline configs
analysis/                              pipeline notebooks only (become pipeline/ in step 6)
  checks/                              paper patch + audit scripts until step 6 absorbs them
paper/                                 paper qmd, templates, _quarto.yml if needed
book/                                  index.qmd, _quarto.yml, chapters/
scripts/mapping/                       figure scripts used by the paper or book
outputs/                               regenerable products only: figures + tables the paper/book read
docs/                                  documentation only (runbook, pipeline_reference, catalog,
                                       how_to_extend, methodology.md, HANDOFF, this plan)
archive/                               one archive for retired code and docs
```

External-venue material (Colombia/CLEC report and deck, IDB-WWF workshop, phase 4 report,
applications, and the 10 Colombia mapping scripts with their `outputs/plots/colombia_report/`
figures) either moves to `archive/external/` or leaves the repo (separate repo or Drive).

Rendered HTML/DOCX/PDF stay out of git everywhere; add `docs/**/*.html`, `analysis/*.html`,
`analysis/*_files/` and `outputs/workflow_files/` to `.gitignore` and `git rm --cached` them.

## 3. Decisions (approved 2026-10-06)

1. **External-venue material leaves this repo.** The user keeps working on it in a personal copy
   of the repo on their own GitHub account (Colombia/CLEC report and decks, IDB-WWF workshop,
   applications, phase 4 report, Colombia mapping scripts and figures). Create that copy from
   `main` before stage 4 removes the material here.
2. **Paper and book move** to root-level `paper/` and `book/`. The living paper is
   `docs/manuscript/paper_draft_5service.qmd`; every other paper draft is stale.
3. **Rendered HTML/DOCX are untracked** everywhere (history not rewritten).
4. `home/jeronimo/data/` (8.8 GB): delete after checking no file in it is the only copy. Pending.
5. `CLAUDE.md` stays untracked for now.
6. **One figure per concept across paper, book and presentation.** Where the book or the
   presentation shows something the paper also shows, they use the paper's figure file (the
   paper is the reference for the latest version). Applies in stage 6 and milestone steps 4-5.
7. **Stale message drafts** are deleted, not archived. The paper went out 10-05, so older
   correspondence is moot. Kept: `docs/justin_devstack_outreach_2026-09-08.draft.md` (milestone
   step 8) and `docs/manuscript/justin_ee_correspondence_provenance_2026-08-31.draft.md` (open
   paper comment on the correspondence-table citation).
8. **The root README is rewritten** once the layout settles (after stage 6), since most of its
   paths and its pipeline description are out of date.

## 4. Staged moves

Each stage is one commit, after a render/run check.

1. **Done 10-06 (uncommitted).** Untracked 108 rendered files (analysis HTML + `_files/`, docs
   HTML/DOCX, `workflow_files/libs`, `outputs/workflow_files/libs`, `plots_archive_mar20`);
   `.gitignore` extended. Kept tracked on purpose:
   `docs/manuscript/chapters/07-regional-profiles_files/figure-html/` (the lab presentation embeds
   two of those PNGs; drop once the presentation uses paper figures),
   `chapters/_header.html` (a Quarto include), `templates/callout-reference.docx` (paper
   template).
2. **Done 10-06.** Deleted `tmp_power_test.nc`, the stale root `_book/` (June render) and 324 R
   temp dirs in `home/`. Left in place pending a decision: `NCP_Global_Hotspots_Deliverables.zip`
   (212 MB, June 15; a book render plus data, possibly the only copy of what was delivered),
   `summary_pipeline_workspace_ha/` (587 MB, taskgraph workspace), `home/jeronimo/data/`.
3. **Done 10-06 (uncommitted).** Five archive folders consolidated into `archive/{docs,R,python,
   scripts,analysis_configs}`; references in sub-READMEs, `methodology.md`, `agu_abstract_2026.md`,
   `colombia_capability_portfolio.md` updated. Stale message drafts deleted (decision 7), including
   two tracked ones.
4. External-venue material out of the repo (after the personal copy exists).
5. Split `analysis/` (scratch out, checks into `analysis/checks/`); update references in
   `analysis/README.md`, WORKLOG pointers, HANDOFF.
6. Paper/book move to `paper/` and `book/`; repoint the book's `zonal_stats_toolkit` figures to
   `outputs/plots/output_plots_diff/`; make book figures match the paper's (decision 6); render both.
7. Prune unreferenced outputs (milestone step 3; appendix list, check scripts that build paths by
   pasting strings first) and resolve the `data_package/` vs `tables/` duplicates.
8. Prune `inst/`; rewrite the root README (decision 8), including the `.venv` and Docker setup.

## Appendix: tracked files no source file mentions by name (124)

Some are built from pasted paths inside scripts, so check the generating script before removing.

- `.Rbuildignore` (0.0 MB)
- `DESCRIPTION` (0.0 MB)
- `LICENSE` (0.0 MB)
- `analysis/KS_tests_beneficiary_masks.html` (0.1 MB)
- `analysis/KS_tests_hotspots.html` (0.1 MB)
- `analysis/hotspot_extraction.html` (0.1 MB)
- `docs/Global NCP Time Series Update.txt` (0.1 MB)
- `docs/archive/manuscript_pre_5service/paper_draft.docx` (30.8 MB)
- `docs/archive/manuscript_pre_5service/paper_draft.html` (51.5 MB)
- `docs/archive/manuscript_pre_5service/workflow.html` (0.0 MB)
- `docs/archive/swy_becky_meeting.html` (4.6 MB)
- `docs/manuscript/literature_review_novelty_claim.ris` (0.0 MB)
- `docs/methodology.html` (80.7 MB)
- `docs/presentations/presentation.html` (31.8 MB)
- `docs/presentations/sandra_valenzuela_colombia_case.html` (11.2 MB)
- `docs/reports/_output_map.html` (3.4 MB)
- `docs/reports/colombia_clec/colombia_clec_report.html` (4.4 MB)
- `docs/reports/colombia_clec/colombia_clec_report_en.html` (4.4 MB)
- `docs/templates/regional_report_template.html` (2.0 MB)
- `outputs/data_package/multiplier_summary_WWF_biome.csv` (0.0 MB)
- `outputs/maps/native10km_panels/c_risk_abs.png` (0.5 MB)
- `outputs/maps/native10km_panels/c_risk_pct.png` (0.5 MB)
- `outputs/maps/native10km_panels/c_risk_red_ratio_abs.png` (0.5 MB)
- `outputs/maps/native10km_panels/c_risk_red_ratio_pct.png` (0.5 MB)
- `outputs/maps/native10km_panels/n_export_abs.png` (1.5 MB)
- `outputs/maps/native10km_panels/n_export_pct.png` (1.7 MB)
- `outputs/maps/native10km_panels/n_ret_ratio_abs.png` (1.8 MB)
- `outputs/maps/native10km_panels/n_ret_ratio_pct.png` (1.7 MB)
- `outputs/maps/native10km_panels/nature_access_abs.png` (0.8 MB)
- `outputs/maps/native10km_panels/nature_access_pct.png` (1.3 MB)
- `outputs/maps/native10km_panels/pollination_abs.png` (1.0 MB)
- `outputs/maps/native10km_panels/pollination_pct.png` (1.3 MB)
- `outputs/maps/native10km_panels/sed_export_abs.png` (1.2 MB)
- `outputs/maps/native10km_panels/sed_export_pct.png` (1.8 MB)
- `outputs/maps/native10km_panels/sed_ret_ratio_abs.png` (1.8 MB)
- `outputs/maps/native10km_panels/sed_ret_ratio_pct.png` (1.8 MB)
- `outputs/plots/colombia_report/colombia_change_c_risk.png` (0.0 MB)
- `outputs/plots/colombia_report/colombia_change_n_export.png` (0.1 MB)
- `outputs/plots/colombia_report/colombia_change_nature_access.png` (0.1 MB)
- `outputs/plots/colombia_report/colombia_change_pollination.png` (0.1 MB)
- `outputs/plots/colombia_report/colombia_change_sed_export.png` (0.1 MB)
- `outputs/plots/colombia_report/colombia_cna_coastal.png` (0.1 MB)
- `outputs/plots/colombia_report/colombia_cna_coastal_flat.png` (0.0 MB)
- `outputs/plots/colombia_report/colombia_cna_nature_access.png` (0.1 MB)
- `outputs/plots/colombia_report/colombia_cna_nature_access_flat.png` (0.1 MB)
- `outputs/plots/colombia_report/colombia_cna_nitrogen.png` (0.1 MB)
- `outputs/plots/colombia_report/colombia_cna_nitrogen_flat.png` (0.1 MB)
- `outputs/plots/colombia_report/colombia_cna_pollination.png` (0.1 MB)
- `outputs/plots/colombia_report/colombia_cna_pollination_flat.png` (0.1 MB)
- `outputs/plots/colombia_report/colombia_cna_sediment.png` (0.1 MB)
- `outputs/plots/colombia_report/colombia_cna_sediment_flat.png` (0.1 MB)
- `outputs/plots/drivers/bar_driver_overlap_abs.png` (0.1 MB)
- `outputs/plots/drivers/bar_driver_overlap_pct.png` (0.1 MB)
- `outputs/plots/drivers/scatter_Crop_Exp_abs.png` (0.3 MB)
- `outputs/plots/drivers/scatter_Crop_Exp_pct.png` (0.3 MB)
- `outputs/plots/drivers/scatter_Forest_Loss_abs.png` (0.3 MB)
- `outputs/plots/drivers/scatter_Forest_Loss_pct.png` (0.3 MB)
- `outputs/plots/drivers/scatter_Grassland_Gain_abs.png` (0.2 MB)
- `outputs/plots/drivers/scatter_Grassland_Gain_pct.png` (0.3 MB)
- `outputs/plots/drivers/scatter_Grassland_Loss_abs.png` (0.2 MB)
- `outputs/plots/drivers/scatter_Grassland_Loss_pct.png` (0.3 MB)
- `outputs/plots/drivers/scatter_Net_Natural_Change_abs.png` (0.3 MB)
- `outputs/plots/drivers/scatter_Urban_Exp_abs.png` (0.3 MB)
- `outputs/plots/drivers/scatter_Urban_Exp_pct.png` (0.3 MB)
- `outputs/plots/exposure_multiplier_dumbbell.png` (0.2 MB)
- `outputs/plots/hotness_dist_nev_name.png` (0.1 MB)
- `outputs/plots/hotness_income_grp.png` (0.1 MB)
- `outputs/plots/hotness_nev_name.png` (0.1 MB)
- `outputs/plots/hotness_region_wb.png` (0.1 MB)
- `outputs/plots/hotness_wwf_biome.png` (0.2 MB)
- `outputs/plots/intensity/hotspot_coverage_nev_name.png` (1.1 MB)
- `outputs/plots/intensity/hotspot_relative_intensity_nev_name.png` (1.1 MB)
- `outputs/plots/intensity/hotspot_share_income_grp.png` (0.1 MB)
- `outputs/plots/intensity/hotspot_share_nev_name.png` (1.1 MB)
- `outputs/plots/intensity/hotspot_share_region_wb.png` (0.2 MB)
- `outputs/plots/intensity/hotspot_share_wwf_biome.png` (0.3 MB)
- `outputs/plots/ks/ecdf/ecdf_fields_mehrabi_2017_mean_log1p.png` (0.1 MB)
- `outputs/plots/ks/ks_bars_GHS_POP_E2020_GLOBE_sum.png` (0.1 MB)
- `outputs/plots/ks/ks_bars_fields_mehrabi_2017_mean.png` (0.1 MB)
- `outputs/plots/ks/ks_bars_hdi_raster_predictions_2020_mean.png` (0.1 MB)
- `outputs/plots/ks/ks_bars_rast_adm1_gini_disp_2020_mean.png` (0.1 MB)
- `outputs/plots/ks/ks_bars_rast_gdpTot_1990_2020_30arcsec_2020_sum.png` (0.1 MB)
- `outputs/plots/ks_beneficiary_masks/ks_bars_GHS_POP_E2020_GLOBE_sum.png` (0.1 MB)
- `outputs/plots/ks_beneficiary_masks/ks_bars_hdi_raster_predictions_2020_mean.png` (0.0 MB)
- `outputs/plots/ks_beneficiary_masks/ks_bars_rast_adm1_gini_disp_2020_mean.png` (0.0 MB)
- `outputs/plots/ks_beneficiary_masks/ks_bars_rast_gdpTot_1990_2020_30arcsec_2020_sum.png` (0.1 MB)
- `outputs/plots/main_report/abs/income_grp/boxplots_income_grp_abs_volumetric.png` (0.2 MB)
- `outputs/plots/main_report/pct/income_grp/boxplots_income_grp_pct_volumetric.png` (0.2 MB)
- `outputs/plots/main_report/ratios/income_grp/boxplots_income_grp_ratios.png` (0.2 MB)
- `outputs/plots/maps/attribution_by_service/map_attr_C_Risk_Red_Ratio_abs.png` (0.8 MB)
- `outputs/plots/maps/attribution_by_service/map_attr_C_Risk_Red_Ratio_pct.png` (0.8 MB)
- `outputs/plots/maps/attribution_by_service/map_attr_C_Risk_abs.png` (0.8 MB)
- `outputs/plots/maps/attribution_by_service/map_attr_C_Risk_pct.png` (0.8 MB)
- `outputs/plots/maps/attribution_by_service/map_attr_N_Ret_Ratio_abs.png` (1.8 MB)
- `outputs/plots/maps/attribution_by_service/map_attr_N_Ret_Ratio_pct.png` (1.8 MB)
- `outputs/plots/maps/attribution_by_service/map_attr_N_export_abs.png` (1.8 MB)
- `outputs/plots/maps/attribution_by_service/map_attr_N_export_pct.png` (1.8 MB)
- `outputs/plots/maps/attribution_by_service/map_attr_Nature_Access_abs.png` (1.8 MB)
- `outputs/plots/maps/attribution_by_service/map_attr_Nature_Access_pct.png` (1.9 MB)
- `outputs/plots/maps/attribution_by_service/map_attr_Pollination_abs.png` (1.8 MB)
- `outputs/plots/maps/attribution_by_service/map_attr_Pollination_pct.png` (2.0 MB)
- `outputs/plots/maps/attribution_by_service/map_attr_Sed_Ret_Ratio_abs.png` (1.6 MB)
- `outputs/plots/maps/attribution_by_service/map_attr_Sed_Ret_Ratio_pct.png` (1.6 MB)
- `outputs/plots/maps/attribution_by_service/map_attr_Sed_export_abs.png` (1.8 MB)
- `outputs/plots/maps/attribution_by_service/map_attr_Sed_export_pct.png` (1.8 MB)
- `outputs/plots/maps/global_access_overlap_heatmap_pct.png` (2.1 MB)
- `outputs/plots/maps/global_attribution_gap_map_min1_abs.png` (6.7 MB)
- `outputs/plots/maps/global_attribution_gap_map_min1_pct.png` (6.9 MB)
- `outputs/plots/maps/global_attribution_gap_map_min2_abs.png` (3.7 MB)
- `outputs/plots/maps/global_attribution_gap_map_min3_abs.png` (2.5 MB)
- `outputs/plots/maps/global_attribution_gap_map_min3_pct.png` (2.4 MB)
- `outputs/plots/maps/global_hotspot_count_heatmap_3plus_abs.png` (1.1 MB)
- `outputs/plots/maps/global_hotspot_count_heatmap_cap3_abs.png` (2.4 MB)
- `outputs/plots/maps/global_hotspot_count_heatmap_cap4_abs.png` (2.4 MB)
- `outputs/plots/maps/global_water_overlap_heatmap_pct.png` (2.0 MB)
- `outputs/plots/output_plots_diff/biome_map_data.csv` (0.0 MB)
- `outputs/plots/output_plots_diff/country_map_data.csv` (0.2 MB)
- `outputs/plots/output_plots_diff/income_grp_map_data.csv` (0.0 MB)
- `outputs/plots/output_plots_diff/region_wb_map_data.csv` (0.0 MB)
- `outputs/plots/output_plots_diff/region_wb_pct_only_cropped.png` (0.1 MB)
- `outputs/plots/pop_exposure_gdp.png` (0.2 MB)
- `outputs/plots/pop_exposure_gini.png` (0.3 MB)
- `outputs/tables/hotspot_5service_category_shares_abs.csv` (0.0 MB)
- `outputs/tables/multiplier_summary_WWF_biome.csv` (0.0 MB)
