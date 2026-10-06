# How to ask a new question with this pipeline

*Draft v1, 2026-10-01. For anyone extending the analysis after the paper. Read `docs/runbook.md`
for how to run each step; this file says which step to touch for which kind of question.*

The pipeline answers one kind of question: how ecosystem service provision changed between two
dates, where the change is most severe, who is exposed, and how it relates to land cover
conversion. Every part of that is parameterized: which rasters, which groupings, which change
metric, which threshold, which spatial scale. Most new questions are a change to one of those, not
new code.

## Two paths, and which one to use

| | Path A | Path B |
|---|---|---|
| Unit | 300 m pixels, aggregated to any vector grouping | 10 km equal-area grid cells (1.52 M) |
| Tool | `zonal_stats_toolkit` (sibling repo), `runner.py` + an `.ini` | `Python_scripts/summary_pipeline_landgrid.py` (Docker) + `analysis/*.qmd` |
| Use for | Totals and per-area change by region, biome, income group, country, or any polygon layer | Hotspots, multi-service overlap, socioeconomic profiling, attribution, null models |
| Paper | Global and regional trajectories (Results 4.1, Figure 2, Annex bar charts) | Everything from hotspots onward |

Path A is the right tool when the question is "how much did X change in group G". Path B is the
right tool when the question is "where are the extreme changes and what co-occurs with them".

## 1. Slice by a new grouping

**Path A.** Add a job to the `.ini` (copy an existing `[job:...]` block): set `agg_vector` to the
polygon layer and `agg_field` to the attribute to group by. The same rasters are summarized per
group (mean, stdev, counts). Example configs: `zonal_stats_toolkit/global_ncp_diff_consolidated.ini`
(change rasters) and `global_ncp_consolidated.ini` (base years). The plotting step,
`zonal_stats_toolkit/compare_and_plot_changes.R`, derives symmetric percent change (SPC) of group
totals from the base-year run and absolute change per area from the change run.

**Path B, one grouping at a time.** Region, income group, biome and country are already columns
on the grid; `docs/templates/regional_report_template.qmd` renders a report for any single value
(commands in `docs/runbook.md`, "Generating a regional / subgroup report").

**Path B, cross-cuts** (for example Sub-Saharan Africa and low income): `filter_multidim()` on
`plt_long.rds`, then `extract_hotspots()` on the subset (example in the runbook).

**Path B, a grouping not yet on the grid:** join the polygon attribute to the master grid first,
by geometry, keyed on the master `grid_fid` (see the pitfalls below).

## 2. Add a service or a new raster version

1. **Declare it once** in `R/service_config.R`: name, raw column prefix, and `good_direction`
   (`"high"` if an increase is favorable). Every R consumer reads this file. Do not type service
   names into scripts; scripts that need a display order assert it matches this file.
2. **Path B:** add the raster to the relevant `analysis_configs/*.yaml`, confirm the file exists,
   rerun the Docker zonal extraction (runbook Step 0), then `analysis/process_data.qmd` and the
   downstream notebooks (runbook "Partial re-runs" table says which).
3. **Path A:** compute the 2020 minus 1992 change, convert volumetric quantities to per hectare
   before zonal stats (`zonal_stats_toolkit/convert_to_ha.py`; not ratios, indices, coastal risk
   or nature access), then run the toolkit. `compare_and_plot_changes.R` keeps its own
   raster-name to service mapping (`map_base_cols`, `map_diff_cols`); add the new service there
   too.
4. **A new version of an existing raster** (one service only): copy
   `Python_scripts/rerun_n_export_path_a.py` with its `.ini`
   (`analysis_configs/path_a_n_export_rerun.ini`). For a ratio of two rasters, copy
   `Python_scripts/rerun_n_ret_ratio_path_a.py`, which aligns the two grids first.

## 3. Change the change metric or the hotspot threshold

Hotspots are the top or bottom 5% of cells by SPC per service, in the direction that means
decline. The threshold and directions are set in `HOTS_CFG` in `analysis/hotspot_extraction.qmd`
(directions come from `R/service_config.R`). Both SPC and absolute change are computed for every
cell (`pct_chg`, `abs_chg`), and hotspots are exported for both (`data/processed/hotspots/pct/`,
`.../abs/`). After a threshold change, rerun runbook steps 2, 3 and 5.

The choice matters: ranking by absolute change instead of SPC keeps only about half of the
hotspot cells (paper Methods, "Hotspot Identification", and the Annex table on hotspot
sensitivity). Report which metric a result uses.

## 4. Test whether two patterns co-occur beyond chance

`scripts/compute_attribution_true_union.R` computes the overlap between ES hotspots and land
cover conversion hotspots, per driver and for their union, with risk and odds ratios.
`analysis/attribution_null_model.R` compares that overlap with a stratified null model: ES hotspots
placed at random within latitude-longitude blocks (whole grid, 10°, 5°, 2°, 1°, 0.5°), keeping each
block's counts. The observed / expected ratio shows how much co-occurrence survives once shared
broad geography is held fixed.

To test a different pair of layers (a new driver, a socioeconomic layer, protected areas), copy
the null model script and replace the two cell sets; the block logic does not change. Read the
ratios, not significance: cells are spatially autocorrelated, so per-cell tests are overconfident.

## 5. Change the spatial scale

Path B's unit is the 10 km grid; aggregate further by grouping (section 1) or by block (section 4).
Finer than 10 km means Path A or a new grid. If you build a new grid, make every stage read the
same master grid file from the start; the crosswalk described below is a repair for legacy data,
not a pattern to repeat.

## Pitfalls that have caused real errors here

- **Two grid ID schemes.** `10k_lcc_granular_metrics.gpkg`'s `grid_fid` indexes a different grid
  (1.69 M cells) from the master grid (1.52 M). Always go through
  `data/processed/lc_grid_fid_to_master_fid_crosswalk.csv` and keep one match per master cell
  (nearest, within 1 m of the best). Joining on raw `grid_fid` pairs cells at random. Runbook,
  "Prerequisite".
- **Grids that look the same but are offset.** Rasters from different model runs can differ in
  origin by a fraction of a degree. Before combining two rasters pixel by pixel, compare their
  transforms; never pair them by array index.
- **Service lists typed into scripts.** They drifted silently in five scripts in August 2026. Use
  `R/service_config.R`.
- **CSV columns with commas.** Three biome names contain commas; split-on-comma tools (awk, cut)
  silently drop them. Use a real CSV reader.
- **Clean exit is not correct output.** Several past bugs rendered without error. Check output
  content (row counts, totals, a known value) after every rerun, and back up a table before
  overwriting it.
