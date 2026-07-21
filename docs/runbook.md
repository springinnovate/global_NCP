# Analysis Runbook

This repository contains several historical Rmd/Qmd notebooks under `analysis/`, but only a few drive the current hotspot workflow. Run the following in order when regenerating results from scratch.

---

## Prerequisite: LC grid crosswalk (run once, before anything else)

`10k_lcc_granular_metrics.gpkg`'s own `grid_fid` is a row-index into a *different* source grid
(`AOOGrid_10x10km_land_4326_clean.gpkg`, 1,691,819 cells — no longer on disk) than the master grid
used everywhere else in the pipeline (`landgrid_1_clean_enriched_4326.gpkg`, 1,522,073 cells, behind
`hotspots_global_pct.gpkg`'s `fid`). Joining on `grid_fid` equality between the two pairs cells
essentially at random — this was a real, previously-undetected bug (see the LCC map striping
investigation, 2026-07-08) that silently produced a wrong attribution-gap headline number for months.

Run **`scripts/build_lc_grid_fid_crosswalk.R`** once to build the nearest-centroid crosswalk
(`data/processed/lc_grid_fid_to_master_fid_crosswalk.csv`, ~99.4% of cells match at ~0m). Re-run only
if either source grid file changes. Both `analysis/hotspot_extraction.qmd`'s LC driver export chunk and
`scripts/compute_attribution_true_union.R` (below) join through this crosswalk instead of trusting
`grid_fid` directly — **but that check is a soft `file.exists()` fallback, not a hard failure**, so if
the crosswalk CSV is missing, those steps silently revert to the old broken behavior instead of erroring.
Always confirm the crosswalk file exists before trusting a from-scratch attribution run.

> **This crosswalk is a patch, not the right design.** It exists because the LC-change pipeline
> (`LC_change.qmd` → `10k_lcc_granular_metrics.gpkg`) was built against a different grid export
> (`AOOGrid_10x10km_land_4326_clean.gpkg`) than every other stage (`landgrid_1_clean_enriched_4326.gpkg`),
> and that divergence went undetected for months. If this pipeline is ever rebuilt from scratch — new
> time points, finer resolution, a regional focus, additional services — **the correct fix is to make
> every processing stage read its base grid from one single master grid file from day one**, not to
> reconcile mismatched grids after the fact with a spatial join. Reaching for another crosswalk script
> should be a last resort for legacy data, not the default pattern going forward.

## Full pipeline (re-run from raw data)

1.  **Data Processing** – `analysis/process_data.qmd`
    *   Consolidates the base zonal statistics outputs.
    *   Calculates bi-temporal change (absolute and symmetric percentage).
    *   Produces the canonical `processed/10k_change_calc.gpkg` used by all downstream steps.

2.  **Hotspot Extraction & Plots** – `analysis/hotspot_extraction.qmd`
    *   Reads `10k_change_calc.gpkg` and identifies hotspots based on the rules in `HOTS_CFG`.
    *   Exports hotspot vector layers (GeoPackages) to `data/processed/hotspots/` split by
        grouping variable (global, region_wb, income_grp, WWF_biome, nev_name) and metric
        (abs / pct). Writes the index to `data/processed/hotspots/_hotspots_index.csv`.
    *   Key params: `run_hotspot_export: true` must be set to regenerate GPKGs; set to `false`
        to skip and load from cache.

3.  **Spatial Clustering & Synthesis** – `analysis/hotspot_synthesis.qmd`
    *   Calculates hotspot intensity (coverage), relative intensity, and multi-service "hotness".
    *   Exports summary tables to `data/processed/tables/`:
        *   `hotspot_area_stats.csv` — relative intensity by service × grouping variable
        *   `hotspot_multiservice_stats.csv` — mean service overlap per group
        *   `hotspot_pop_exposure.csv` — population exposed by income / HDI / GDP / Gini bins
    *   The final chunk (`regional-subsets-export`) splits `hotspot_area_stats.csv` into
        per-group CSV files under `data/processed/tables/regional_subsets/`. This only needs
        re-running when `hotspot_area_stats.csv` changes (new services, updated InVEST inputs,
        or a different hotspot threshold).

4.  **Socioeconomic & Driver Analysis** – `analysis/KS_tests_hotspots.qmd`
    *   Consumes hotspot outputs to produce KS-test statistical summaries.

5.  **Attribution Gap (true union across all 5 drivers)** – `scripts/compute_attribution_true_union.R`
    *   Requires the LC grid crosswalk (see Prerequisite above) to already exist.
    *   Computes the true cell-level union overlap between ES hotspots and LCC driver hotspots
        (Forest_Loss, Crop_Exp, Urban_Exp, Grassland_Loss, Grassland_Gain) — this is the source of
        the book/paper's current headline attribution-gap numbers. No earlier script computed the
        union across all drivers; only per-driver marginals existed before this.
    *   Outputs to `data/processed/tables/`: `lcc_driver_magnitude_summary.csv`,
        `lcc_es_hotspot_per_driver_risk.csv`, `lcc_es_hotspot_true_union.csv`.
    *   Re-run whenever the ES hotspot set or the LCC driver definitions change.

> Historical notebooks such as `ch_analysis.Rmd`, `data_prep.Rmd`, etc., are kept for reference but are not part of the reproducible pipeline.

---

## Partial re-runs (most common case)

The GPKGs and summary tables are stable between pipeline runs. Individual steps can be re-run independently:

| What changed | What to re-run |
|---|---|
| InVEST model outputs / new service | Full pipeline (steps 1–4); step 5 too if the ES hotspot set changed |
| Hotspot threshold (`pct_cutoff`) | Steps 2–3 only, plus step 5 (attribution gap depends on the ES hotspot set) |
| `hotspot_area_stats.csv` only | Step 3 `regional-subsets-export` chunk only |
| LCC driver definitions or source LC grid | Prerequisite crosswalk step, then step 5 |
| Nothing — just need a regional report | Template only (see below) |

**To run only the `regional-subsets-export` chunk** without re-rendering the whole notebook,
open `analysis/hotspot_synthesis.qmd` in RStudio and run the chunk interactively
(Ctrl+Shift+Enter inside the chunk).

---

## Generating a regional / subgroup report

The parameterized template at `docs/templates/regional_report_template.qmd` produces a
self-contained HTML for any single grouping-variable value. It reads from the pre-computed
`regional_subsets/` CSVs (fallback: full `hotspot_area_stats.csv` if subsets don't exist yet).

```bash
# By World Bank region
quarto render docs/templates/regional_report_template.qmd \
  -P grouping_var:region_wb \
  -P group_val:"Sub-Saharan Africa" \
  -P report_title:"Sub-Saharan Africa"

# By income group (also shows population exposure)
quarto render docs/templates/regional_report_template.qmd \
  -P grouping_var:income_grp \
  -P group_val:"5. Low income" \
  -P report_title:"Low Income Countries"

# By biome
quarto render docs/templates/regional_report_template.qmd \
  -P grouping_var:WWF_biome \
  -P group_val:"Tropical & Subtropical Moist Broadleaf Forests" \
  -P report_title:"Tropical Moist Forests"

# By country
quarto render docs/templates/regional_report_template.qmd \
  -P grouping_var:nev_name \
  -P group_val:Brazil \
  -P report_title:Brazil
```

**Note:** Population exposure is only shown when `grouping_var` is `income_grp`, because
`hotspot_pop_exposure.csv` is cross-tabulated by income group only. Driver attribution is
global (not subregional) and is shown in every report.

> **Deferred — requires full synthesis re-run (better machine):**
> Exposure by `region_wb`, `WWF_biome`, and `nev_name` can be added by extending the
> `calc-pop-exposure` chunk in `hotspot_synthesis.qmd` with additional `group_by()` calls
> (the columns are already on `grid_df`). That chunk must be set back to `eval: true` and
> the full notebook re-rendered. Once done, update `generate_regional_subsets.R` to also
> copy the new `hotspot_pop_exposure_{gv}.csv` files into `regional_subsets/{gv}/`, and
> update the template to load the right file based on `grouping_var`.

### Cross-cut filtering (e.g., Sub-Saharan Africa + Low income)

Pre-computed CSVs cover one dimension at a time. For multi-criteria cross-cuts, use
`filter_multidim()` on the raw grid data in R:

```r
devtools::load_all()

# Load raw long-format grid (produced by hotspot_extraction.qmd)
plt_long <- readRDS(file.path(data_dir(), "processed", "plt_long.rds"))

# Filter to Sub-Saharan Africa + Low income cells
subset_df <- filter_multidim(
  plt_long,
  region_wb  = "Sub-Saharan Africa",
  income_grp = "5. Low income"
)

# Extract hotspots for this cross-cut
hs <- extract_hotspots(subset_df, ...)
```