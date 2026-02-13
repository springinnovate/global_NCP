# Global NCP – Hotspots Time‑Series Workflow (Long Context)

*Last updated: 2026-02-04*

This document captures the full project context for assistants (e.g., Codex in VS Code) and collaborators. Use it alongside the slimmer spec in `doc/codex_context.min.md`. When coding or refactoring, prefer the **minimal** file as your primary context and consult this long version for rationale and narrative background.

---

## 1) Project Overview

**Goal.** Quantify global change in multiple ecosystem services at ~10‑km resolution over ~1995–2020, identify **hotspots** of concerning change, and describe patterns across **subregions** (World Bank regions, income groups, continents, UN regions, WWF biomes). Produce clear, reproducible figures (bars/violins/maps) and simple statistics (e.g., KS tests) to communicate patterns **before** deep modeling.

**Core questions.**

* Where are the global hotspots of ecosystem‑service change?
* How do hotspot magnitudes and distributions differ by subregion?
* Do subregions systematically deviate from global patterns (KS tests)?
* Which services tend to co‑occur in hotspots (simple combos)?

**Key outputs.**

1. Processed 10‑km grid with subregional tags (`processed/10k_change_calc.gpkg`).
2. Hotspot feature layers (global and per‑group) as compact GPKGs under `processed/hotspots/` plus an index CSV.
3. Reusable tidy table `plt_long` (cell × service with `abs_chg`/`pct_chg`).
4. **Intensity & Enrichment**: Metrics quantifying hotspot coverage and disproportionate concentration per region (`hotspot_area_stats.csv`).
4. Figures: trimmed **barplots** of magnitudes (all cells), **violins** of hotspot distributions, and (later) **KS** tables/plots.

---

## 2) Data Sources & Assumptions

**Data root.** All raw/processed data live under `data_dir()` (backed by `GLOBAL_NCP_DATA`). On the main server (lilling): `/home/jeronimo/data/global_ncp`.

**Grid & attributes.**

* Base grid: ~10‑km cells with a stable integer key `fid` and country key `c_fid`.
* Services appear as columns with suffixes `*_abs_chg` / `*_pct_chg` (absolute/percent change between two time slices).
* Subregional attributes are joined via point‑on‑surface from two vector layers:

  * `vectors/cartographic_ee_ee_r264_correspondence.gpkg` → `iso3`, `region_wb`, `income_grp`, `continent`, `region_un`, `subregion`, etc.
  * `vectors/Biome.gpkg` → `BIOME` (int), `WWF_biome` (chr).
* Joined output is written as `processed/10k_change_calc.gpkg`.

**CRS & geometry.**

* `sf_use_s2(TRUE)` globally. All inputs are transformed to the grid CRS before joins.
* Representative **point‑on‑surface** is used for attribute joins to avoid polygon‑polygon slivers.

**Service labels.** Raw columns are normalized to a canonical set used in figures and rules:

```
C_Risk, N_export, Sed_export,
C_Risk_Red_Ratio, N_Ret_Ratio, Sed_Ret_Ratio,
Pollination, Nature_Access
```

(Additional raw names map via a lookup; see §4.2.)

**Directionality.**

* **Loss services** (bad when they go **down**): `Nature_Access`, `Pollination`, `N_Ret_Ratio`, `Sed_Ret_Ratio`, `C_Risk_Red_Ratio` → keep **lowest** tail.
* **Gain services** (bad when they go **up**): `Sed_export`, `N_export`, `C_Risk` → keep **highest** tail.

**Assumptions.**

* **Methodological Dual-Path**:
  * **Path A (Pixel-Level)**: Direct difference of rasters. Used for raw change summaries.
  * **Path B (Grid-Level)**: Aggregation to 10km grid first, then difference. Used for **hotspot identification** and regional synthesis.
* **Aggregation Logic**: Extensive variables (e.g. Nitrogen kg) are **summed**; Intensive variables (e.g. Risk Index) are **averaged**. On an equal-area grid, these are proportional and comparable for relative change.

* `fid` is unique and stable across all outputs.
* `c_fid` references the country polygon key used elsewhere.
* Percent change columns may contain `NA/Inf`; these are filtered at pivot time.

---

## 3) Repository Structure (conventional)

```
project root
├── analysis/
│   ├── prepare_data.qmd              # preprocessing & consolidation
│   ├── process_data.qmd              # change calculation & metric derivation
│   ├── hotspot_extraction.qmd        # main narrative & orchestration
│   ├── hotspot_intensity.qmd         # area intensity & enrichment analysis
│   ├── hotspot_multiservice.qmd      # overlap & "hotness" analysis
│   ├── KS_tests_hotspots.qmd         # statistical comparisons (KS tests)
│   └── restore_checkpoint.R          # restores big tidy tables (plt_long, grid_sf)
├── R/
│   ├── paths.R                       # data_dir(), ncp_data_root(), etc.
│   ├── extract_hotspots.R            # extract_hotspots(); returns list of outputs
│   ├── hotspot_violins.R             # run_hotspot_boxplots_by()
│   ├── utils_hotspot.R               # shared hotspot utils
│   └── ...                           # other helpers
├── analysis_configs/
│   └── service_meta.csv              # (planned) service labels & directions
├── data/                             # (mounted by data_dir())
│   ├── vectors/                      # country/biome source vectors
│   ├── processed/
│   │   ├── 10k_change_calc.gpkg      # grid + subregional tags
│   │   └── hotspots/                 # all hotspot GPKGs + index CSV
│   └── raw/                          # (if needed)
├── outputs/
│   └── plots/
│       ├── abs/<group_col>/bars_*.png
│       ├── pct/<group_col>/bars_*.png
│       └── violins/<group_col>/*_violins.png
└── doc/
    ├── codex_context.md              # this file
    └── codex_context.min.md          # slim spec for assistants
```

> The exact file names under `R/` can evolve; the key point is to keep **hotspot logic** and **plot logic** modular, roxygen‑documented, and imported with `devtools::load_all()`.

---

## 4) Reproducible Runbook

### 4.1 Quarto metadata & run metadata

* Quarto front matter uses `params.analysis_version` (e.g., `v0.4.1`).
* A lightweight `run-metadata` chunk prints: analysis version, rendered time, git branch/commit, and `data_root`. This chunk is safe to run anytime.

### 4.2 Pivot to tidy long (`plt_long`)

* Read `processed/10k_change_calc.gpkg` (ensuring `fid`).
* Split ID/group columns vs change columns (`*_abs_chg`, `*_pct_chg`).
* `pivot_longer()` → `pivot_wider()` to get `abs_chg` and `pct_chg` by `service`.
* Filter `Inf`/`NA` and cells lacking `c_fid`.
* **Service label map**: recode raw names to canonical labels. (Plan to externalize to `analysis_configs/service_meta.csv` with columns `raw,label,direction,pref_metric` and join once.)
* Set `service` factor order to the canonical eight, then append extras in sorted order.
* Create a slim geometry `grid_sf <- sf_f[, c("fid","c_fid")]` and `rm(sf_f)` to save memory.

**Checkpointing (recommended).**

* Save heavy tables for fast resumes: `saveRDS(plt_long, "analysis/_checkpoints/plt_long.rds")`; same for `grid_sf` if desired.
* Restore with `analysis/restore_checkpoint.R` (already prepared to print sizes and confirm columns).

### 4.3 Hotspot configuration (single source of truth)

Define once in the QMD (or a small R file read by the QMD):

```
HOTS_CFG <- list(
  analysis_name   = "global_NCP_hotspots",
  pct_cutoff      = 0.05,
  threshold_mode  = "percent",   # or "count" (with n_cut)
  rule_mode       = "vectors",    # uses loss/gain below
  loss            = c("Nature_Access","Pollination","N_Ret_Ratio","Sed_Ret_Ratio","C_Risk_Red_Ratio"),
  gain            = c("Sed_export","N_export","C_Risk"),
  combos          = list(
    deg_combo = c("Nature_Access","Pollination","N_export","Sed_export","C_Risk"),
    rec_combo = c("Nature_Access","Pollination","N_Ret_Ratio","Sed_Ret_Ratio","C_Risk_Red_Ratio")
  ),
  groupings       = c("income_grp","region_wb","continent","region_un","WWF_biome"),
  write_layers    = TRUE,
  write_index     = TRUE,
  out_dir         = file.path(data_dir(), "processed", "hotspots")
)
```

**Validate**: ensure no service appears in both loss and gain; warn on missing services vs `plt_long$service`.

### 4.4 Hotspot extraction

`extract_hotspots(df, value_col, pct_cutoff, threshold_mode, rule_mode, loss_services, gain_services, combos, id_cols, sf_obj, ...)`:

* Ranks within each `service` and flags **top/bottom tails** based on rule mode.
* Returns:

  * `hotspots_df` (long rows flagged as hotspots),
  * `non_hotspots_df`,
  * `summary_df` (one row per `fid` with counts/types/combos),
  * `binary_matrix` (wide 0/1 columns per service for hotspots only),
  * `hotspots_sf` (if `sf_obj` provided: hotspot features with summary + binary columns).
* Writing: we **write only hotspot features** (compact) with safe file names and an **index CSV**:

  * Columns: `scope, group_col, group_val, metric, n_hot, gpkg`.
  * Paths follow: `processed/hotspots/{abs|pct}/{global|<group_col>}/hotspots_*_{abs|pct}.gpkg`.

A thin runner `run_one_hotset()` applies the config and writes artifacts for global and for each grouping value; it enforces `fid` integrity against the geometry and returns one row for the index.

### 4.5 Barplots (all cells, not hotspot‑only)

* Purpose: show **magnitudes of total change** per service and subregion—clear and non‑statistical.
* Pipeline: start from `plt_long`, compute **trimmed summaries** per subregion and service (e.g., drop zeros/NA, trim upper 0.1% with `cut_q`), keep canonical `svc_order`, optionally include a **Global** bar in each facet.
* Scales: `scales = "free_y"` per service facet.
* Visual cue: if `include_global = TRUE`, draw a distinct **Global** bar (different fill or a black outline) and place it **last** within each facet.
* Output: `outputs/plots/{abs|pct}/<group_col>/bars_<group_col>_{abs|pct}.png`.

### 4.6 Violins (hotspots only)

* Purpose: compare the **distribution** of hotspot magnitudes across subregions.
* Inputs: hotspot rows only (from the wrapper or by filtering `hotspots_df`), keeping only canonical services.
* Trimming: remove `NA/0`, then per‑service trim to [0.1%, 99.9%] (configurable via `cut_q`).
* Sampling: optionally sample up to `plot_n` rows for speed.
* Scales: `facet_wrap(~service, ncol = 3, scales = "free_y")`.
* Output: `outputs/plots/violins/<group_col>/*_{abs|pct}_change_violins.png`.
* Make background **white** (e.g., device `ragg_png()` and `theme_minimal()`; avoid transparent PNGs in dark UIs).

### 4.7 KS tests (Implemented)

* Compare the **ECDFs** of hotspot magnitudes between a subregion and the **global** distribution, or between pairs of subregions.
* Provide both the **statistic** and **adjusted p‑values** (e.g., BH FDR) across services and metrics.
* **Visuals**: Heatmaps of KS statistics, ECDF overlays, and **Cliff's Delta** (directionality) bar plots.
* **Transformations**: Signed power transformations used to visualize small but significant effects.
* Clarify in text that bars (magnitude plots) are **direction‑agnostic** (absolute size of change), while violins + KS can be run on **absolute** or **percent** changes depending on the question.

### 4.8 Hotspot Intensity & Multi-service

* **Intensity**: % of land area classified as hotspot per region.
* **Enrichment**: Ratio of (Observed Share of Hotspots / Expected Share based on Area). Values > 1 indicate disproportionate concentration.
* **Hotness**: Average number of overlapping service hotspots per pixel.

### 4.9 Current run status / next steps

- **Completed**:
  - Core hotspot extraction (v1.0.2).
  - KS Analysis pipeline (robust to suffixes, improved visuals).
  - Intensity/Enrichment metrics.
  - Documentation of Sum vs Mean aggregation logic.
- **Next**:
  - Land Cover change analysis (new branch).
  - Final report drafting.

---

## 5) Coding Conventions

* **Paths**: always use `data_dir()` (from `R/paths.R`) and `here::here()` for repo‑relative paths. Put a `.here` sentinel in repo root so `here::here()` is stable.
* **Roxygen**: Functions in `R/` are roxygenized; exports are explicit. We keep a **compat alias** (e.g., `agg_change` → `aggregate_change_simple`) until old references are gone.
* **Naming**: snake_case for objects and file names; service labels use the canonical set (§2); group columns: `income_grp`, `region_wb`, `continent`, `region_un`, `WWF_biome`.
* **IO discipline**: writes are **idempotent**; use `delete_dsn = TRUE` for GPKG. Filenames are slugified; avoid spaces.
* **Performance**: prefer `grid_sf` (slim geometry) over full attribute SF; remove big frames after deriving `plt_long` (`rm(...); gc()`). Use `Sys.setenv(GDAL_NUM_THREADS="ALL_CPUS")`.
* **Plot look**: consistent theme across plots; white background PNGs; minimal clutter. Free y‑scales per service facet unless otherwise noted.
* **Re‑runs**: compute hotspots once, cache artifacts, and read from GPKGs/CSV when plotting or testing.

---

## 6) Remote Dev & Session Management

**VS Code + Remote SSH.**

* Preferred workflow: connect to `lilling`, open repo, and run R in an interactive terminal.
* Keep long jobs alive with **GNU screen** (or tmux). Example:

  ```bash
  screen -S rwork        # create session
  screen -r rwork        # reattach
  # inside screen, start R and run your QMD chunks
  ```
* VS Code settings commonly used (remote):

  ```json
  {
    "r.rterm.linux": "/usr/bin/R",
    "r.rpath.linux": "/usr/bin/R",
    "r.rterm.option": ["--no-save","--no-restore","--quiet"],
    "terminal.integrated.defaultProfile.linux": "bash",
    "terminal.integrated.profiles.linux": {
      "bash":   { "path": "/bin/bash" },
      "screen": { "path": "/usr/bin/screen", "args": ["-R","-S","rwork"] }
    }
  }
  ```
* Ensure repo root contains a `.here` file so `here::here()` resolves consistently.
* Quarto CLI may not be installed on server; run chunks interactively or install Quarto if rendering server‑side is needed later.

**Tip:** If you disconnect, the screen session (and R inside it) keeps running. Reattach with `screen -rd rwork`.

---

## 7) How Assistants (Codex) Should Behave

* **Respect scientific logic.** You may refactor, modularize, and optimize, but do **not** change how hotspots are defined or which services are loss/gain without explicit instruction.
* **Prefer the minimal context.** Read `doc/codex_context.min.md` first for invariants and paths; consult this long doc for details.
* **Centralize rules.** Any change to thresholds, service directionality, or groupings must go through `HOTS_CFG` and be validated.
* **Reduce recomputation.** Reuse written GPKGs and the `_hotspots_index.csv` rather than recalculating.
* **Improve ergonomics.** It’s acceptable to add helpers (e.g., a single `make_all_bars()` that loops over groupings and metrics) and to externalize service metadata to `analysis_configs/service_meta.csv`.
* **Document changes.** Update roxygen headers and this doc when altering interfaces.

---

## 8) Limitations & TODOs

* **Service metadata externalization.** Replace hard‑coded recode vectors with `analysis_configs/service_meta.csv` (columns: `raw,label,direction,pref_metric`).
* **Aliasing cleanup.** Keep `agg_change` alias until all code uses `aggregate_change_simple` (or vice versa), then deprecate.
* **KS module.** Implement KS calculations + multiple‑testing correction and a compact visualization.
* **Caching.** Persist `plt_long` (e.g., `fst`/`arrow`) to speed resumes and reduce memory pressure.
* **Tests.** Add small snapshot tests for the hotspot runner and plot builders.
* **Quarto render.** Optionally add CLI render once Quarto is installed server‑side.
* **Plot polish.** Finalize color palette, legend placement, and annotation text (especially explaining **direction‑agnostic magnitude** in barplots).

---

## 9) Quick Commands (Cheat‑Sheet)

**Set stable repo root & sanity‑check paths**

```bash
cd ~/projects/global_NCP
[ -f .here ] || touch .here
Rscript - <<'RS'
if (!requireNamespace("here", quietly=TRUE)) install.packages("here", repos="https://cloud.r-project.org")
cat("here() ->", here::here(), "\n")
cat("paths.R exists ->", file.exists(here::here("R","paths.R")), "\n")
source(here::here("R","paths.R"), encoding="UTF-8")
cat("data_dir() ->", data_dir(), "\n")
RS
```

**Create checkpoints**

```r
saveRDS(plt_long, here::here("analysis","_checkpoints","plt_long.rds"))
sf::st_write(grid_sf, here::here("analysis","_checkpoints","grid_sf.gpkg"), delete_dsn = TRUE)
```

**Restore checkpoints**

```r
source(here::here("analysis","restore_checkpoint.R"), encoding = "UTF-8")
```

**List produced hotspot files**

```r
list.files(here::here("data","processed","hotspots"), recursive = TRUE, pattern = "\.gpkg$", full.names = TRUE)
readr::read_csv(here::here("data","processed","hotspots","_hotspots_index.csv"))
```

**Barplots loop (abs & pct for all groupings)**

```r
groupings <- HOTS_CFG$groupings
for (gc in groupings) {
  for (metric in c("abs","pct")) {
    build_barplots_by(
      df_long = plt_long,
      group_col = gc,
      metric = metric,
      out_dir = here::here("outputs","plots", metric, gc),
      out_stub = paste0("bars_", tolower(gc), "_", metric),
      include_global = TRUE,
      keep_only_ordered = TRUE,
      cut_q = 0.999,
      save_plot = TRUE
    )
  }
}
```

---

## 10) Versioning & Branching

* **Analysis version** appears in Quarto via `params.analysis_version` (e.g., `v0.4.1`).
* **SemVer for analysis**: bump **PATCH** for plot tweaks/docs; **MINOR** for schema/config changes (e.g., new grouping columns); **MAJOR** for breaking path/structure changes.
* **Branches**: `feat(qmd): ...`, `fix(hotspots): ...`, `refactor(plots): ...`. Keep small, descriptive commits. Example: `feat(qmd): config‑driven hotspot export + tidy pivot + run metadata`.

---

## 11) Outputs Catalog (reference)

* **Index CSV**: `data/processed/hotspots/_hotspots_index.csv`

  * Columns: `scope (global|by_group)`, `group_col`, `group_val` (chr), `metric (abs|pct)`, `n_hot`, `gpkg` (path or NA).
* **Global layers**:

  * `data/processed/hotspots/abs/global/hotspots_global_abs.gpkg`
  * `data/processed/hotspots/pct/global/hotspots_global_pct.gpkg`
* **Grouped layers** (pattern):

  * `data/processed/hotspots/{abs|pct}/<group_col>/hotspots_<group_col>_<slug(group_val)>_{abs|pct}.gpkg`
* **Figures**:

  * Bars: `outputs/plots/{abs|pct}/<group_col>/bars_<group_col>_{abs|pct}.png`
  * Violins: `outputs/plots/violins/<group_col>/*_{abs|pct}_change_violins.png`

---

### Final note

For day‑to‑day coding, assistants should read **`doc/codex_context.min.md`** first. Use this long document to answer *why* something is done a certain way and to inform documentation, papers, and onboarding.
