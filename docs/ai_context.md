# Global NCP – Hotspots Time-Series Workflow (Long Context)

_Last updated: 2026-01_

This document provides full project context for AI assistants and collaborators.
Use `doc/ai_context.min.md` for invariants and rules; this file explains **why** the workflow is structured as it is.

---

## 1. Project Overview

**Objective.**
Quantify global change in multiple ecosystem services (~10 km grid, ~1992–2020), identify **hotspots** of concerning change, and describe how magnitudes and distributions vary across subregions.

The emphasis is on:
- clarity
- reproducibility
- descriptive patterns **before** complex modeling

**Key questions**
- Where are global hotspots of ecosystem-service change?
- How do hotspot magnitudes differ by subregion?
- Do subregions systematically deviate from global patterns (KS tests)?
- Which services co-occur in hotspots?

---

## 2. Analysis Entry Points (authoritative)

These QMDs orchestrate the workflow:

- `analysis/process_data.qmd`
  - Consolidates services/beneficiaries, calculates change, and builds canonical dataset
  - Handles joins, service recoding, and checkpointing

- `analysis/hotspot_extraction.qmd`
  - Defines and validates `HOTS_CFG`
  - Calls hotspot extraction and writes GPKGs + index

- `analysis/KS_tests_hotspots.qmd`
  - Compares hotspot distributions (subregion vs global)
  - Produces tidy KS tables and visual summaries

- `analysis/LC_change_preparation.qmd` & `analysis/LC_change_granular.qmd`
  - **Prep**: Extracts/reclassifies raw LC rasters to 9-class.
  - **Granular**: Runs `diffeR` models (Forest Loss, Expansion) to produce driver metrics.
  - **Integration**: Metrics joined in `hotspot_extraction.qmd`.

Logic should be implemented in `R/` and **called** from these QMDs.

---

## 3. Data Model

- Base grid: ~10 km cells with stable `fid`
- Services stored as `{service}_abs_chg` and `{service}_pct_chg`
- Subregional attributes joined via point-on-surface:
  - WB regions, income groups, continents, UN regions
  - WWF biomes
  - Inputs: `vectors/cartographic_ee_ee_r264_correspondence.gpkg`, `vectors/Biome.gpkg`
- Output written to:
  `data/processed/10k_change_calc.gpkg`
- **Aggregation Logic**:
  - **Extensive** variables (e.g. Nitrogen kg) are **summed**.
  - **Intensive** variables (e.g. Risk Index) are **averaged**.
  - On an equal-area grid, these are proportional and comparable.

Percent change uses symmetric mode (bounded [-200, 200]); `Inf` (appearing from 0) is clamped to 200%. `NA` values are filtered during pivot.

---

## 4. Long Table (`plt_long`)

Built once, reused everywhere.

Steps:
1. Read consolidated GPKG
2. Pivot change columns to long
3. Recode services to canonical labels
4. Filter invalid values
5. Factor `service` using canonical order
6. Drop heavy geometry after deriving `grid_sf`

Checkpointing (`analysis/_checkpoints/`) is strongly encouraged.

---

## 5. Hotspot Logic

Hotspots are defined **per service**, using vectorized loss/gain rules:

- Loss services → lowest tail
- Gain services → highest tail
- Default threshold: top/bottom 5%

Hotspots are:
- extracted once
- written as compact GPKGs
- indexed in `_hotspots_index.csv`

Downstream analyses **must reuse these artifacts**.

---

## 6. Plotting Philosophy

**Barplots**
- All cells (not hotspot-only)
- Show magnitude of change
- Direction-agnostic
- Trim extreme tails
- Optional global reference bar

**Violins**
- Hotspot-only
- Compare distributions across subregions
- Free y-scales per service

White background PNGs only (no transparent plots).

---

## 7. KS Tests (current focus)

- Compare ECDFs of hotspot magnitudes:
  - subregion vs global
  - optionally between subregions
- Adjust p-values for multiple testing
- Output tidy tables and compact visuals

Interpretation must clearly distinguish:
- magnitude summaries (bars)
- distributional differences (violins, KS)

---

## 8. Land Cover Change (LCC)

- **Goal**: Attribute ES hotspots to drivers (conversion vs. degradation).
- **Methodology**: `diffeR` (Pontius et al.) for Gross Loss/Gain/Exchange.
- **Models**:
  - **Forest Loss**: Binary (Forest vs. Non-Forest).
  - **Expansion**: Multi-class (Urban, Cropland, Other).
- **Artifacts**:
  - Input: `processed/10k_lcc_granular_metrics.gpkg`
  - Joined in: `hotspot_extraction.qmd`

---

## 9. Coding & Design Principles

- Paths resolved via `data_dir()` and `here::here()`
- Rules centralized in config objects
- Functions are pure where possible
- Side effects (IO) are explicit and idempotent
- Performance matters: reuse, cache, slim geometries

---

## 10. Known Limitations & Next Steps

- Externalize service metadata to `analysis_configs/service_meta.csv`
- Finalize KS test runner + visualization
- Add lightweight tests for hotspot logic
- Polish plot annotations for paper-ready figures

---

## 11. How the Assistant Should Help

- Preserve scientific intent
- Improve structure, readability, and reuse
- Avoid duplicating logic across QMDs
- Prefer explicit context files over implicit memory

## 12. Project Status & History

- See `analysis/WORKLOG.md` for the active task list, daily progress, and detailed session history.

## 13. Developer Reference (Operational)

### VS Code + Remote SSH Settings

Recommended settings for maintaining persistent R sessions on the remote server:

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

### Quick Commands

**Set stable repo root & sanity-check paths:**

```bash
cd ~/projects/global_NCP
[ -f .here ] || touch .here
Rscript -e 'if (!requireNamespace("here", quietly=TRUE)) install.packages("here"); cat("here() ->", here::here(), "\n"); source(here::here("R","paths.R")); cat("data_dir() ->", data_dir(), "\n")'
```
