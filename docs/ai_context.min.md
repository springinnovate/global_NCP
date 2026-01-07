# AI Context (Minimal, Invariants)

## Project Goal
Global NCP time-series analysis (~1995–2020): quantify ecosystem-service change at ~10 km resolution, extract **hotspots** of concerning change, and summarize patterns globally and by subregions (WB region, income group, continent, UN region, WWF biome).

## Primary Entry Points (do not duplicate logic)
- `analysis/Consolidation.qmd`
- `analysis/hotspot_extraction.qmd`
- `analysis/KS_tests_hotspots.qmd`

All major logic should be **called from these QMDs**, not redefined inline.

## Data & Geometry Invariants
- Unique grid key: `fid`
- Country key: `c_fid`
- Long table schema (required):
  `fid, c_fid, service, abs_chg, pct_chg, <grouping vars>`
- Slim geometry object: `grid_sf <- sf[, c("fid","c_fid")]`

## Canonical Services (order matters)
- C_Risk, N_export, Sed_export,
- C_Risk_Red_Ratio, N_Ret_Ratio, Sed_Ret_Ratio,
- Pollination, Nature_Access

Always factor `service` using this order (extras appended alphabetically).

## Hotspot Rules (HOTS_CFG is the single source of truth)
- `rule_mode = "vectors"`
- `threshold_mode = "percent"`
- `pct_cutoff = 0.05`
- Loss services (bad when ↓):
  `Nature_Access, Pollination, N_Ret_Ratio, Sed_Ret_Ratio, C_Risk_Red_Ratio`
- Gain services (bad when ↑):
  `Sed_export, N_export, C_Risk`
- Groupings:
  `income_grp, region_wb, continent, region_un, WWF_biome`

Never change rules without explicit instruction.

## Outputs (fixed layout)
- Hotspots:
  `data/processed/hotspots/{abs|pct}/{global|<group_col>}/hotspots_*.gpkg`
- Index:
  `data/processed/hotspots/_hotspots_index.csv`
- Plots:
  `outputs/plots/{abs|pct}/<group_col>/*.png` (white background)

## Plotting Conventions
- Barplots: magnitude of change (direction-agnostic), trimmed tails
- Violins: hotspot-only distributions, free y-scale per service
- Save PNGs with white background (`bg = "white"`, `dpi = 300`)

## Performance & Reproducibility
- Always use `data_dir()` from `R/paths.R`
- Cache heavy objects (`plt_long.rds`, hotspot GPKGs)
- Do not recompute hotspots if written artifacts exist

## Assistant Behavior
- Refactor for clarity and modularity, **never** change scientific meaning
- Centralize constants and rules
- Prefer reuse of written artifacts over recomputation
