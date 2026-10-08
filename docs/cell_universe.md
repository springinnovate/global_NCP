# Cell universe: which cells every number is computed on

Settled 2026-10-07. Checked by `analysis/verify_cell_universe.R` (stops on any mismatch). Use these
counts and denominators everywhere (paper, book, deck, figures); do not re-derive them by hand.

## Counts

| Set | Cells | Definition |
|---|---:|---|
| Grid | 1,522,073 | All cells of the IUCN AOO 10 km equal-area grid in `10k_change_calc.gpkg` |
| **Evaluated cells** | **1,372,621** | Grid minus Antarctica and open ocean (`continent`) and minus Lakes and Rock & Ice (`WWF_biome`). Same exclusions as `hotspot_extraction.qmd`. Cells with no region, income group or biome stay in. |
| N export, sediment export, pollination | 1,372,621 | Evaluated cells with a value (all of them) |
| Nature access | 1,354,611 | Evaluated cells with a value |
| Coastal risk | 79,473 | Evaluated coastal cells with a value |
| Hotspot cells, any service | 189,932 | Union of the five 5% sets; all inside the evaluated cells |
| Hotspots per service | 68,632 (N, sediment, pollination), 67,731 (nature access), 3,974 (coastal risk) | 5% tail of each service's valid cells, by SPC, in its adverse direction |

**Hotspot share: 189,932 / 1,372,621 = 13.8% of evaluated cells.** In text: "1.37 million cells".

## Denominators for group results

Relative prevalence (`data/processed/tables/hotspot_area_stats.csv`, built by
`analysis/rebuild_hotspot_area_stats.R`) uses, for each service, the evaluated cells that have that
service and the grouping. Groups excluded from reporting: High income nonOECD, and cells with no
group. Shares are taken among grouped cells, so about 45% of coastal risk hotspots (coastal-edge cells
with no biome) and about 40% (no region or income group) sit outside the group results.

| Grouping | Evaluated cells with the grouping |
|---|---:|
| World Bank region | 1,323,140 |
| Income group (excl. High income nonOECD) | 1,292,444 |
| WWF biome | 1,321,423 |

## Numbers that are NOT the universe

- **1,302,099**: an older sum of group totals from a previous `hotspot_area_stats.csv`. Retired;
  do not cite.
- **1.37 million coastal risk points** (`coastal_risk_tnc_esa1992_2020_20251224_013825.gpkg`):
  shoreline points, a different unit from grid cells. The similar magnitude is a coincidence.
- **Before 2026-10-07** the prevalence table counted Lakes and Rock & Ice cells in group
  denominators. Fixing it moved the lower-middle vs high-income OECD ratio from 2.4x to 2.3x and a
  few biome values by 0.1. Previous table kept as
  `hotspot_area_stats_pre_universe_fix_20261007.csv`.

## Known small discrepancy

Pollination's production hotspot set differs from a re-derived 5% tail by 34 cells (17 swapped)
because of tied values at the cutoff. Counts match exactly; nothing downstream depends on which tied
cells are in.
