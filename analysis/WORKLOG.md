# Worklog — Global NCP Hotspots (v1.3.4)

### 2026-07-28 (later same day)

#### Near-miss: `landgrid_1_clean_enriched_4326.gpkg` has no ID column at all — confirmed `10k_change_calc.gpkg` as the safe master-grid source for the 5-service redesign

While building `scripts/extract_hotspots_5service.R`, copied `hotspot_extraction.qmd`'s geometry-loading fallback (reads `data/vector_basedata/landgrid_1_clean_enriched_4326.gpkg` when `grid_sf` isn't already in memory, tries `orig_fid`/`grid_fid`/`id` in turn, falls back to `geom_sf$fid <- seq_len(nrow(...))` if none exist). That file's actual columns are `Id` (junk, all zeros), `RasterVal`, `c_fid`, `country`, `continent`, etc. — **none of orig_fid/grid_fid/id exist**, so the fallback fired, producing a purely positional fake ID with no relationship to any other file's cell identity. This is the same failure class as the 2026-07-08 LCC crosswalk bug and the 2026-07-24 many-to-one join bug — the fourth instance of this project being bitten by ungoverned grid-identity assumptions. Caught this time only because a `stopifnot` fid-coverage check happened to fire; not guaranteed in general.

**Fix**: used `data/processed/10k_change_calc.gpkg` instead (has a real `grid_fid` column). **Verified, not assumed**: joined its `grid_fid` against `hotspots_global_pct.gpkg` (the canonical, manuscript-verified 8-service hotspot output) — zero mismatches across all 225,113 hotspot cells on every shared attribute (c_fid, continent, region_un, subregion, nev_name, region_wb, income_grp, WWF_biome). This confirms the two files share the same true identity space, not just a coincidentally-matching column name.

**Not fixed, flagged as a fast-follow, not blocking tonight's redesign work**: this repo has several similarly-named grid-like gpkgs (`landgrid_1_clean_enriched_4326.gpkg`, `10k_change_calc.gpkg`, `10k_lcc_granular_metrics.gpkg`, `AOOGrid_10x10km_land_4326_clean.gpkg`, one with a literal timestamp in its filename) with no documented hierarchy or canonical-source statement anywhere. That ambiguity is the root cause enabling all four grid-identity incidents to date. Recommend a follow-up pass: document (or better, rename/consolidate) so there is one unambiguous, well-named canonical master grid file, referenced consistently, rather than several similarly-named candidates of unknown relative authority.

### 2026-07-28

#### Figure 9 (compound-risk serviceshed multiplier) showed >10B people — confirmed a Global-row double-counting bug, fixed

Becky and Steve flagged this in a 2026-07-21 meeting (Jerónimo not present): the compound-risk
dumbbell chart (`outputs/plots/downstream_exposure_dumbbell_compound.png`, Figure 9 in the paper)
appeared to show over 10 billion people at the "≥1 service" tier — impossible, exceeds world
population.

**Root cause, confirmed exactly**: `analysis/plot_multiplier_effect.R`'s compound-tier
aggregation (`compound_connected`) summed `population` from `outputs/tables/
exposure_comparison_compiled.csv` grouped only by `overlap_category`/`exposure_type`, with no
filter excluding the `country == "Global"` row that the CSV also carries (a pre-computed total
across all 224 countries, alongside the per-country rows). Summing both together exactly doubled
every value: "≥1 service" came out as 14.80B instead of the true 7.40B. The per-region/income/
biome/country breakdowns (the Annex charts, `analysis/plot_multiplier_effect.R`'s per-grouping
loop) were **not** affected — they already filtered out their own grouping column's "Global"
value.

**Second, smaller issue fixed in the same pass**: the script defined "Connected Beneficiaries" as
`travel_footprint` alone, not `combined_total` (the union of downstream + travel-access reach)
that `02-methods.qmd` actually documents as the definition. Switched both the compound chart and
the per-grouping charts to `combined_total` for consistency.

**Verified: no manuscript/book/presentation text needed to change.** The prose numbers (3,065M
in-situ; 6,011M/3,756M/7,584M connected at 2+/3+/1+ tiers; 2.5×/5×/8× multipliers) were already
sourced correctly from `06-hotspot-WHO.qmd`'s own traceability table, not from the buggy script —
only the image itself was wrong. That mismatch (chart visually contradicting correct text) is
almost certainly what read as "the numbers don't add up."

**Also**: added an explicit 10B gridline to the compound chart (previously topped out at 3B,
making the correct ~7.6B point look like it was off the scale) and removed
`outputs/plots/downstream_exposure_dumbbell.png` — an orphaned, unreferenced duplicate from an
earlier iteration of this script, superseded by the `_compound` version, nothing in the repo
generated or pointed to it anymore.

**Files changed**: `analysis/plot_multiplier_effect.R`. Regenerated:
`downstream_exposure_dumbbell_compound.png`, `exposure_multiplier_dumbbell_{region_wb,income_grp,WWF_biome}.png`.
Paper, book, and presentation re-rendered to pick up the corrected figures.

### 2026-07-24

#### Attribution-gap numbers corrected AGAIN (34.5%/65.5% was itself inflated by a many-to-one join) — now 34.2%/65.8%, odds ratio 10.79

**Root cause:** `scripts/compute_attribution_true_union.R`'s crosswalk join (`lc_grid_fid` → `master_fid`, built 2026-07-08) is necessarily many-to-one: the LC source grid has 1,691,819 rows vs. the master grid's 1,522,073, so nearest-centroid matching lets up to 9 LC rows share the same nearest master cell. The script used `nrow(lc)` on the joined-but-not-deduplicated table as both the total grid denominator (`n_grid`) and, via `lc$grid_fid[keep]`, each driver's hotspot-cell count — silently counting duplicate LC-side rows as distinct master cells in both places. Inflated `n_grid` by ~11% (1,681,849 counted vs. 1,515,620 actual distinct master cells) and inflated individual driver-hotspot counts unevenly.

**Fix:** after the crosswalk join, keep only the nearest match (min `match_dist_m`) per master `grid_fid` before computing anything downstream (`group_by(grid_fid) %>% slice_min(match_dist_m, n = 1, with_ties = FALSE)`).

**Corrected numbers (supersedes 2026-07-08's 34.5%/65.5%, odds ratio 12.17):**
- ES-hotspot ↔ LCC-driver-union overlap: **34.2%** (77,058 / 225,113 cells), not 34.5%.
- Background rate among non-ES cells: 4.6% (union covers 9.0% of the 1,515,620-cell grid, not 8.2% of 1,681,849).
- Odds ratio **10.79** [10.66–10.92], risk ratio 7.44, *p* ≈ 0 — still a strong, highly significant positive association, magnitude only.
- Per-driver risk ratios shifted much more than the union (uneven duplicate inflation): Crop_Exp 36.6→**97.5**, Forest_Loss 16.7→**25.4**, Urban_Exp 7.8→**9.1**, Grassland_Loss 4.4→**4.8**, Grassland_Gain 3.9→**4.2**. Ranking order (weakest→strongest) unchanged; the true associations were previously *understated*, not overstated.
- Attribution gap: **65.8%**, not 65.5% — same "coverage limitation" reading holds, magnitude only.

**Manuscript/book/presentation:** all attribution-gap numbers rewritten across `01-problem.qmd`, `05-drivers-WHY.qmd`, `08-conclusions.qmd`, `index.qmd`, `paper_draft.qmd` (including Table 3 and its new "how to read risk ratio vs. odds ratio" callout), `presentation.qmd`. Same-session, separately: standardized all these files on **risk ratio** as the lead effect-size statistic (was inconsistently odds ratio in some places, risk ratio in others, with per-driver odds-ratio and risk-ratio values never reconciled against each other) — odds ratio now noted as a secondary, equivalent statistic everywhere. `05-drivers-WHY.qmd`'s methodological-correction section now documents all three corrections (threshold mismatch → grid-id mismatch → many-to-one join) so the history stays traceable. All still flagged pending Becky/Steve review.

**NOT fixed, deferred until after today's paper-review meeting:** `analysis/hotspot_extraction.qmd` has the *exact same* many-to-one join pattern, unfixed, in two places (the `geom_sf` enrichment chunk and the `lc-hotspot-overlap-setup` chunk — both load `10k_lcc_granular_metrics.gpkg` and join through the same crosswalk without deduplicating). This produces the per-service driver-overlap table (`lcc_es_hotspot_overlap_pct.csv`, shown in `05-drivers-WHY.qmd`'s reactable) and several driver-hotspot maps/heatmaps (`global_lcc_driver_map.png`, `global_lcc_driver_hotspots_map.png`, `heatmap_driver_overlap_pct.png`/`_abs.png`) — these likely carry the same inflation and have NOT been corrected. Deliberately not touched today: fixing it means rerunning the full hotspot-extraction notebook (regenerates maps/gpkgs), which is the same notebook that caused the 2026-07-08 striping-bug incident — too risky to do with under 40 minutes before a live paper-review meeting. **Next priority after the meeting**: apply the same nearest-match dedup fix to both chunks in `hotspot_extraction.qmd`, rerun, regenerate the affected maps/heatmaps/CSVs, and re-check every per-service number and image against the corrected output.

### 2026-07-08

#### Striping bug root-caused and fixed; attribution-gap numbers corrected AGAIN (8.2%/91.8% was itself wrong) — now 34.5%/65.5%, odds ratio 12.17

**Root cause found:** not the positional-join bug hypothesized at the end of the 2026-07-07 session. `10k_lcc_granular_metrics.gpkg`'s own `grid_fid` column indexes a *different* source grid (`AOOGrid_10x10km_land_4326_clean.gpkg`, since deleted from disk, 1,691,819 cells) than the master grid used everywhere else in the pipeline (`landgrid_1_clean_enriched_4326.gpkg`, 1,522,073 cells). `hotspot_extraction.qmd`'s fid-detection logic only recognized lowercase `orig_fid`/`grid_fid`/`id` columns; the master grid's real columns are `Id` (capital, all zeros), `c_fid` (a country code), and `RasterVal` (the actual unique cell id, never checked). None matched, so it fell back to `geom_fid <- seq_len(nrow(...))` — the exact fallback an old code comment warned against. This coincidentally worked for `plt_long`/`10k_change_calc.gpkg` (identical row count/order to the master grid, confirmed 0m offset) but not for the LCC file (different grid entirely). Verified: using LCC `grid_fid` values as positional lookups into the master grid gave a 100% mismatch rate (median offset 8,600 km).

**Bigger discovery:** `scripts/compute_attribution_true_union.R` — written 2026-07-07 to fix the stale 24%/76% bug — had the *same* bug: it intersected `es$grid_fid` (master-grid scheme) directly against `lc$grid_fid` (LCC's own scheme). So the "corrected" 8.2%/91.8% (odds ratio 0.88, "no net excess co-occurrence") figures reported yesterday were themselves wrong, built on the same class of error as the thing they were fixing.

**Fix:** `scripts/build_lc_grid_fid_crosswalk.R` (new) — nearest-centroid spatial join between the two grids (same bbox/CRS, confirming same underlying tessellation); 99.41% of cells match at ~0m, 0.59% have no correspondence and are dropped. Writes `data/processed/lc_grid_fid_to_master_fid_crosswalk.csv`. Applied at the source in `analysis/hotspot_extraction.qmd` (both places `10k_lcc_granular_metrics.gpkg` is loaded — the `geom_sf` enrichment chunk and the `lc-hotspot-overlap-setup` chunk, which also fixes the downstream `lc-hotspots-export-by-group` and `lc-hotspot-overlap-calc` chunks since they consume the same corrected `lc_df`) and in `compute_attribution_true_union.R`.

**Corrected numbers (supersedes 2026-07-07's 8.2%/91.8%):**
- ES-hotspot ↔ LCC-driver-union overlap: **34.5%** (77,668 / 225,113 cells), not 8.2%.
- Background rate among non-ES cells: 4.1% (union covers 8.2% of the 1,681,849-cell grid overall).
- Odds ratio **12.17** [12.03–12.32], risk ratio 8.32, *p* ≈ 0 — a strong, highly significant positive association, not "no net excess co-occurrence."
- Every individual driver strongly positive (previously reported as weak/mixed): Crop_Exp OR 44.0, Forest_Loss OR 19.9, Urban_Exp OR 8.0, Grassland_Loss OR 4.8, Grassland_Gain OR 4.2.
- Attribution gap: **65.5%**, not 91.8% — read as a coverage limitation (LCC monitoring reaches ~1/3 of severe ES decline), not an absence of relationship.
- This is closer to, and stronger than, the *original* pre-2026-07-07 framing, not the reframed one. Per-service driver-overlap table also regenerated (`lcc_es_hotspot_overlap_pct.csv`/`_abs.csv`) — same bug, same fix, per-service overlaps now much higher (up to 46.9% for Sed_Ret_Ratio × Forest_Loss).

**New/changed scripts:**
- `scripts/build_lc_grid_fid_crosswalk.R` (new) — builds the crosswalk; rerun if either source grid file changes.
- `scripts/compute_attribution_true_union.R` (patched) — remaps `lc$grid_fid` through the crosswalk before intersecting with `es_fids`.
- `analysis/hotspot_extraction.qmd` (patched) — both `10k_lcc_granular_metrics.gpkg`-loading chunks now remap through the crosswalk; source-level fix, persists across re-renders.
- `scripts/mapping/make_lcc_overview_map.R` (patched) — driver classification now includes "Other (Grassland only)" instead of dropping those cells (previously ~15,632 cells silently excluded from the map).
- `scripts/mapping/make_attribution_map.R` (patched) — the catch-all class was mislabeled "Attribution Gap (Change without Conversion)"/"Change Occuring Off-Pixel", implying zero conversion; renamed to "Below Top-5% LCC Threshold" to correctly convey "conversion may be present, just not in the top-5% intensity tier."
- `scripts/mapping/make_lcc_true_overlap_map.R` (kept as a verification tool, not a manuscript deliverable) — its output PNG was retired since the consolidated driver-hotspots map (with the grassland class added) now covers the same ground; this script was the one that originally surfaced the striping bug and remains useful for spot-checking the crosswalk fix.

**Manuscript/book/presentation:** all attribution-gap numbers and interpretation rewritten across `01-problem.qmd`, `05-drivers-WHY.qmd`, `08-conclusions.qmd`, `index.qmd`, `paper_draft.qmd`, `presentation.qmd`, `scripts/audit_claims.R`. Framing shifted from "no net excess co-occurrence, mixed by driver" to "strong positive association where conversion is intense, but limited coverage (misses ~2/3 of severe ES decline)" — closer to the pre-2026-07-07 thesis, restated with corrected, verified numbers. `05-drivers-WHY.qmd`'s methodological-correction callout now documents both prior errors (threshold mismatch, then grid-id mismatch) so the history is traceable. All still flagged pending Becky review, 2026-07-09.

**Maps consolidated:** `global_lcc_true_overlap_map.png` (diagnostic companion, ES∩LCC subset) removed as a separate deliverable now that `global_lcc_driver_hotspots_map.png` (all LCC driver hotspots) includes the grassland-only class the companion map introduced. Also fixed a pre-existing caption/content mismatch: several manuscript captions claimed `global_lcc_driver_hotspots_map.png` shows the ES∩LCC intersection — it does not, and never did; it's the unfiltered driver-hotspot footprint. Captions corrected to describe the image accurately.

**Lesson learned for future pipeline runs:** this entire investigation traced back to the granular LCC computation (`LC_change.qmd`) having been run against `AOOGrid_10x10km_land_4326_clean.gpkg`, a different grid export than `landgrid_1_clean_enriched_4326.gpkg` (the master grid used by every other stage). If this analysis is redone in the future (e.g., more time points, finer resolution, regional focus), start every stage — including the LCC/diffeR computation — from the *same* master grid file, or explicitly carry forward a real, stable cell ID (not a fallback `seq_len()`) from day one. That single inconsistency, introduced once early in the pipeline, propagated silently for months and nearly derailed the paper's headline finding the night before a PI review meeting.

**Deferred:** group-level driver hotspot files (`drivers_by_group/pct/{income_grp,region_wb,nev_name,WWF_biome}/`) not regenerated — nothing currently consumes them. Future work idea (not yet started): explore a sliding scale of top-N% thresholds (beyond the fixed 5%) and driver combinations for the attribution-gap analysis, to characterize how sensitive the co-occurrence rate is to the threshold choice rather than reporting a single cutoff.

### 2026-07-07 (cont.)

#### Possible striping artifact in LCC driver-hotspot maps — flagged, NOT diagnosed, top priority next session

User spotted an unnatural-looking horizontal banding pattern in `outputs/plots/maps/global_lcc_driver_hotspots_map.png` and in a new comparison map built the same session (`global_lcc_true_overlap_map.png`, see `scripts/mapping/make_lcc_true_overlap_map.R` — plots the TRUE intersection of ES hotspots and LCC driver hotspots, 17,885 cells / 7.9%, built as a non-destructive companion to the existing driver map for visual comparison). Correctly recalled two historical precedents in this log: the 2026-04-08 "Fragment Bug" (`gdf.explode()` fragmenting grid cells, causing striping + duplicated data, fixed via `st_intersects` re-aggregation, v1.3.1) and the 2026-04-28 "Spatial Alignment Crisis" (`seq_len()` reassignment bug scrambling spatial joins across `process_data.qmd`/`hotspot_extraction.qmd`/`hotspot_synthesis.qmd`, "diagnosed and eliminated").

**One diagnostic check done before stopping:** compared against `outputs/plots/maps/first_look_map_pct.png` — the canonical, heavily-reviewed ES-hotspot-only map. That map shows **no striping** — hotspot cells follow organic, real ecological boundaries (Sahel, Amazon, SE Asia, agricultural belts), clean. This means the banding is **not** the old pipeline-wide bug recurring; it appears specific to the LCC-driver-hotspot data product (`data/processed/hotspots/drivers_by_group/pct/global/hotspots_global_pct.gpkg`), not the canonical ES-hotspot pipeline.

**Not yet done:** actually finding the root cause. Starting point identified — `grep -rln "drivers_by_group"` surfaces `analysis/hotspot_extraction.qmd` as the only candidate for where this gpkg is actually built (the other 3 hits — `make_attribution_map.R`, `make_lcc_driver_map.R`, `make_lcc_overview_map.R` — are downstream map consumers, not the source). Next session should trace that chunk in `hotspot_extraction.qmd` and check for a similar fragmentation/join issue to the two historical bugs above, before trusting the driver-hotspot maps or re-deriving anything visual from that gpkg further.

**Implication for the numbers (not just the maps):** `scripts/compute_attribution_true_union.R` (the 8.2%/91.8% true-union calculation from earlier today) reads directly from the raw `10k_lcc_granular_metrics.gpkg`, not from the `drivers_by_group` derivative — a different code path. That does NOT mean it's automatically clean; it just hasn't been checked for the same class of bug yet. Given how much rides on the attribution-gap numbers (already flagged provisional pending Becky's 2026-07-09 review), this needs to be ruled out explicitly, not assumed.

**Update — user confirmed the raw big file is very likely NOT the problem.** Two more maps checked, both derived directly from the raw LC gpkgs with no hotspot-consolidation step: `global_lcc_driver_map.png` (from `make_lcc_driver_map.R`) and `global_lcc_net_change_map.png` (the first half of `make_lcc_overview_map.R`, straight off `10k_lcc_metrics.gpkg`) — **neither shows striping.** Only the second half of `make_lcc_overview_map.R` (which reads the *consolidated* `drivers_by_group` gpkg) does. This pins the problem specifically to the ES-hotspot/LC-change-hotspot **assembly** step, not the underlying week-long `10k_lcc_granular_metrics.gpkg` computation — good news, since that computation does not need to be rerun.

**Further evidence, and a diagnostic plan.** User noted the striped map (`global_lcc_driver_hotspots_map.png`) looks *regionally* wrong, not uniformly wrong: South America, East Asia, Java/Borneo, and the Caucasus look right; Russia, Sub-Saharan Africa, and parts of South America show stripes. Also noted `global_attribution_gap_map_min2_pct.png` (from `scripts/mapping/make_attribution_map.R`, also an ES↔LCC overlap map) is clean. Working theory (user's, and it fits the evidence well): a **positional join bug** — code assuming two tables share row order and merging by position (`cbind`, direct column assignment) instead of an explicit `left_join(..., by = "grid_fid")`. A constant-offset positional misalignment would land wrong in large contiguous blocks (if the grid is stored in spatially-clustered/latitude-sorted order) while landing right elsewhere by chance — exactly the "some regions look right, some look wrong" pattern reported, and structurally the same shape as the 2026-04-28 `seq_len()` bug.

**Plan for next session:**
1. Read the exact chunk in `hotspot_extraction.qmd` that builds `drivers_by_group/pct/global/hotspots_global_pct.gpkg` — pin down precisely how geometry gets attached / how the 5 per-driver hotspot flags get merged; look for positional (row-order-dependent) joins vs. explicit `by = "grid_fid"` keyed joins.
2. Read `make_attribution_map.R` (source of the clean `global_attribution_gap_map_min2_pct.png`) as a **working reference implementation** for the same kind of ES↔LCC overlap — diff its join approach against #1.
3. Check intermediate row counts at each join step in the suspect chunk — silent misalignment often shows up as an unexpected row count even before checking geometry.
4. Fix: very likely swapping the positional merge for an explicit `left_join(..., by = "grid_fid")`, matching both the working reference script and the 2026-04-08 fix pattern (`st_intersects` + `group_by %>% summarise` re-aggregation).
5. Verify: regenerate `drivers_by_group`, rebuild the two striped maps, confirm visually clean, then recompute true-union numbers from the fixed file and compare against `compute_attribution_true_union.R`'s numbers (raw-gpkg-based, bypasses this step entirely). Closer convergence than today's 8.2% vs 7.9% would confirm both the fix and that today's headline numbers were sound all along.

**Idea for later (not step 1, a fix-quality consideration once the root cause is confirmed):** user proposed doing the ES↔LCC assembly via an actual spatial join (`st_intersects`/`st_join`) rather than an attribute-table join, and pointed out this grid is regular (equal-area squares, no complex overlapping shapes like the biome/country polygons that made the original Fragment Bug expensive) — so a geometry-based join here should be cheap and exact, not the computational burden it would be for irregular polygons. Worth doing once the root cause is confirmed: a plain `grid_fid` attribute join is cheaper *if* the IDs are trustworthy across both files; a geometry-based spatial join is slightly more expensive but doesn't depend on trusting the ID at all, matching by actual location instead — the safer choice if there's any chance the IDs drifted between pipeline stages (plausible, given that's essentially what the 2026-04-28 bug was). Check which situation applies before picking one. Matches the precedent that already worked once (`st_intersects` + `group_by %>% summarise` re-aggregation, 2026-04-08 fix).

**Contingency — if `grid_fid` turns out to have actually drifted between files, don't just patch this one output.** If the keyed-join test (plan step 1 above) reveals the `grid_fid` values themselves are inconsistent between the two files — not just a join-method mistake in this one script — the job isn't done by switching this one map/table to a spatial join and moving on. That fixes the symptom here but leaves the same drift latent everywhere else in the pipeline that still trusts `grid_fid` as a stable key, ready to resurface the next time this data gets rebuilt or a new downstream product joins on it. In that case, trace back through the pipeline to find **where** the two numbering schemes diverged (which processing step re-sorted, re-filtered, or re-generated one grid's `grid_fid` without preserving correspondence to the other) and fix it at the source, so every future join on `grid_fid` — not just this one — is trustworthy again.

### 2026-07-07

#### Attribution gap recomputed: 24%/76% was stale, corrected to 8.2%/91.8% (no net excess co-occurrence)

**What happened:** A systematic claim-verification pass across ch03–ch07 flagged that `05-drivers-WHY.qmd`'s headline "24% co-occurrence / 76% attribution gap" traced to `data/processed/tables/lcc_es_hotspot_overlap.csv`, generated before the driver set was finalized at 5 transitions (Forest Loss, Cropland Expansion, Urban Expansion, Grassland Loss, Grassland Gain). That file only covers 2 drivers and its per-row percentages (4–47%) don't reconcile with the current top-5%-severity threshold used everywhere else — reconstruction (matching magnitude and testing directly) indicates it used a much looser "any detected conversion" threshold instead. The live per-driver pipeline (`analysis/hotspot_extraction.qmd` → `lcc_es_hotspot_overlap_pct.csv`/`_abs.csv`) was fine on its own terms, but never computed the *union* across all 5 drivers — only per-driver marginals existed, so no script had ever produced a defensible single headline number.

**What was built:** `scripts/compute_attribution_true_union.R` — computes the true cell-level union across all 5 current drivers using the same top-5%-cutoff definition as the live pipeline, and tests the overlap against the canonical ES-hotspot cell set (225,113 cells, same set behind the Ch06 in-situ population figures) with a proper chi-square/Fisher's exact test rather than an eyeballed ratio. Outputs: `lcc_driver_magnitude_summary.csv`, `lcc_es_hotspot_per_driver_risk.csv`, `lcc_es_hotspot_true_union.csv` in `data/processed/tables/`.

**Result — a directional flip, not just a magnitude correction:**
- True union overlap: **8.2%** of ES hotspot cells (vs. driver-union base rate of 9.3% among non-hotspot cells) — statistically indistinguishable from background (odds ratio 0.88, 95% CI 0.86–0.89). The naive "~5% expected by chance" framing in the old text was also wrong: the correct chance baseline for a union of 5 overlapping 5%-tail drivers is ~9.1% of the grid, not 5%.
- Per-driver split is real and mixed-sign: Forest Loss (RR 1.18) and Urban Expansion (RR 1.22) show a small positive association with ES hotspots; Cropland Expansion (RR 0.80), Grassland Loss (RR 0.79), and Grassland Gain (RR 0.86) show a small negative one. The aggregate near-zero result is these effects cancelling out, not the absence of a pattern.
- Corrected attribution gap: **91.8%**, not 76%.
- Framing choice made with the user: report the corrected finding as a *stronger* version of the chapter's actual thesis ("is land cover monitoring alone sufficient?") rather than softening or hedging it — the answer becomes "no, and closer to not-at-all, except for a modest forest-loss/urban-expansion signal."

**Files updated with corrected numbers and framing:** `docs/manuscript/chapters/05-drivers-WHY.qmd` (full section rewrite + methodological-correction callout), `01-problem.qmd`, `08-conclusions.qmd`, `index.qmd`, `paper_draft.qmd` (also fixed a pre-existing inconsistency where the Methods section said "three conversion drivers" but Results referenced a 4th — aligned to 5, matching the book), `docs/presentations/presentation.qmd`. `scripts/audit_claims.R`'s Audit 4 rewritten to check the new `lcc_es_hotspot_true_union.csv` instead of the stale file (its Audit 3 was also separately broken — schema drift in `hotspot_pop_exposure.csv` — patched to skip gracefully with a pointer to the correct verification path instead of crashing).

**Housekeeping:** `lcc_es_hotspot_overlap.csv` moved to `data/processed/tables/_deprecated/` (not deleted) with a README explaining its provenance and why it's superseded. RSQLite installed for faster attribute-table-only queries against large gpkg files going forward.

**Not yet resolved:** `paper_draft.qmd` and `presentation.qmd` still say "252,215" hotspot cells, while the book's Ch06 traceability table (and a fresh direct query of `hotspots_global_pct.gpkg`) says 225,113 for the same `hotspot_count >= 1` definition. Flagged for the user, not corrected — touches the paper's abstract headline number and deserves its own verification pass, separate from this one.

**Backup:** branch `pre-attribution-gap-rewrite` created at commit `1d6325f`, immediately before this rewrite began, in case any of this needs to be unwound.

#### Rigor/tone review of the attribution-gap rewrite, terminology pass, and provisional-status flags

Follow-up session, same day, reviewing the rewrite above before it goes any further.

**Rigor check on the new prose.** User pushed back on the initial rewrite in three ways, all addressed:
1. Asked for the overlap numbers and statistics (odds ratio, 95% CI, chi-square, p-value) to be explained in plain terms with the actual counts, not just summary percentages — done conversationally, then formalized as a `callout-tip` in `05-drivers-WHY.qmd` right where the odds ratio first appears, walking through the 4-cell contingency table → odds ratio → CI → p-value-vs-effect-size distinction step by step.
2. Flagged adjective inflation ("considerably sharpens," "More strikingly") and, more substantively, flagged that "suggests that mechanisms ... contribute substantially to observed functional decline" was speculation beyond what a co-occurrence analysis can support (no data measures those mechanisms directly). Reworded to "plausible contributing mechanisms, but this co-occurrence analysis cannot establish their relative contribution."
3. Objected to the paper (`paper_draft.qmd`) narrating its own revision history ("an earlier calculation... since corrected") — reviewers/coauthors don't need that, it belongs in this WORKLOG only. Removed from `paper_draft.qmd` (3 instances); kept in the book (`05-drivers-WHY.qmd`'s methodological-correction callout) and `presentation.qmd`'s internal status slide, since those are working documents where the transparency is useful.

**Terminology pass: "extreme" → "intense".** User found "extreme" vague/loaded, especially doubled up with "hotspot" in the same sentence ("extreme ecosystem service hotspot"). Went through all ~58 instances across 10 files (all book chapters, `index.qmd`, `paper_draft.qmd`, `presentation.qmd`) individually rather than blind find-replace, since usage split three ways:
- Adjective describing the threshold ("extreme (top 5%) land cover conversion") → swapped to "intense" (~44 instances).
- Noun form paired redundantly with "hotspot" ("localized extremes (hotspots)") → dropped entirely rather than replaced (8 instances) — e.g. ch03/ch09 now just say "hotspots."
- Genuine statistical-tail usage ("cells at the opposite extreme," "extreme percentiles," index.qmd's glossary heading "A Relative Extreme, Not an Absolute Threshold," and one instance in `paper_draft.qmd:198` the user explicitly asked to keep) — left as "extreme," these are technically precise, not vague.
- One unrelated instance (ch09: "extreme polar latitudes," about raster coverage) — untouched.

**Further adjective trim.** User pushed back again on my own replacement choices ("exceptionally severe," "unusually high") as still smuggling in unearned judgment/rarity claims. Principle applied: drop the intensifier or let the adjacent number carry the claim, rather than swap one evaluative word for another. Fixed in ch04 (parallel "moderately degraded" / "severely degraded" instead of "exceptionally severe") and ch08 (two instances — "unusually high relative intensities, experiencing... 3.5 to 8.5 times higher" → just "hotspot concentrations 3.5 to 8.5 times higher"; "particularly rapid service decline" → "rapid service decline").

**WHY-pillar framing questioned.** User asked whether ch05's chapter, filed under the book's "WHY" pillar (of the WHAT/WHERE/WHO/WHY four-question framework in `01-problem.qmd`), is actually answering a "why" question. Assessment: no, not in the causal sense — the chapter itself already disclaims causal attribution and its own title is "The Monitoring Gap: Is Land Cover Change Alone Enough?", not a causal-mechanism framing. It's really a monitoring-adequacy/detectability question. The chapter-level reframe (from the 2026-06-24 WHY-reframing session) never fully propagated to the ch01 pillar label. Not resolved yet — flagged for discussion, not fixed, per user's explicit "pause and think" instruction. Also found, in passing: `01-problem.qmd`'s four-question list assigns Chapter 5 to *both* "WHERE Analysis (Chapters 4–5)" and "WHY Analysis (Chapter 5)" — likely a stale range, should probably read "Chapter 4" for WHERE. Not fixed yet either.

**Provisional-status callouts added.** Given how much the attribution-gap section changed (direction of the finding flipped, not just the number) and that this hasn't been reviewed by Becky yet (meeting scheduled **2026-07-09**), expanded the callouts in both `05-drivers-WHY.qmd` (callout-important added above the existing methodological-correction callout) and `paper_draft.qmd` (new callout-important at the top of "The Spatial Attribution Gap" section, matching the paper's existing `[FLAG FOR BECKY]` style — flags the section for review without narrating correction history, consistent with the instruction above). Both explicitly say: do not cite the 8.2%/91.8% figures or the "strengthens the thesis" framing outside this working draft until reviewed.

**State at end of day:** nothing committed — still on `task/housekeeping`, backup at `pre-attribution-gap-rewrite` (commit `1d6325f`). Next session: user will review current state independently, bring additional wordiness instances found during their own read-through, and revisit the WHY-pillar framing question and the 252,215-vs-225,113 discrepancy (still unresolved, flagged in the entry above).

### 2026-06-24

#### Presentation polish (session 10) + WHY reframing across paper, book, and presentation

**Blank slides fully eliminated (second pass)**
Removed 8 additional `---` separators immediately *before* `#` section headers (Part 2: Methods, WHAT, WHERE, WHO, WHY, Regional Profiles, Synthesis, Thank You). First pass (session 9) removed `---` after section headers; these were the before-header instances also creating blank slides.

**Mermaid Analytical Pipeline: font and label fixes**
Reduced `fontSize` to `13px` via `themeVariables`; shortened longest node labels to reduce box overflow. Box sizing still tight — flagged as todo for future pass.

**Methods slides restructured: two new slides added**
- *Data Sources* renamed *Data Inputs* and split into three-column layout (ES / Land Cover / Socioeconomic)
- *Eight Ecosystem Services*: new slide with all 8 services in a table grouped by theme (Habitat & Access / Coastal Risk / Hydrological Regulation), plain-language descriptions, and a callout explaining the retention ratio distinction (declining ratio = functional degradation even without volume change)
- *Socioeconomic Context: The WHO Input Layers*: new slide with `global_socioeconomic_context_map.png` (4-panel: Pop, HDI, GDP, GINI); caption points to KS typology finding
- *Spatial Framework & Analytical Groupings*: kept, with italic grid note and cleaned caption

**WHY section: two new slides added**
- *What Land Cover Change Looked Like 1992–2020*: `global_lcc_net_change_map.png` as setup before attribution gap results
- Existing attribution gap slide unchanged

**Appendix: hotspot intensity boxplot added**
- `boxplots_unified/region_wb/boxplots_volumetric_pct.png` added as appendix slide with compact reading guide callout (wide box = variable, narrow = consistently extreme; Latin America punchline)

**WHY reframing — major conceptual work (presentation + paper + book)**

Core insight: the WHY question is not "what drives ES change" but "is land cover monitoring alone sufficient to track where ES provision is declining most?" — more honest about what the co-occurrence analysis actually measures and more novel as a contribution.

Changes applied across all files:
- **Presentation**: section divider → "WHY: The Monitoring Gap"; slide 4 WHY question → "Is monitoring land cover change alone enough to track where ES provision is declining most?"; mermaid node → "Monitoring Gap"; speaker notes updated
- **paper_draft.qmd**: Discussion section title → "The Sufficiency of Land Cover Monitoring as a Proxy for ES Provision Change"; Becky callout resolved (Option A applied — "consistent with the view that LC monitoring is insufficient", compressed to a short flag asking for an optional citation if she wants Option B)
- **ch01-problem.qmd**: WHY question heading → "Is land cover change alone enough to track ES provision decline?"; WHY framework bullets updated; chapter cross-reference corrected (8→5)
- **ch05-drivers-WHY.qmd**: chapter title → "The Monitoring Gap: Is Land Cover Change Alone Enough? (WHY)"; opening sentence → "To assess whether monitoring land cover change alone is sufficient..."; Summary callout header → "Five findings on the sufficiency of LC monitoring..."

**ch04 boxplot section: reading guide added**
Full callout explaining how to read the boxplots added before the tabset — axes, IQR, whiskers, color, wide vs narrow interpretation, direction of change per service type, and the key point that all cells are already global top 5%. All three ratio boxplot figures (previously captionless) now have proper captions. Latin America finding called out in region tab.

**Paper and book shared with Becky Chaplin-Kramer** for co-author review. Two open questions flagged for her:
1. Were InVEST SDR/NDR runs performed with fixed or era-specific climate inputs?
2. Optional Option B citation for the monitoring proxy claim (REDD+/IPBES literature)

**Nothing committed** — awaiting render review and Becky's feedback before committing.

### 2026-06-22

#### Population exposure numbers — full audit and correction

Traced all population exposure figures to their source data. Two errors found and corrected across Ch06, Ch08, paper_draft.qmd, and docs/methodology.md.

**Error 1 — Service-weighted sum reported as distinct individuals.**
`hotspot_pop_exposure.csv` stores one row per (service × socioeconomic bin). Summing `exposed_population` across all 8 services yields 5,286 million — but this counts each person once per qualifying service (avg 1.68 services per hotspot cell → 377,719 cell-service units across 225,113 unique cells). The correct count of distinct individuals in any hotspot cell is **3,065 million**, computed by joining `hotspot_count ≥ 1` cells from `hotspots_global_pct.gpkg` to `GHS_POP_E2020_GLOBE_sum` in `10k_change_calc.gpkg`.

**Error 2 — In-situ compound hotspot population was unverifiable and wrong.**
The figure of 8.6 million for "people living in 2+ service areas" could not be reproduced from any output file. The beneficiary CSVs for 2+ overlapping cells only store connected (downstream/access) populations in the billions, not in-situ counts. The correct in-situ figure for `hotspot_count ≥ 2` cells is **1,212 million** (85,599 cells).

**Verified numbers (from current data):**

| Filter | Cells | GHS-POP in-situ | Connected (union) | Multiplier |
|---|---|---|---|---|
| `hotspot_count ≥ 1` | 225,113 | 3,065 M | 7,584 M | 2.5× |
| `hotspot_count ≥ 2` | 85,599 | 1,212 M | 6,011 M | ~5× |
| `hotspot_count ≥ 3` | 41,025 | 445 M | 3,756 M | ~8× |

The 7.6B / 6,409M / 7,390M beneficiary figures (all hotspot cells) were verified correct.

The 700× and 1,700× multipliers previously stated were derived from the wrong 8.6M denominator and have been corrected to ~5× and ~8× respectively.

**Files updated:** Ch06 (data traceability callout + all prose + Key Takeaways), Ch08 (Key Takeaway #2), paper_draft.qmd (§Population Exposure), docs/methodology.md (§Population Exposure section with verified table; §Attribution section with symmetric 5%/5% threshold documentation). Changes not yet committed — awaiting render review.

### 2026-06-20

#### Book chapter polish (Phase 3, sessions 5–6)

*   **Chapter 7 (Regional Profiles) reviewed and cleaned:**  All `~X million` population placeholders replaced with `[TBD — extract from pop_exposure table]`. Two suspicious hardcoded numbers removed: "~580 million depending on water/food" (effectively LAC's entire population) and "~1,450 million smallholder farmers" (South Asia's full regional head-count, not just farmers). A `callout-warning` was added explaining how to extract real region-level numbers from the interactive pop_exposure table. "Next Steps" stub section removed.
*   **Key Takeaways sections added to all remaining chapters:** Chapters 4, 6, 7, and 8 now have substantive Key Takeaways callouts (five findings each). These synthesize the geographic concentration patterns (Ch4), cross-scale exposure logic and three socioeconomic typologies (Ch6), Global South burden and within-region heterogeneity (Ch7), and the five top-line policy messages including the pipeline-as-infrastructure framing (Ch8).
*   **Final attribution language fix in Ch8:** Bullet "(no detectable conversion)" in the policy section's Driver-Specific Strategies → "(no co-occurrence with extreme LCC)". Zero remaining instances of the old causal framing across all `.qmd` files.

#### Interactive Summary Statistics — Chapter 4

*   **DT replaced with `reactable`:** The two existing Summary Statistics tables (Hotspot Area Coverage, Compound Risk) were failing to show filter inputs in Quarto tabsets because DT's `filter = "top"` row does not initialize in hidden tabs. Switched to `reactable` (`filterable = TRUE`, `searchable = TRUE`), which initializes lazily and renders filter inputs correctly. All column names cleaned (snake_case → readable labels), numerics pre-rounded, default sort set to Relative Intensity / Mean Overlapping Services descending. Installed `reactable` (v0.4.5) + its dependency `reactR` into the project R library.

*   **Cross-dimensional analysis tab added:** A third tab "Country × Biome (Cross-dimensional)" was added to the Summary Statistics tabset. It loads `hotspots_global_pct.gpkg` via SQL (no geometry, 225K rows in ~1.4 s), pivots the 8 per-service hotspot flags to long format, and computes a (country × biome × service) cross-tabulation with n_hot, n_total, % hotspot area, % of global hotspots, expected share, and relative intensity. Produces 3,553 rows (country–biome–service combinations with ≥1 hotspot cell). The chunk is cached so it only runs on first render.

    Example query verified in R: Brazilian Mangroves — 83 cells total; Pollination 57/83 (68.7%, 2.25× expected); Coastal Risk 7/83 (8.4%, 4.78× expected — nearly 5×). Any similar combination (e.g., Indonesia Mangroves, India Tropical Dry Forests, South Korea Temperate Forests) can be queried live in the rendered HTML book without re-running R.

*   **README updated:** New "## Interactive Book Output" section added describing the three table types, their filter/sort capabilities, and example queries. Aimed at co-authors and future users who receive the rendered HTML and want to understand what analytical questions can be answered without re-running the pipeline.

### 2026-06-19
*   **Paper polish (Phase 3):** Completed Introduction AI-slop cleanup, added Scalability paragraph to Conclusions, swept all book chapters for attribution language. No issues found in chapters 03, 04, 06, 07, 09.
*   **Attribution framing hardened:** Rewrote Results §Spatial Attribution Gap (was making causal claims inconsistent with Discussion), tightened Abstract, Methods, and Conclusions to consistently use co-occurrence language. Table label changed from "Unmapped Degradation" → "No Detected LCC".
*   **Climate forcing mechanism clarified:** Expanded Discussion item 2 to explain the specific R-factor pathway by which precipitation changes propagate through InVEST SDR/NDR into modeled service outputs. Added IPCC AR6 (2021) and Nearing et al. (2004) to references.
*   **⚠️ OPEN QUESTION FOR BECKY — blocks submission:** Were the InVEST SDR/NDR model runs for 1992 and 2020 performed with (a) the **same fixed climate inputs** (e.g., one long-term-average WorldClim R-factor raster applied to both years) or (b) **era-specific climate inputs** for each year (e.g., year-matched CHELSA or ERA5 precipitation/erosivity)? This determines whether "Climate Forcing" is a direct or only indirect explanation for the 76% attribution gap, and therefore how strongly to hedge the Discussion language. A callout note has been added in `paper_draft.qmd` §Attribution Analysis to flag this for the next advisor meeting.

### 2026-06-16
*   **Repository Restructuring & Cleanup:** Conducted a major repository cleanup to align with FAIR principles and good industry practices. Transitioned the project from a standard R package structure to a broader reproducible research project structure, acknowledging its evolution into a large-scale analytical pipeline.
*   **Data Consolidation:** Unified data directories, ensuring that `C:\projects\global_NCP\data` contains the most recent canonical data, while deprecating redundant `home/` directories.
*   **Git Cleanup:** Removed a large number of temporary and untracked files from the `home/` directory (e.g., temporary Rscript runs and libloc files) from the git repository to ensure a clean and reproducible state.

### 2026-06-12
*   **Population Exposure Milestone:** Calculated the total 2020 GHSL population captured across the 1.3 million evaluated 10km grid cells (7,855,519,292 people).
*   **Near-Universal Exposure:** Verified that the 7.6 Billion "Connected Beneficiaries" represent **96.7%** of the evaluated global population. This massive share confirms that almost the entire global population is connected to at least one ecosystem service loss hotspot via downstream hydrological pathways or travel-access footprints.

### 2026-06-11
*   **Methodological Optimization (Population Exposure Extraction):** Bypassed the single-attribute limitation of `zonal_stats_toolkit` for the multi-level hotspot beneficiaries analysis. 
*   **Multi-Dimensional Slicing:** By extracting raw population data directly to the 1.5 million 10km grid cells (`landgrid_1_clean_enriched_4326.gpkg`) using `exactextract` (with `strategy="raster-sequential"` to prevent GEOS C++ crashes) and subsequently grouping in Pandas by `['country', 'region_wb', 'income_grp', 'WWF_biome']` simultaneously, we squash millions of rows into a lightweight, highly flexible CSV.
*   **Analytical Power Unlocked:** This structural decision allows downstream R scripts to effortlessly filter and cross-tabulate complex intersections (e.g., "Exposure in Low-Income countries within Sub-Saharan Africa") on the fly, without needing to re-run expensive spatial intersections.

### 2026-06-08
*   **Ground-Truth Narrative Audit:** Conducted a comprehensive, data-driven audit of all high-level claims in the synthesis chapters and manuscript draft using exact values from `hotspot_area_stats.csv` and the attribution scripts.
*   **Narrative Corrections:** Purged several "echo chamber" inaccuracies in the text. Verified that Lower-Middle Income countries face the highest relative intensity (1.19x absolute, 1.6x OECD), Latin America and East Asia are the true regional epicenters, and Mangroves are the most severely impacted biome (nearly 5x expected intensity). Excluded micro-states (area < 10,000 sq km) from country-level rankings, revealing South Korea, Jamaica, Malaysia, and Guatemala as top intensity spots.
*   **Hotspot Definition Refinement:** Clarified manuscript language to explicitly define hotspots based on the "extreme 5% of relative change values (Symmetric Percentage Change)", correctly identifying approx. 250,000 unique cells with at least one hotspot.
*   **Output Audit Artifact:** Established a permanent logging mechanism (`outputs/audit_summary.txt`) to maintain a paper trail of the core ground-truth statistics for peer review and manuscript defense.

### 2026-06-04
*   **Pipeline Robustness & Zombie Data Fix:** Identified and resolved a critical bug where `process_data.qmd` ingested a stale, misaligned coastal GPKG because the file loading was hardcoded to grab the top 3 files by date. Updated the script to dynamically load *all* GPKGs present in `summary_pipeline_workspace_ha`.
*   **Coastal Extraction Canonical Path:** Restored `analysis_configs/c_protection_synth.yaml` to point to the archived coastal risk rasters (`Rt_1992.tif`, etc.). Confirmed that calculating ratios natively on vectors and *then* rasterizing them is the only stable path, bypassing C-level crashes.
*   **Dateline Artifact Resolution:** Added `sf::st_wrap_dateline()` with `DATELINEOFFSET=180` to the final export step of `process_data.qmd` to prevent horizontal tearing artifacts when rendering the EPSG:4326 output in QGIS.
*   **Future-Proofing the Pipeline:** Refactored year-detection regex from hardcoded `"1992|2020"` to dynamic `"[0-9]{4}"`. Added prominent `[MANUAL UPDATE REQUIRED]` templates directly into the `process_data.qmd` script to guide future users on exactly how to drop specific years (for multi-year comparisons) or add new variables without breaking the analysis.

### 2026-06-03
*   **Geometry Crashes Finally Conquered:** After a grueling two-week struggle involving `GEOSException` crashes, memory leaks, and exploded geometries, we have finally established a mathematically sound and highly performant vector-extraction workflow.
*   **The Breakthrough:** The root cause of the crashes in Python/GEOS was isolated to a small number of malformed "poison polygons" during the C-level EPSG:4326 reprojection phase.
*   **Solution Implementation:** We consolidated the grid creation into a single, robust Python script (`build_master_grid.py`). It uses chunked reprojection (processing the 1.5M cell grid in blocks of ~7500). If a chunk fails the fast C-level reprojection, the script falls back to an isolated row-by-row projection, safely discarding the few mathematically impossible geometries while preserving the rest.
*   **Pipeline Success:** `summary_pipeline_landgrid.py` was successfully run against this new master grid (`landgrid_1_clean_enriched_4326.gpkg`) for both the 1992/2020 Services and the Socioeconomic Beneficiaries. The pipeline finished in ~13 minutes with zero crashes and zero duplicated/exploded fragments.
*   **Housekeeping:** Deleted redundant scratch scripts (e.g., `clean_grid_4326.py`, deprecated in favor of `build_master_grid.py`).
*   **R Consolidation:** `process_data.qmd` was overhauled to simply merge the pristine output GPKGs from the Python workspace based on the reliable `fid`.

### 2026-06-02
*   **Vector Data Enrichment Pipeline Stabilized:** After being blocked for over a week by intractable geometry and performance issues in the R-based `prepare_data.qmd` script, a robust Python-based solution has been successfully developed and executed.
*   **Problem:** The original R script was unacceptably slow and consistently failed with obscure `GEOSException` errors when performing spatial joins on the 1.5M-cell grid.
*   **Solution:** A new script, `Python_scripts/enrich_grid.py`, was created to handle this critical data preparation step.
    1.  **Performance:** The initial polygon-intersection approach was too slow. The script was re-engineered to use a much faster and more stable **centroid-based spatial join**. This reduced processing time from hours to minutes.
    2.  **Robustness:** Iteratively debugged a series of `KeyError` and `ValueError` exceptions related to inconsistent column names (`WWF_BIOME` vs. `WWF_biome`, `country` vs. `nev_name`) and internal `geopandas` state (`index_right` conflicts).
    3.  **Final Output:** The script successfully produced `landgrid_1_clean_enriched.gpkg`, a clean, attribute-rich vector grid containing all necessary biome and country/regional information. This file now serves as the canonical input for the main zonal statistics pipeline, unblocking all downstream analysis.

---

### 2026-05-27 (cont. 7)
*   **Final Strategic Pivot & Course Correction:** The `GEOSException: ...closed linestring` error continues to be completely intractable in the vector-based Python pipeline (`summary_pipeline_landgrid.py`), even with multiple aggressive cleaning patches. This confirms that the vector file's geometry issues are too severe to be reliably fixed on-the-fly in a multiprocessing environment.
*   **Definitive Solution:** The project is now fully reverting to the **hybrid raster-vector workflow** that was prototyped on 2026-05-26. This is the only robust path forward.
    1.  **Deprecate Vector Pipeline:** The `summary_pipeline_landgrid.py` script and its associated vector-based logic are now considered deprecated. All efforts will focus on the raster-based workflow.
    2.  **Create Zone Raster:** The `analysis/create_zone_raster.R` script provides the stable "zone" input needed for Python.
    3.  **Implement Raster Pipeline:** A new configuration (`analysis_configs/services_raster.yaml`) has been created to drive `summary_pipeline_rasterzones.py`. This script performs all zonal statistics using the zone raster, completely avoiding vector geometry processing in Python and thus eliminating the `GEOSException`.
    4.  **Simplify R Consolidation:** The `analysis/process_data.qmd` script has been overhauled. It no longer needs to perform complex spatial joins or aggregations to fix "exploded" fragments. It now reads the clean CSV output from the raster pipeline and performs a simple, fast `left_join` by `fid` against the master grid.
*   This new workflow is not only more robust and error-free but also significantly simpler and faster. The `README.md` has been updated to reflect this as the new canonical procedure.

---

### 2026-05-27 (cont. 6)
*   **Python Pipeline Failure (`GEOSException` Persists):** The `closed linestring` error continues to occur in the `zonal_stats` worker process during reprojection, even after the `buffer(0)` patch was applied.
*   **Root Cause Analysis:**
    1.  This confirms that this dataset contains exceptionally stubborn geometry invalidities.
    2.  The file I/O cycle where the main process writes a temporary GeoPackage and the worker process reads it is the most likely source of re-introducing these subtle errors.
    3.  The single `buffer(0)` call in the worker is insufficient. It may even be creating empty or invalid sliver polygons from highly malformed inputs, which are not being filtered out before the `to_crs()` call.
*   **Resolution:**
    1.  **Aggressive Just-in-Time Cleaning:** The `zonal_stats` function in `summary_pipeline_landgrid.py` has been patched with a much more robust cleaning sequence. It now performs a `buffer(0).make_valid()` and then explicitly filters out any empty or near-zero-area geometries that may have been created. This mirrors the extensive cleaning performed in the main process and ensures the data is as clean as possible immediately before the sensitive reprojection step. This should finally resolve the recurring geometry exceptions.

---

### 2026-05-27 (cont. 5)
*   **Python Pipeline Failure (`GEOSException`):** The pipeline is now running past the `FileNotFoundError` but fails during zonal statistics with a `shapely.errors.GEOSException: IllegalArgumentException: Points of LinearRing do not form a closed linestring`.
*   **Root Cause Analysis:**
    1.  This error occurs during the `gdf.to_crs()` reprojection step inside the `zonal_stats` function.
    2.  This is a classic geometry validity issue. Although the `main()` function performs extensive cleaning (`buffer(0)`, `make_valid()`) before writing a temporary GeoPackage, this error indicates that either the cleaning was insufficient, or that the file I/O cycle and/or the reprojection operation itself is re-introducing or exposing subtle invalidities.
    3.  The history of this project (`WORKLOG.md`) shows a recurring theme of geometry issues when passing data from R's `sf` package to Python's `geopandas`.
*   **Resolution:**
    1.  **Pipeline Hardening:** A patch has been applied to `summary_pipeline_landgrid.py`. The `zonal_stats` function will now re-apply the `gdf.geometry.buffer(0)` cleaning trick immediately after reading the vector data. This ensures that any geometry issues are fixed just-in-time before the reprojection is attempted, making the process more robust against these recurring data integrity problems.

---

### 2026-05-27 (cont. 4)
*   **Recurring Python Pipeline Failure (`RasterioIOError`):** The pipeline failed again with a `No such file or directory` error, this time for `n_retention_ratio_2020.tif`.
*   **Root Cause Analysis:**
    1.  The error message (`/data/base_years_ha/n_retention_ratio_2020.tif: No such file or directory`) is identical in nature to the previous failure. It indicates the Python script is looking for a file in a path that is missing the `/raw/` subdirectory.
    2.  The fix applied in the previous step (updating all paths in `analysis_configs/services_slim.yaml` to include `/raw/`) correctly resolves this issue for all raster layers.
    3.  The fact that the pipeline failed again on a *different* file but with the *same* path issue strongly indicates that the pipeline was executed using the original, un-patched YAML configuration file.
*   **Resolution:**
    1.  **Action Required:** The user must ensure they are running the Python pipeline using the version of `analysis_configs/services_slim.yaml` that was corrected in the previous step. No new code or configuration changes are needed.
    2.  The `summary_pipeline_landgrid.py` script remains robust enough to handle individual task failures if the YAML is partially correct, but the root cause of the `FileNotFoundError` must be addressed by using the fully corrected configuration file.

---

### 2026-05-27 (cont. 3)
*   **Python Pipeline Failure (`RasterioIOError`):** The Python pipeline failed during zonal statistics with a `No such file or directory` error for `sed_retention_ratio_2020.tif`.
*   **Root Cause Analysis:**
    1.  The immediate error is a `FileNotFoundError`, indicating the path in the `services_slim.yaml` config is incorrect or the file is missing.
    2.  The user confirmed with an `ls` command that all required raster files, including the ratio files, exist in a single directory: `.../raw/base_years_ha/`.
    3.  A review of `analysis_configs/services_slim.yaml` revealed that the paths were missing the `/raw/` subdirectory (e.g., pointing to `${GLOBAL_NCP_DATA}/base_years_ha/...` instead of `${GLOBAL_NCP_DATA}/raw/base_years_ha/...`). This path mismatch is the direct cause of the `FileNotFoundError`.
*   **Resolution:**
    1.  **Primary Fix:** All raster paths in `analysis_configs/services_slim.yaml` have been updated to include the correct `/raw/` subdirectory, ensuring they point to the actual file locations.
    2.  **Robustness Patch (Retained):** A patch was previously applied to `summary_pipeline_landgrid.py` to make it more robust. It now checks if a task returns `None` (indicating failure) and skips it, preventing the main script from crashing with an `AttributeError`. This remains a useful improvement.

---

### 2026-05-27 (cont. 2)
*   **Definitive Root Cause & Solution:** The user discovered that the Python `GEOSException` could be bypassed by using `geopandas.read_file(..., on_invalid="ignore")`. This confirms the root cause is a small number of features with invalid geometries being written by R's `sf` package that `geopandas` cannot read by default.
*   **Pipeline Hardening:** Instead of creating more intermediate "cleaned" files, the core Python script (`summary_pipeline_landgrid.py`) has been updated to use the `on_invalid="ignore"` flag. This makes the pipeline itself resilient to these minor upstream errors, providing a much more robust and direct solution. The separate `patch_add_biomes.R` script is no longer necessary, as the main `prepare_data.qmd` can be run in its complete form, and the Python script will now correctly ignore any problematic geometries it produces.

---

### 2026-05-27 (cont.)
*   **Pipeline Unblocking Strategy:** The `prepare_data.qmd` script continues to fail on complex geometry operations. To unblock the pipeline without losing more time, we've adopted a two-stage approach:
    1.  **Generate Base Grid:** Run a simplified version of `prepare_data.qmd` that intentionally excludes the problematic biome attribute join. This is expected to succeed and produce a clean grid with country/regional attributes.
    2.  **Patch Biomes:** Created a new, standalone script (`analysis/patch_add_biomes.R`) that takes the output from step 1 and performs only the biome join. This script uses a robust `st_join` followed by a `distinct(ID)` call to handle any duplicates created if a grid cell touches multiple biomes.
*   This strategy allows us to get a complete, analysis-ready grid file (`AOOGrid_10x10km_land_4326_clean.gpkg`) so the downstream Python pipeline can finally proceed.

---

### 2026-05-27
*   **Data Pipeline Failure & Strategic Rollback:**
    *   **Breaking Change Identified:** After multiple failed attempts to fix the `IllegalArgumentException: Invalid number of points in LinearRing` error, a comparison with the last known working version of `prepare_data.qmd` was performed.
    *   **Root Cause:** The error was introduced when the data preparation logic was changed to accommodate biome attributes. The original, working script used a direct `st_join` on polygons. The new, failing script introduced a call to `st_point_on_surface()` before joining, which is much less tolerant of minor geometric invalidities created during the `st_transform()` reprojection step.
    *   **Resolution:** The `prep-grid-aoo-land` chunk in `prepare_data.qmd` has been reverted to the simpler, more robust logic from the last working version. This removes the dependency on `st_point_on_surface` and the complex, multi-source attribute join, which was the source of the instability. The pipeline should now be able to generate the base grid successfully, as it did before these changes. The addition of biome data will be re-evaluated in a separate, more robust manner after the core pipeline is restored.

---

### 2026-05-26
*   **Data Pipeline Crisis & Strategic Pivot:**
    *   **Root Cause Re-confirmed:** The Python pipeline (`summary_pipeline_landgrid.py`) consistently fails with a `GEOSException: Invalid number of points in LinearRing` when reading the master grid GeoPackage (`AOOGrid_10x10km_land_4326_clean.gpkg`). This indicates a subtle geometry validity issue created by the R `sf` package that Python's `geopandas/shapely` cannot tolerate.
    *   **Failed Repair Attempts:** A standalone patch script (`patch_fix_grid_geom.R`) using `st_buffer(dist = 0)` was created to aggressively repair the geometries. However, this process proved to be unacceptably slow, running for over 4.5 hours without completion, making it an unviable solution.
    *   **New Strategy: Hybrid Raster-Vector Workflow:** A new, more robust strategy has been adopted to permanently solve this issue.
        1.  **Create Zone Raster:** A new script (`create_zone_raster.R`) was created to perform a fast, one-time conversion of the vector grid into a "zone raster" where each pixel's value is its corresponding `fid`.
        2.  **Raster-Based Zonal Stats:** A new Python script (`summary_pipeline_rasterzones.py`) will perform the zonal statistics using the new zone raster and the service rasters. This completely bypasses the need for Python to read the problematic vector file, eliminating the geometry errors.
        3.  **Attribute Join in R:** The main R script (`process_data.qmd`) will be updated to read the simple CSV output from the new Python script and join it back to the canonical vector grid, which contains all the rich attribute data (country, biome, etc.).
    *   This new hybrid approach is faster, more robust, and preserves the methodological integrity of the analysis by separating the geometry-heavy processing from the statistical calculation.

---

### 2026-05-22
*   **Major Data Pipeline Overhaul & Rerun:**
    *   **Root Cause Identified:** The critical `orig_fid not found` error in `process_data.qmd` was traced back to a stale base grid file (`AOOGrid_10x10km_land_4326_clean.gpkg`). This old grid was missing key attributes (like country names) and contained geometric artifacts (dateline wraparound), which were causing cascading failures in the Python pipeline.
    *   **Robust Solution Implemented:**
        1.  The `prepare_data.qmd` script was updated with a new, authoritative chunk (`prep-grid-aoo-land`) to generate a clean master grid from the original sources. This new grid correctly joins all attributes and fixes the dateline artifact.
        2.  The `process_data.qmd` script was simplified by removing the redundant and error-prone "Robust Attribute Assembly" logic, as the pipeline can now trust its clean input.
    *   **Full Data Regeneration Initiated:** A full rerun of the data pipeline has been started on the `lilling` server. This involves:
        1.  Running `prepare_data.qmd` to create the new master grid (a long process, ~1.5 hours).
        2.  Running the upstream Python pipeline (`summary_pipeline_landgrid.py`) for both services and beneficiaries using the new clean grid.
        3.  Running `process_data.qmd` to generate the final analysis-ready datasets.
    *   **YAML Fix:** Corrected a `YAMLException` in `chapters/01-problem.qmd` caused by improper indentation in the YAML header.
*   **Next Steps:** While the data regeneration runs, the focus will shift to refining the narrative, language, and presentation of the final Quarto book. A transfer prompt has been created to start a new chat session for this purpose.

---

### 2026-05-21
*   **Dashboard Layout Debugging Saga:** Spent significant time debugging the layout of `analysis/eda_dashboard.qmd`.
    *   **Initial Problem:** Plots were rendering too small to be readable in the dashboard format.
    *   **Attempt 1:** Switched the document format from `html` (page) back to `dashboard` and enabled `scrollable: true` to allow tall plots to render at their full height.
    *   **Attempt 2:** Implemented a side-by-side `columns` layout for the main plot sections to improve readability and use of space.
    *   **Core Issue Identified:** An unclosed `div` block (caused by a missing `:::` to close a `columns` section) was making all subsequent dashboard tabs appear empty.
    *   **Resolution & Final Layout:** Correctly structured the `columns` blocks for all sections, which fixed the empty tabs. After experimentation with side-by-side layouts (e.g., `width="50%"`), the decision was made to lock in a vertically stacked layout (`width="100%"` for all columns) within each major section. This provides a consistent, readable, top-to-bottom flow for all plots and tables in the dashboard. The layout is now considered stable.

---

### 2026-05-17 (cont.)
*   **Rasterization Workflow Template:** Created `scripts/gdal_rasterization_template.sh` to formalize and document the robust `gdal_rasterize`-based workflow. This template includes steps for GeoPackage reprojection and rasterization of both continuous and binary columns, ensuring easy reusability and preventing loss of this critical methodological knowledge.

---

### 2026-05-17
*   **Output Naming Convention:** Standardized raster output filenames to include the change metric (`_abs` or `_pct`) for clarity and consistency. For example, `hotspot_count.tif` is now `hotspot_count_abs.tif`. This ensures that all raster files can be distinguished by their filename alone.

---

### 2026-05-16
*   **Methodological Reflection:** Acknowledged that the extensive time spent debugging Python-based rasterization was inefficient. The direct use of `gdal_rasterize` from the command line proved to be a faster, more powerful, and more reliable solution from the beginning. Future rasterization tasks should default to using the core GDAL command-line tools to avoid similar issues with high-level library wrappers.

---

### 2026-05-15
*   **Rasterization & Grid Validation Saga:**
    *   Spent significant time debugging a persistent and subtle rasterization issue. Initial attempts to rasterize hotspot counts using the `vector_to_raster.py` script resulted in "ghost rasters" (tiny file sizes, empty when loaded in QGIS/R) and severe spatial misalignment artifacts (a single vector grid cell producing up to four raster pixels).
    *   After exhausting multiple fixes within the Python `rasterio` library (grid snapping, nodata value changes, removing compression/tiling), the root cause was identified as a deep incompatibility within the library stack in the server's Python environment.
    *   **Definitive Solution:** Abandoned the Python script in favor of the core `gdal_rasterize` command-line tool. This immediately produced a correctly aligned raster with a 1-to-1 mapping between vector cells and raster pixels. This will be the standard procedure for all future rasterizations.
    *   **Grid Geometry Verification:** A subsequent check of the reprojected vector grid's geometry (`hotspots_global_abs_epsg8857.gpkg`) initially caused confusion, as the bounding box of individual cells was not 10km x 10km.
    *   **Final Validation:** Developed a new verification script (`verify_grid_area.py`) to measure the true geometric **area** of the reprojected polygons, not just their bounding box. This definitively confirmed that each grid cell has an area of **100 km²**, validating the integrity of our equal-area grid and resolving a long-standing point of uncertainty. The project's core spatial foundation is now fully verified.

---

### 2026-05-12
*   **Finalize `hotspot_synthesis.qmd` & Prepare for Interpretation:**
    *   Completed a major debugging and refinement pass on `analysis/hotspot_synthesis.qmd` to ensure it runs locally and produces clean, final outputs.
    *   Resolved numerous rendering errors, including TeX installation failures (by switching to HTML output), missing `kable()` function errors (by adding `library(knitr)`), and data type mismatches in summary tables.
    *   Significantly improved the population exposure plots by:
        *   Correctly handling and filtering income group categories to remove "NA" values from plots.
        *   Enforcing a canonical service order for facets.
        *   Switching to a fixed y-axis scale for better comparability across services.
    *   Enhanced the report's clarity by replacing the raw configuration code chunk with a clean, formatted summary table.
    *   The `hotspot_synthesis.qmd` notebook is now stable and produces all necessary summary tables and visualizations, paving the way for the final interpretation phase.
    *   Prepared a transfer prompt and a git commit message to checkpoint this progress before moving to a new chat session focused on `analysis/results_interpretation.qmd`.

---

## Project Overview & Goals

**Goal:** Quantify global change in multiple ecosystem services (ES) at ~10-km resolution over 1992–2020, identify **hotspots** of concerning change, and attribute those changes to environmental and socioeconomic drivers.

**Key Objectives:**
1.  Robust spatial extraction of ES changes bypassing Modifiable Areal Unit Problem (MAUP) artifacts.
2.  Identification of ES hotspots using Symmetric Percentage Change (to handle zero-baselines and capture local vulnerability).
3.  Integration of Land Cover Change (LCC) metrics to attribute ES decline to Land Conversion vs. Degradation.
4.  Socioeconomic characterization of hotspots via Kolmogorov-Smirnov (KS) tests.

## Current State: Version 1.3.2 (Visual Unification & Presentation Polish)

**Status:** The core pipeline architecture is mathematically validated, cleaned, and finalized. We have successfully unified the visual styling using canonical WWF colors across all flowcharts and plots, preparing everything for the final presentation.

**Active Focus & The Final Wrap-Up Plan:**
*   **Geographic Clustering:** Finalizing the geographic narrative of "Compound Risk" (Hotness) and "Disproportionate Burden" (Relative Intensity) using the consolidated `hotspot_synthesis.qmd` pipeline.
*   **Land Cover Change (LCC) Interpretation:** Reviewing the outputs of the "Drivers of Change" chunks in `hotspot_extraction.qmd` to define our "Attribution Gap" (conversion vs. degradation).
*   **Socioeconomic Interpretation (KS Tests):** Interpreting KS Test heatmaps and Cliff's Delta plots to profile the socioeconomic context of extreme ES decline.
*   **Presentation & Handoff:** Sharing final exported plots and datasets with co-authors via OneDrive, and drafting the final Key Takeaways and methodology sections.

## Key Challenges & Architectural Solutions (For Final Report)

This section highlights the major technical and methodological hurdles overcome during the pipeline's development, serving as a direct outline for the Methods paper.

*   **The Fragment Bug & Spatial Alignment:** *Challenge:* Bypassing C++ GEOS bottlenecks by exploding complex multipolygons into 1.67M fragments caused striping and duplicated data. *Solution:* Reverted to a mathematically safe `st_intersects` spatial joining and re-aggregation process (`group_by %>% summarise`), collapsing fragments back into pristine 10km parent cells to perfectly align data (v1.3.1).
*   **Spatial Extraction Scaling:** *Challenge:* `exactextract` memory leaks and C++ segmentation faults when processing massive, jagged regional multipolygons (e.g., Biomes). *Solution:* Adopted a "Hybrid Extraction" architecture—using `exactextract` for simple grids (10km) and rasterized `zonal_stats_toolkit` for complex regional polygons.
*   **Simpson's Paradox & MAUP:** *Challenge:* Observing "sign flips" where a region showed negative Absolute Change but positive Percentage Change. *Solution:* Documented the distinct spatial narratives. Mean Absolute Change captures systemic volume shifts (weighted by huge baselines), while Mean Symmetric Percentage Change captures widespread landscape footprint shifts.
*   **Zero-Baselines & Scale Bias:** *Challenge:* Absolute change is heavily biased by the size of the baseline ecosystem, and standard percentage change fails on zero-baselines. *Solution:* Transitioned to **Symmetric Percentage Change (SPC)** to normalize the data, capturing the true *intensity* of ecological response for Land Cover Change attribution.
*   **Aggregation Statistic — Mean for All ES Services (2026-07-02):** *Challenge:* The extraction pipeline produces both `_sum` and `_mean` columns for every ES service raster. Managing mixed `_sum`/`_mean` references across `process_data.qmd`, `calculate_bitemporal_change.py`, and downstream R scripts required constant manual attention and was a recurring source of bugs and confusion. *Decision:* Standardised on **`_mean` for all ES service variables** at the 10km grid-cell level. *Rationale:* (1) All input rasters are pre-normalised to per-hectare units (sourced from `base_years_ha/`), so the mean within each equal-area cell gives a valid, comparable per-ha rate. (2) For hotspot detection, SPC(mean, mean) = SPC(sum, sum) for equal-area cells — the choice does not affect rankings or results. (3) Regional totals for volumetric services are computed separately via Path A, which bypasses the grid entirely. *Implementation:* `services_slim.yaml` now specifies `op_stats: [mean]` only; stale `_sum` references removed from `calculate_bitemporal_change.py` and `process_data.qmd`. *Note:* Socioeconomic variables (GHS-POP, GDP) retain `_sum` — these are absolute counts/totals where summing is the correct interpretation and population exposure figures depend on this.

## Phase 4: Pre-Submission Polish & Repo Cleanup (2026-06-25 to 2026-07-02)

### Manuscript language & framing
*   **"Who bears the burden" → exposure framing:** Replaced "who bears the greatest burden" with "which populations and socioeconomic contexts face the greatest exposure" throughout paper, book preface, and conclusions. Distinction: the analysis measures *exposure co-occurrence*, not welfare impact or burden.
*   **Abstract shortened:** Reduced from ~320 to ~210 words. Removed SPC definition, expanded acronyms, and per-figure methodology from abstract. Kept 4 headline numbers (252K cells, 1.6×, 3.1B/7.6B, 76%). Research objectives updated to reflect WHY reframing ("is monitoring land cover alone sufficient?") and WHO framing.
*   **Paper conclusions stale numbers fixed:** 6.4B / 1.15× replaced with verified 3.1B in-situ → 7.6B connected (2.5×), consistent with Ch06 and presentation.
*   **"markedly" → "substantially/sharply":** Removed LLM-register word throughout paper and book preface.
*   **Sherman et al. (2026) DOI confirmed:** Verification notes removed from all files.
*   **MAUP citations added:** Openshaw (1984), Fotheringham & Wong (1991), Simpson (1951) added to paper references and Key References table in ch02.

### Book chapters updated
*   **Ch01 "Caveats & Transparency":** Expanded from 5 one-liners to 4 substantive paragraphs. Temporal snapshot now notes two-endpoint design, intermediate dynamics gap, 2020 aging, and future extension path. Attribution gap now precisely states "not co-occurring with extreme (top 5%) land cover conversion." "Natural variation" dropped as standalone (absorbed as named mechanism in attribution gap). "About the Analysis" methods section rewritten with correct 8-service list, dual-path description, and proper citations.
*   **Ch02 intro:** Tightened from one verbose paragraph to two short ones. InVEST now "models of eight ecosystem services"; "satellite land cover records" → "ESA CCI and C3S land cover classification maps at 300m"; "gridded socioeconomic layers" → named (population, GDP, HDI, Gini).
*   **Ch02 Pre-processing:** WGS84 / projection detail moved to ch09 Technical Appendix. Ch02 now states decisions plainly (3 short paragraphs). Coastal Risk column-name detail also moved to ch09.
*   **Ch02 Serviceshed section ADDED:** New `## Population Exposure and the Serviceshed Multiplier` section — was present in flowchart but had no dedicated methods text. Covers in-situ baseline, two routing pathways (hydrological + travel-time), multiplier definition, and stratification outputs.
*   **Ch02 Reproducibility:** Now shows both Path A (zonal_stats_toolkit / services_diff_ha_groupings.yaml) and Path B (exactextract / services_slim.yaml chain) execution steps. audit_claims.R included as step 6.
*   **Ch02 Summary:** Serviceshed component added as explicit fourth pillar (was missing despite "four-component framework" claim).

### Repo cleanup
*   **Python_scripts/:** archive/ created; deprecated scripts moved (summary_pipeline_rasterzones, summarize_cp_points_grid, temp_nature_access_diff, check_valid_geometries); dead files deleted (DEM_Mask, processed_data, import rasterio.py); README.md written with pipeline order and per-script descriptions.
*   **R/:** percentileR.R archived (superseded by get_hotspots.R); README.md rewritten with accurate file list and pct_change.R vs pct_change_calc.R dependency explanation.
*   **scripts/:** archive/ created; one-offs moved (gdal_rasterization.sh, plot_hex.R, enrich_grid_attributes.R — superseded by enrich_grid.py); duplicate make_attribution_map.R removed; README.md written with Validation / Reference Data / Output / Mapping / Archive sections.
*   **analysis_configs/:** archive/ created (zonal_stats_diff.yaml, global_ncp_base_ha.ini); README.md written documenting Path A / B / C config mapping; c_protection_synth.yaml path updated from interim/archive/ → interim/coastal_protection_rasters/.
*   **README.md (root):** Fixed critical errors — Quick Start was calling deprecated `summary_pipeline_rasterzones.py` with non-existent `services_raster.yaml`; results_interpretation.qmd references removed (notebook deleted); ES services list updated to correct 8-service table with variable names; audit_claims.R added to pipeline flow.

## Phase 4 continued: Paper/Book polish pass 2 (2026-07-03)

### Data claim verification
*   **Sub-Saharan Africa error caught and corrected (2026-07-03):** The preface and ch08 erroneously stated SSA carries a "disproportionate share" of hotspot burden. Verified from data: SSA relative intensity = 0.78× (BELOW expected given land area). Only LAC (1.40×) and EAP (1.30×) exceed 1.0×. Corrected in index.qmd and ch08. Root cause: AI-generated plausible-but-unverified claim. Prompted systematic review.
*   **5×/8× claims corrected (2026-07-03):** Presentation stated "5× higher population density than median background" and "8× for 3+ service cells." These are actually **serviceshed multipliers** (connected beneficiaries ÷ in-situ for 2+/3+ compound cells) from ch06 data, not density ratios. Fixed in presentation slide and speaker notes.
*   **SPC examples corrected (2026-07-03):** ch02 examples stated 100→0 = -100% and 0→100 = +100%. Both wrong. Correct: **100→0 = -200%** (theoretical minimum) and **0→100 = +200%** (theoretical maximum). Fixed with explanation of ±200% bounds as a feature of SPC.
*   **audit_claims.R expanded (2026-07-03):** Now 8 checks (was 5). Fixed critical bug: income ratio check was comparing "Low income" vs "High income" but the 1.6× claim is "Lower-middle income" vs "High income OECD." Added checks for hotspot count, population exposure, 5×/8× disambiguation, KS test count.

### Paper polish
*   **Abstract rewritten** to ~210 words; closing sentence reframed from limitation ("cannot causally partition") to contribution ("highlights limits of LC monitoring as sole proxy"); nature accounting angle added.
*   **Research objectives** updated with WHAT/WHERE/WHO/WHY labels, WHO reframing ("which socioeconomic contexts"), WHY reframing ("is monitoring LC alone sufficient").
*   **Author list**: Stephen Polasky added; "[Additional Authors TBD]" placeholder retained.
*   **KS methods paragraph** cleaned of AI slop: "utilizing" → "using"; "artificially inflates" → precise language; "matched counterfactual background" → "background of typical stable conditions"; Andam et al. citation removed (was misapplied — Andam is about PA effectiveness, not KS background design); "Type I error propagation across multiple simultaneous testing dimensions" → "Type I error across 40 simultaneous tests (8 services × 5 covariates)."
*   **39/40 result** added to paper KS section, ch02 callout, presentation notes, and methodology.md.
*   **FDR plain-language explanation** added to ch02 (callout), methodology.md, and presentation speaker notes.
*   **Spatial co-occurrence justification** added: explicit paragraph explaining that the comparison operates on ranked binary overlays, not raw values — making the comparison between continuous-derived (SPC) and categorical-derived (Pontius) hotspots methodologically defensible. Becky callout added for literature validation.
*   **Paper conclusions** stale numbers fixed: 6.4B / 1.15× → 3.1B / 7.6B / 2.5× (consistent with ch06).
*   **Data sources paragraph**: verbose pipeline detail about excluded cells removed from paper (now one clause).
*   **Paper structure callout** added for Becky (target journal question → drives restructure decision).
*   **diffeR → Pontius contingency matrix approach**: section heading and framing corrected in ch02 and ch05. diffeR correctly positioned as implementation tool, not the methodology itself.

### Book polish
*   **ch01 How to Use**: corrected chapter numbers (ch4 WHERE was missing from all paths; ch7 was wrong labelled as "WHY"); four reading paths now accurate.
*   **ch01 Users section**: Nature accounting practitioners added as a distinct user type (GEP, SEEA, TNFD).
*   **ch01 About the Analysis**: authors updated (Polasky added).
*   **ch02 intro**: "InVEST biophysical models" → "InVEST models of eight ecosystem services"; "satellite land cover records" → "ESA CCI and C3S land cover classification maps at 300m"; "gridded socioeconomic layers" → named covariates.
*   **ch03**: Stacked three-section layout → tabset (World Bank Region / Income Group / WWF Biome).
*   **ch08 WHO section**: "Hundreds of millions" → verified 3.1B / 7.6B figures.
*   **ch08**: GEP / nature accounting section added ("For Nature Accounting Frameworks") covering GEP, SEEA EA, TNFD.
*   **_quarto.yml**: Polasky added to author line.
*   **index.qmd**: Sub-Saharan Africa claim corrected; universal signal bullet rewritten in plain language; "who bears burden" → "which populations and socioeconomic contexts"; attribution gap typo fixed; nature accounting user type added.

### Documentation
*   **methodology.md KS section** rewritten: plain-language explanations of median background rationale, Cliff's Delta vs p-values, FDR correction with 39/40 result and ecological interpretation of the one non-significant combination.
*   **presentation.qmd**: 5×/8× serviceshed multiplier framing corrected; LAC per-service values flagged for verification; FDR plain explanation added to speaker notes.

## Reference Information
*   **Environment Notes:** Local machine: Lenovo (Windows 11) | Remote: lilling (VS Code Remote SSH) | AI assistant: Gemini Code Assist / Copilot
*   **Active Entry Points:** `analysis/process_data.qmd`, `analysis/hotspot_extraction.qmd`, `analysis/hotspot_synthesis.qmd`, `analysis/KS_tests_hotspots.qmd`
*   **Known Issues / Gotchas:** Hotspot rules (loss vs gain services) must remain centralized in `HOTS_CFG`. Be careful not to mix interpretive direction (good/bad change) with magnitude summaries.

---

## 🛠️ Merged Worklog: Zonal Stats Toolkit (Pre-Integration)

*This section consolidates the historical worklog from the `zonal_stats_toolkit` repository. Moving forward, all notes for both the Python extraction engine and the R/Quarto synthesis pipeline will be tracked in this single document.*

### Key Methodological Milestones (Toolkit)
*   **Spatial Dissolve vs Tabular Grouping:** Proved that geographic dissolves prior to extraction cause massive OOM errors and slowdowns. The optimized design uses a high-res grid and tabular aggregations post-extraction.
*   **Pollination Discrepancy:** Identified that $\text{Mean}_{2020} - \text{Mean}_{1992}$ diverges from $\text{Mean}_{\Delta}$ for Pollination due to NoData mask misalignments (shifting agricultural footprints).
*   **Legacy vs Optimized Validation:** Achieved 0.9975 Pearson Correlation between legacy GDAL rasterize and the optimized `exactextract` pipeline. Variance is strictly due to boundary-pixel handling (`ALL_TOUCHED` artifacts). Optimized pipeline safely calculates exact fractional overlap.
*   **Raster Conversion Overhaul:** Refactored `convert_to_ha.py` to use `rasterio` and `WarpedVRT` in small blocks (sequential `max_workers=1` with `BIGTIFF=YES`), definitively resolving memory and write failures on global rasters.

### Chronological Toolkit Notes (Jan - Mar 2026)
*   **Mar 24:** Visualization refactor for bitemporal difference plots. Switched to SEM for error bars and filtered bottom 10% micro-states to prevent variance skewing. Developed `append_ratios.py` for missing data.
*   **Mar 20:** Runner config enhancements (skip jobs).
*   **Mar 13 (Bi-Temporal Math):** Implemented `calculate_bitemporal_change.py` using `osgeo.ogr` directly on the GPKG. This calculates Absolute and Symmetric Percentage Change (SPC) via raw SQL updates, explicitly bypassing memory-intensive `geopandas` operations and `sqlite3` limitations to prevent OOM crashes on global grids.
*   **Mar 13 (Validation):** Built validation framework `compare_gpkg_columns.py` (NRMSE metrics). Enforced runner determinism.
*   **Jan 28-29:** Coastal protection vector attribute integration (`Rt`, `Rt_ratio`).
*   **Jan 20-22:** Disk space management, permission fixes, and visualization layout refinements.
*   **Jan 12:** Docker execution bypassing host permissions, fixing NaN handling, and output column filtering.

---

### 2026-05-08 (Urgent Task: Rasterization)
### 2026-05-08 (Major Conceptual & Analytical Refinements)
*   **Nuanced Driver Mapping:** Finalized the land cover change driver analysis by significantly refining the classification logic in `make_lcc_driver_map.R`. Replaced the generic "Multiple Overlapping Drivers" category with specific, policy-relevant transitions like "Deforestation for Cropland," "Savannization / Pasture," and "Grassland to Cropland."
*   **Grassland Dynamics:** Fully integrated both "Grassland Loss" and "Grassland Expansion" as distinct drivers into the mapping scripts, ensuring these critical rangeland dynamics are no longer masked.
*   **Refined Attribution Terminology:** Replaced the presumptive "Degradation-driven (Stable Land Cover)" category in `make_attribution_map.R` with the more accurate and defensible term **"Attribution Gap (Change without Conversion)"**. This new label correctly describes ES hotspots that are spatially decoupled from major land conversion.
*   **Clarified Metric Interpretation:** Added a detailed methodological note to `make_lcc_driver_map.R` to explicitly state that the land conversion metrics are calculated as a percentage of the *total 10km cell area*, clarifying the interpretation of "landscape transformation intensity".
*   **Expanded Socioeconomic Analysis:** Enhanced `hotspot_synthesis.qmd` to include absolute population exposure analysis by GDP and Gini quartiles. Removed the redundant 'Built Area' variable from the `KS_tests_hotspots.qmd` analysis.
*   **Visualization & Documentation Polish:** Increased map output resolution to 600 DPI for clarity, improved color palettes for distinguishability, and updated `Key_Takeaways.md` and other documentation to reflect all conceptual shi*   **Hotspot Count Rasterization:** Developed a new Python script `Python_scripts/convert_hotspot_gpkg_to_raster.py` to convert the vector-based hotspot count maps (from `hotspots_global_pct.gpkg`) into a GeoTIFF raster. This provides a raster-based output of hotspot frequency, as urgently requested.
*   **Hotspot Count Rasterization:** Developed a new Python script `Python_scripts/convert_hotspot_gpkg_to_raster.py` to convert the vector-based hotspot count maps (from `hotspots_global_pct.gpkg`) into a GeoTIFF raster. This provides a raster-based output of hotspot frequency, as urgently requested.

---

### 2026-05-11
*   **General-Purpose Rasterization Utility:** Refactored the specialized `convert_hotspot_gpkg_to_raster.py` script into a flexible, general-purpose command-line tool named `vector_to_raster.py`.
    *   The new script is no longer hardcoded to specific "hotspot" columns. It now accepts a list of columns to rasterize via a required `--columns` argument.
    *   Enhanced flexibility by adding command-line arguments to control the output `--resolution`, target `--crs`, `--nodata` value, and raster `--dtype` (e.g., `float32`, `int16`).
    *   This provides a robust and reusable utility for converting any attribute from a vector file into a GeoTIFF, addressing the need for a more reliable rasterization method than manual QGIS operations for our various grid-level summary files.

*   **Pipeline Cleanup & Housekeeping:**
    *   Disabled the creation of large intermediate synthesis files (`10k_grid_synth_all.gpkg`, `10k_grid_ES_change_benef.gpkg`) in `process_data.qmd` by default. These files were useful for debugging but are not required for the final analysis and can be recreated if needed. This change will keep the `processed/` directory cleaner.
    *   Documented the legacy status of `prepare_data.qmd` with a note explaining it is a one-time setup script for the base grid and not part of the routine analytical workflow.
    *   Confirmed that existing intermediate files can be safely deleted to free up disk space.

*   **Documentation Consolidation:** Overhauled the project's documentation to eliminate redundant `README` files and establish a clear, maintainable structure.
    *   Consolidated all high-level information into a single, comprehensive root `README.md`.
    *   Created a central `docs/` directory to house detailed, long-form documentation.
    *   Moved content from various `README_*.md` files into `docs/methodology.md`, `docs/data_dictionary.md`, and `docs/runbook.md`.
    *   Explicitly documented the key methodological distinction between the project's "difference of aggregates" approach (Path B) and the alternative "aggregate of differences" (Path A) in `docs/methodology.md`.

---

## Chronological Log (Newest to Oldest)

### 2026-05-05
*   **Visualization Consistency:** Updated `hotspot_synthesis.qmd` to ensure all "hotness" and "exposure" bar charts use a consistent red intensity color scale (`scale_fill_distiller`) mapped to the value, rather than categorical colors for the groups. This improves visual coherence across the analysis.
*   **Code Health:** Added the `group_palettes` object definition to `hotspot_synthesis.qmd` to resolve a missing object error that was causing rendering to fail.
*   **Visual Unification & Cleanup:** Systematically removed redundant "main report" plotting blocks and ensured the "High income: nonOECD" category is consistently and globally filtered out from all visualizations in `hotspot_extraction.qmd` and `hotspot_synthesis.qmd` to reduce noise.
*   **LCC Grasslands Integration:** Integrated "Model 3: Grassland Loss" into the `LC_change_granular.qmd` pipeline, adding a specific reclassification matrix to explicitly track the conversion of grasslands to other uses.
*   **Narrative Refinement:** Updated `Key_Takeaways.md` to incorporate the "Spatial Attribution / Degradation" findings and highlight the new Grassland Loss model, aligning with the latest feedback.
*   **Plotting Iteration (Synthesis & Volumetric Plots):** Reverted the combined volumetric plots in `hotspot_extraction.qmd` back to separate figures for absolute and percent change. Fixed the y-axis labels in the `hotspot_synthesis.qmd` bar charts to display the numeric key instead of being blank, improving readability.

### 2026-05-06
*   **Boxplot Unification & Refinement:** Refactored the entire boxplot generation logic in `hotspot_extraction.qmd` into a single, unified function. This ensures all boxplots (volumetric, ratio, coastal) have a consistent aspect ratio, a universal numeric legend with a key at the bottom, and larger, more readable fonts. This resolves previous inconsistencies and simplifies future maintenance.
*   **Data Dictionary Updates:** Improved the data dictionaries in `KS_tests_hotspots.qmd` and `hotspot_synthesis.qmd` to provide clearer, more accessible definitions for key statistical terms and output table columns, enhancing the project's usability for collaborators.
*   **Granular LCC Integration:** Verified and finalized the integration of the "Grassland Loss" model into `LC_change_granular.qmd`, ensuring its results are correctly consolidated into the final output GeoPackage.

### 2026-05-04
*   **Infrastructure & Environment:** Resolved persistent VS Code Remote SSH synchronization and connection hangs that have been occurring since last week on `lilling`.
    *   *Diagnosis:* The VS Code server backend was fragmenting and leaving behind orphaned `node` processes for language servers (Pylance, Quarto) and the core RPC server, which blocked new connections.
    *   *Troubleshooting:* Implemented a targeted process-kill command (`pkill -u jeronimo -f .vscode-server`) via terminal to forcefully clean up the hung background processes. This successfully resets the remote connection state without requiring physical or system-level reboots of the server by IT.
*   **Plotting Refinement:** Updated `compare_and_plot_changes.R` to exclude the "High income: nonOECD" group from the main report's bar plots to remove outliers and clarify the primary trends, as discussed in the last review meeting.
*   **Housekeeping:** Identified and removed a redundant, outdated copy of `hotspot_extraction.qmd` that was incorrectly located in the `R/` directory. Confirmed `analysis/hotspot_extraction.qmd` is the correct, canonical version.

### 2026-05-02
*   **Infrastructure & Sync:** Diagnosed and bypassed silent VS Code Remote SSH hangs on `lilling` without a hard reboot (safely wiped corrupted `~/.vscode-server`). Established a `tar`-over-SSH sync workaround to bypass strict Windows IT firewalls lacking `rsync`.
*   **Python Engine Optimization:** Refactored `zonal_stats_toolkit/runner.py` to concurrently schedule both raster and vector tasks in the execution graph, significantly improving parallelism ahead of the v1.4.0 merger.
*   **Visual Polish (Boxplot Color Ramps):** Solved the `ggplot2` global scale dominance issue in the plotting scripts (`hotspot_extraction.qmd`). Implemented localized data normalization (`scales::rescale`) so canonical intensity colors (Reds) dynamically scale from 0 to 1 strictly within their respective facets.
*   **Methodology Documentation:** Updated `README_Methodology.md` to formally transition "Path C" from a hypothetical "Future Analysis" into a completed "Validation Analysis," explicitly confirming that the grid-level hotspots mathematically align with pixel-level differences.
*   **Feedback Manifesto Audit:** Cross-referenced meeting notes to finalize terminology ("Multi-service Decline" over "collapse"), prepared the biome-faceted scatterplots for the "Attribution Gap", and confirmed non-OECD outlier exclusions for main report boxplots.
*   **Next Steps Planned:** Ready to implement "Model 3: Grassland Loss" in `LC_change_granular.qmd` to accurately track Forest-to-Grassland and pristine Grassland-to-Cropland transitions.

### 2026-05-01
*   **Post-Meeting Debrief & Cleanup:** Successfully presented the "Drivers of Change" (LCC Attribution) and "Who is Affected" (Socioeconomic / KS Tests) sections to Steve and Becky. The compound risk mapping, red-intensity boxplots, and LCC driver correlations resonated strongly. 
*   **De-escalating "Rescue Mode":** Safely stripped out local hardcoded `here("home", "jeronimo", ...)` fallback paths from all R mapping scripts (`make_socieconomic_maps.R`, `make_attribution_map.R`, `make_lcc_overview_map.R`, `make_hotspot_count_map.R`). Returned the pipeline to universally use `data_dir()` for server-side processing on `lilling`.
*   **Server Stability & Repackaging:** Remotely rendered `hotspot_extraction.qmd` on `lilling` to establish the final, single source of truth. Repackaged the `global_ncp_data_archive.tar.gz` archive with the updated Data Dictionary, preparing the data outputs for distribution without plot files.
*   **Path to v1.4.0 (The Merger):** With Pillar 4 and Pillar 5 validated, the repository is officially ready for the massive architectural merge. The upcoming `v1.4.0` will natively integrate the Python `zonal_stats_toolkit` directly into the `global_NCP` repo, creating a single, unified pipeline repository.

### 2026-04-30
*   **Visual Unification (4+ Hotspot Cap):** Standardized the compound risk narrative by capping overlapping hotspot counts at "4+" across both spatial maps and regional stacked barplots. Applied a unified semantic color ramp (Yellow to Dark Red) across `make_hotspot_count_map.R` and `hotspot_synthesis.qmd` to ensure immediate visual recognition of extreme compound risk.
*   **"First Look" Overview Maps:** Created minimalist, high-resolution global overview maps (solid red, no heatmaps) for both absolute and percentage hotspots to serve as clean anchor visuals for the presentation slide deck.
*   **Server Rendering & Single Source of Truth:** Pushed all visualization updates to the remote repository and successfully re-rendered the canonical `hotspot_synthesis.qmd` pipeline on the Lilling server, ensuring all plots and CSVs remain perfectly in sync.
*   **Next Immediate Step:** Diving into the "Attribution Gap" (Pillar 4) by analyzing Land Cover Conversion (LCC) overlaps using `lcc_es_hotspot_overlap_pct.csv` to build out the narrative for Coastal Risk (driven by Urban Expansion) and Pollination (driven by Forest Loss/Cropland Expansion).

### 2026-04-28
*   **Spatial Alignment Crisis Averted**: Diagnosed and eliminated a critical `seq_len()` reassignment bug in variable-length datasets across `process_data.qmd`, `hotspot_extraction.qmd`, and `hotspot_synthesis.qmd` that was scrambling downstream spatial joins and creating "striped" artifacts in maps. Enforced strict `stop()` fallbacks to prevent silent spatial corruption.
*   **Emergency "Rescue Mode" Implementation**: Successfully extracted and utilized a 2.2GB data archive (`global_ncp_data_archive.tar.gz`) to bypass long-running spatial joins under a strict deadline, temporarily routing scripts to safely read local `plt_long.rds` and GPKGs.
*   **Visualization Overhaul (Barplots & Intensity)**: Replaced confusing categorical color ramps in signed change bars and intensity plots with strict, semantic "Good (Green) / Bad (Red)" logic. Implemented an automatic alphanumeric `[ID]` key system on the y-axis to perfectly map subregions to legends.
*   **Dual-Metric Driver Analysis**: Upgraded `make_attribution_map.R` and the `hotspot_extraction.qmd` land-cover driver overlap chunks to loop over both Absolute (`abs_chg`) and Percentage (`pct_chg`) metrics. Programmatically recreated the massive `global_attribution_gap_map.png` directly in R to eliminate QGIS bottlenecks.
*   **Upcoming Priorities (Next 48 Hours)**:
    *   **Drivers**: Review and compare the newly generated `abs_chg` vs `pct_chg` scatterplots and heatmaps to finalize the "Attribution Gap" narrative.
    *   **Equity**: Review the "Absolute Population Exposure" (affected people) outputs generated by `hotspot_synthesis.qmd`.
    *   **Socioeconomics**: Perform a final validation pass on the KS analysis results.
    *   **Presentation**: Finalize the "Why" (drivers) and "Who" (people) sections of the presentation slide deck.
*   **Version 1.3.2 Release**: Unified pipeline visual styling (Mermaid flowcharts, spatial maps, and plots) to strictly use canonical WWF colors. Cleaned up redundant documentation and finalized the narrative methodology structure for the presentation slide deck.

### 2026-04-27
*   **Hotspot Boxplot Pipeline Overhaul**: Resolved critical "silent failures" in `hotspot_extraction.qmd` where Quarto intercepted error messages and skipped plot generation due to missing `plt_long` attributes. Implemented a robust on-the-fly attribute join from the master grid (`AOOGrid_10x10km_land_4326_clean.gpkg`), fixed `dplyr` dynamic scoping issues (`across(all_of())`), and added aggressive `stderr()` diagnostic logging.
*   **Coastal Visualization Fix**: Refactored coastal service boxplots to use pre-calculated 1.5*IQR whiskers (`stat="identity"`) instead of `outlier.shape=NA`. This permanently solves the issue of invisible outliers stretching the y-axis and causing scattered point artifacts.
*   **PDF Image Resolution**: Fixed LaTeX pathing during document rendering to ensure the freshly rendered, canonical-colored plots are correctly embedded into the final PDF.
*   **KS Test Enhancements**: Deprecated legacy `cfg$paths` in favor of `data_dir()` across the pipeline, and successfully integrated **Built Area** (`GHS_BUILT_S_E2020_mean`) into the socioeconomic covariate analysis.
*   **Methodology Flowchart (`workflow.qmd`)**: Developed a presentation-ready, high-resolution Mermaid.js flowchart documenting the end-to-end analytical pipeline. Mapped the dual-path extraction architecture (Regional Zonal Summaries vs. 10km Grid Analysis) and perfectly aligned the final deliverables with the slide deck's narrative structure (WHAT, WHERE, WHY, WHO). Bypassed strict parsing bugs in Mermaid v11.6.0 to implement custom WWF color palettes, transparent overlays, and thick routing arrows.

### 2026-04-23
*   **Workspace Integration**: Configured a VS Code Multi-root Workspace bringing `global_NCP` and `zonal_stats_toolkit` side-by-side for unified development.
*   **Documentation Unification**: Merged the historical worklog from the `zonal_stats_toolkit` repository into the central `WORKLOG.md` to officially centralize project tracking.
*   **Aesthetic Unification & Fixes**: Applied universal canonical color palettes for Biomes, WB Regions, and Income Groups across the `global_NCP` and `zonal_stats_toolkit` plotting scripts. Fixed exact string matching issues for Income Groups with numeric prefixes.
*   **Contextual Mapping**: Developed `generate_context_groupings_map()` to produce a 4-facet overview map of all geographic groupings, providing a clean visual baseline for the slide deck introduction.
*   **Equity Analysis (Impact Tier)**: Audited codebase for population metrics and implemented the `Absolute Population Exposure` module in `hotspot_synthesis.qmd`. This calculates the total number of people living in top 5% ES hotspots, segmenting the vulnerable populations by HDI bin and Income Group.
*   **Narrative Consistency**: Enforced standard terminology: "Relative Socioeconomic Shift" for KS statistical testing and "Absolute Population Exposure" for raw population counts.

### 2026-04-13
*   **Data Alignment Bugfix:** Resolved a fatal desynchronization bug in `hotspot_synthesis.qmd` where missing `fid` identifiers in the master attribute grid caused silent Quarto crashes during attribute joins.
*   **Technical Debt Documentation:** Formally documented the "Fragment Bug" spatial join bypass as technical debt across `process_data.qmd`, `README_Methodology.md`, and `README_pipeline.md`. Outlined the V1.4.0 plan to replace it with a robust `orig_fid` tabular join.
*   **Data Packaging:** Created a lean 2.2GB final data archive (`global_ncp_data_archive.tar.gz`) for co-author handoff. It strictly includes the analysis-ready `processed/` datasets, `outputs/` plots, `vector_basedata/` grids, and a standalone `README`. Excluded all raw/intermediate raster data to ensure easy sharing.
*   **Presentation Strategy:** Outlined the final slide deck structure for co-authors, focusing heavily on Compound Risk (Hotness), Disproportionate Burden (Relative Intensity), and the "Attribution Gap" (Land Conversion vs. Degradation).
*   **Housekeeping:** Cleaned up residual Git artifacts and removed deprecated scratch scripts.

### 2026-04-10
*   **Final Synthesis & Key Takeaways:** Successfully consolidated Intensity, Share, Relative Intensity, and Multi-service "Hotness" (Compound Risk) into a single, bulletproof pipeline (`hotspot_synthesis.qmd`).
*   **Codebase Grooming:** Officially deprecated `hotspot_intensity.qmd` and `hotspot_multiservice.qmd`, removed dead code in Python utilities, and prepared the repository for co-author handoff via secure, read-only OneDrive sharing.
*   **Visualization Polish:** Re-engineered compound risk and relative intensity bar charts to automatically loop over all canonical groupings, generating presentation-ready outputs for the final report.

### 2026-04-08
*   **Pipeline Fixes (The Fragment Bug):** Discovered that Python's `gdf.explode()` was fragmenting the 1.5M grid cells into 1.67M jagged pieces to bypass GEOS bottlenecks. This caused severe striping (dropped cells) and impossible hotspot counts (up to 180) due to duplicated data.
*   **Spatial Join & Re-aggregation Patch:** Implemented a robust `st_intersects` spatial join and re-aggregation (`group_by %>% summarise`) in `process_data.qmd`. This mathematically collapses all fragments back into their pristine 10km parent cells, ensuring perfect 1:1 data alignment. Striping is completely eliminated, and max hotspot counts are strictly capped at 8 (the total number of services). Stable extraction pipeline version tagged as `v1.3.1`.
*   **V2 Technical Debt Documentation:** Formalized the "V2 Simplification Plan" to use `orig_fid` (preserved from Python prior to explosion) to bypass spatial joins entirely in future analysis updates (`v1.4.0`).

### 2026-03-24
*   **LCC Driver Correlation Improvements:** Upgraded Land Cover Change (LCC) vs Ecosystem Service scatterplots to use 2D density heatmaps (`geom_bin2d`) with a logarithmic viridis scale to solve massive overplotting. Restructured plots to a faceted 3x3 canonical layout and removed deprecated `USLE`/`N_retention` metrics.
*   **Methodological Documentation (Absolute vs. Percentage Change):** Documented the critical decision to use Symmetric Percentage Change (SPC) rather than Absolute Change for attributing ES declines to drivers. Absolute change is heavily biased by the baseline ecosystem size (e.g., a 5% loss in a massive forest yields a larger absolute drop than a 100% loss in a tiny forest). SPC properly normalizes the data to reveal the *intensity* of the ecological response relative to the local baseline.
*   **KS Socioeconomic Analysis Validation:** Verified the `KS_tests_hotspots.qmd` pipeline. Confirmed the successful execution of balanced sampling (`comparison_mode = "median"`) to correct the 5% vs 95% class imbalance. Updated KS Heatmaps and Cliff's Delta plots to enforce the canonical 3x3 service ordering for presentation consistency.
*   **Hotspot Visual Polish:** Unified boxplot aesthetics in `hotspot_extraction.qmd` to remove arbitrary color maps, using a consistent clean `gray95` fill. Added horizontal, ranked (Top 10/Bottom 10) boxplots for country-level aggregations to drastically improve legibility.
*   **Automated Faceted Mapping:** Developed `make_faceted_maps.R` to fully automate the generation of spatial maps across 4 groupings (World Bank Region, Income Group, Biome, Country). Implemented a dynamic Cartography Rule Engine for automatic color ramp selection (diverging/sequential, goods/damages) and utilized `patchwork` for complex multi-scale layout stitching. Applied Equal Earth projection (`EPSG:8857`) and 1st/99th percentile outlier trimming to ensure high-quality visualization of absolute change.
*   **Documentation Refinement (Conceptual Framing):** Harmonized `README_Methodology.md` with explicit definitions of a "hotspot" (framing it as a *relative extreme* ranking label rather than an absolute threshold or evidence of cause). This analogy (the "marathon finisher") will directly support the framing of the final methods paper.

### 2026-03-20 (Pre-v1.3.3)
*   **Architectural Validation (Spatial Extraction Strategies):** Ran a test using `exactextract` in Python for large regional groupings (Biomes/WB Regions) by exploding them into 85,000 fragments. It ran for over 33 hours without finishing. Definitively proved `exactextract` is unscalable for massive regional groupings. Permanently adopted hybrid approach (`zonal_stats_toolkit` for regions, `exactextract` for 10km grids). Drafted open source feature request for C++ level `groupby`.
*   **Methodological Pivot (True Regional Baselines):** Configured regional base-year extraction to strictly bypass the 10km grid. By summarizing directly from the per-hectare rasters to the large spatial units, we bypass MAUP and grid-level division-by-zero artifacts.
*   **Pipeline & Cache Fixes:** Resolved Quarto caching trap in `KS_tests_hotspots.qmd` and re-enabled hotspot export chunks.
*   **Housekeeping:** Archived legacy QA/QC validation scripts.

### 2026-03-18
*   **Difference Analysis Pipeline Fixes & Completion:** Resolved persistent C++ `Segmentation fault` crashes in exactextract backend caused by microscopic topological errors. Implemented aggressive pre-processing. Completed "Mean of Differences" (Path C) extraction for regional groupings. Created `aggregate_yaml_outputs.py` for mathematical recombination.

### 2026-03-16
*   **Bi-temporal Change Validation:** Successfully cross-validated Symmetric Percentage Change (SPC) calculations between the R pipeline (`process_data.qmd`) and the Python SQLite pipeline (`calculate_bitemporal_change.py`). Both produced mathematically identical results. Confirmed R pipeline as primary workflow due to in-memory speed.

### 2026-03-10
*   **Data Consolidation (Path B):** Finalized primary base-year services dataset (`interim/10k_grid_services_base.gpkg`) using per-hectare corrected base year rasters. Bumped analysis version to **v1.2.1**.
*   **Difference Analysis (Path C):** Completed `summary_pipeline_landgrid.py` run on hectare-normalized difference rasters to establish the "Mean of Differences" dataset. Created `analysis/Consolidation.qmd` to load and validate outputs from Path B vs Path C.

### 2026-03-09
*   **Final Base Year Extraction (Per Hectare):** Initiated fresh run of summary statistics extraction for 1992 & 2020. Corrected volumetric variables to "per hectare" basis for global consistency. Configured `services_diff_ha.yaml` for Path C analysis.

### 2026-03-04
*   **V2 Pipeline Optimization:** Implemented caching for `plt_long`, added fallback logic for LCC driver column names, optimized GPKG export, and corrected export loops in `hotspot_extraction.qmd`. Synchronized continent/biome filters in KS tests.

### 2026-03-03
*   **V2 Pipeline Debugging:** Identified and fixed corrupt `grid_fid` issue causing NAs in `10k_change_calc_v2.gpkg`. Launched full V2 hotspot extraction. Created `compare_hotspots_v1_v2.qmd` to compute Jaccard Index overlaps between methodologies.

### 2026-02-27
*   **V2 Pipeline Implementation:** Created `analysis/process_zonal_stats_v2.qmd` to calculate Symmetric Percentage Change. Updated extraction/KS notebooks to be version-aware (`input_gpkg` and `output_suffix` params).

### 2026-02-24
*   **Refined Granular LCC Workflow:** Created `LC_change_preparation.qmd` for raw ESA/C3S extraction and 9-class reclassification. Updated `LC_change_granular.qmd` and removed testing limits. Launched 48h global extraction in `screen` session. Created `viz_granular_lcc.qmd`.

### 2026-02-20
*   **Granular LCC Analysis:** Created `LC_change_granular.qmd` to implement specific driver models (Forest Loss, Expansion) using `diffeR` metrics. Parameterized input GPKG path in `hotspot_extraction.qmd` for workflow flexibility.

### 2026-02-17
*   **LCC Pipeline Finalization:** Fixed `fid` vs `grid_fid` conflict in `LC_change.qmd`. Fixed grouping aggregation logic to generate `lcc_summary_by_group.csv`. Implemented chunked processing (50k cells/chunk).

### 2026-02-13
*   **LCC Integration & Documentation:** Validated `hotspot_extraction.qmd` logic for LCC overlap (Drivers of Change). Updated documentation to formally include the LCC pipeline and `diffeR` methodology.

### 2026-02-11
*   **Strategic Narrative / Pitch:** Defined the "Drivers of Change" strategy to attribute hotspots to Land Conversion (via `diffeR`) versus Degradation/Intensification.

### 2026-02-10
*   **Land Cover Change Integration:** Shifted focus to attributing hotspots. Created `analysis/land_cover_change.qmd` to compute binary transitions (Natural/Transformed) from ESA 300m maps.

### 2026-02-04
*   **KS Analysis Finalization & Methodology Refinement:** Optimized data pivoting in KS tests, implemented "signed power" transformations for plots, centralized configurations, refined groupings (removed `region_un` and `continent`), and documented Sum vs. Mean aggregation logic.

### 2026-02-02
*   **Hotspot Intensity & Multi-service Analysis Fixes:** Updated `hotspot_intensity.qmd` to calculate against total area and implemented Relative Intensity metric. Fixed setup chunks and alphabetical ordering in `hotspot_multiservice.qmd`.

### 2026-01-31
*   **Refactoring and Scope Refinement:** Initiated refactoring of `Consolidation.qmd` into `prepare_data.qmd` and `process_data.qmd`. Focused groupings on `income_grp`, `region_wb`, and `WWF_biome`.

### 2026-01-21
*   **Technical Issue Resolution:** Confirmed "fat tail" and bi-modal distributions are inherent properties of Symmetric Percentage Change (SPC). Investigated persistent sign flips (MAUP artifact). Added "Hotspot Area Analysis".

### 2026-01-19
*   **Ratio Calculations:** Created `calculate_ratios.py` to generate reliable sediment and nitrogen retention ratios with parallelized, tiled processing and `BIGTIFF=YES` support. Added automated statistical checks.

### 2026-01-16
*   **Repository Cleanup:** Archived legacy R zonal stats workflows (`zonal_stats.qmd`, `asign_ids_grid.qmd`).

### 2026-01-09
*   **Difference Rasters:** Implemented `batch_raster_diff.py` to calculate 2020-1992 difference rasters to support the transition to `zonal_stats_toolkit`.

### 2026-01-07
*   **Sign Flip Resolution:** Resolved absolute vs percent polarity issues by centralizing logic and normalizing service names.

### 2026-01-06
*   **Bug Fixes & Handoff:** Fixed `c_fid` drop bug in `Consolidation.qmd`, normalized service names, bumped to v1.0.1. Extracted pipeline overview to `README_pipeline.md`.

### 2026-01-05
*   **AI Context Migration:** Created `ai_context.md`, migrated to AI assistant (Copilot / Gemini).
`n### 2026-06-16`n- **Documentation Clarification:** Standardized the definition of 'hotspot' across the repository (including `README.md`, manuscript `index.qmd`, `paper_draft.qmd`, methodology, and book chapters) to explicitly state it is the top 5% of grid cells by rank, not a value-based percentile.
