---
title: "Hotspot Redesign Plan — 5-Service Set, New Overlap Categories, Beneficiary Reanalysis"
status: "DRAFT — for Becky's review, she is on holiday this week"
date: "2026-07-28"
branch: "feature/hotspot-5service-redesign"
---

## ⚠️ NEXT SESSION — READ THIS FIRST

1. ~~Monthly progress report~~ — drafted (6-column format: successes, upcoming priorities,
   challenges/opportunities, publications, speaking engagements, excited-about). Covers this
   month's work across three threads: the Global NCP hotspot redesign, SWY literature review, and
   the IADB-WWF workshop series. **Not confirmed whether the user has copied the final version
   into the actual report/spreadsheet yet** — check before assuming this is fully closed out.
2. ~~Verify the native-change-figure regeneration~~ — done: confirmed after session sign-off,
   4-row layout renders correctly (see WORKLOG "2026-07-29 (evening close-out)"). Nothing to
   check here.
3. ~~Reach out to Becky with today's progress~~ — **done**: user sent the native-10km map outputs
   to Becky (`outputs/maps/map_native10km_{pct,abs}.png`, plus the individual per-service panel
   files in `outputs/maps/native10km_panels/`). Not confirmed whether the fuller written progress
   summary (5-service maps recap, Figure 9 fix, Phase 5.3 ruled out) or the orange/teal
   color-scheme explanation accompanied the maps or still needs to follow separately — check with
   the user if it matters, but this is no longer a blocking item either way.
4. ~~Subregional (income/region/biome/country) hotspot reruns~~ — **partially done, 2026-07-30**:
   area/coverage/share + multi-service hotness stats rerun on the corrected 5-service definition
   (`hotspot_area_stats.csv`, `hotspot_multiservice_stats.csv`, all 219 regional-subset CSVs
   regenerated). Population exposure (HDI/GDP/Gini) deliberately skipped this round — same
   heaviest-chunk deferral as before, and conceptually distinct from Rich's pending buffered-
   beneficiary rerun. Along the way, fixed a real syntax bug (stray `<` in `HOTS_CFG`, never
   caught because the chunk had been `eval: false` since June) and the notebook's stale 8-service
   config. The summed `n_total` denominator (1,302,099) initially looked like it might be the same
   stale figure flagged below in "pending major paper edits" item #1 — traced fully same-day and
   confirmed legitimate, a different (and correct) denominator for a different question than the
   1,372,621 global-coverage figure; see WORKLOG (2026-07-30) follow-up entry. Not a blocker.
   Remaining: housekeeping items, waiting on Rich.

---

## Why this exists

Becky and Steve met last week (Jerónimo not present, Steve's first time weighing in) and raised
several concerns about the current 8-service hotspot analysis. Becky followed up directly in
Slack with concrete instructions. This document turns both into one ordered execution plan,
flags what's genuinely open/uncertain, and is meant to be adaptable into a status message back
to Becky (she's on holiday, but wants the hotspot rasters ready for Rich to pick up).

**Checkpoint status**: all prior work committed and pushed to `feature/swy-model-integration`
(commit `711a621`). This plan executes on a new branch, `feature/hotspot-5service-redesign`,
branched off that checkpoint — if anything here needs to be rolled back, that branch point is
safe to return to.

---

## Status as of 2026-07-28

**Resolved**: the Figure 9 / >10B population bug (open question #2 below) — confirmed root
cause (a "Global" summary row was double-counted in `analysis/plot_multiplier_effect.R`), fixed,
verified against the manuscript's already-correct prose numbers (no text changes needed
anywhere), regenerated, paper/book/presentation re-rendered. Full detail in WORKLOG
(2026-07-28 entry).

**Update (2026-07-29)**: Phase 1 (5-service extraction) and Phase 2 (the 3 overlap maps) are
now both done. Rasters + maps handed off to Rich, buffer-distance and LC-attribution questions
sent to Becky. Also added, beyond the original 3 maps: 2 pairwise breakdowns of the access
category (Access+Coastal Risk, Access+Pollination — the original 3-way access map hid a real
signal since Coastal Risk is a narrow shoreline-only phenomenon), plus a summary CSV + faceted
bar chart giving cell counts/land-area shares for every tier across all 5 maps. Remaining:
Phase 5.3 (biome/mangrove row-offset check), Phase 5.1/5.2 (10km-native change maps, restructured
export/retention figure), and the housekeeping items below. See
`scripts/mapping/make_5service_overlap_maps.R`, `scripts/mapping/make_5service_overlap_summary.R`,
and `docs/hotspot_5service_rasters_README.md` for what was delivered.

**Update (2026-07-29, later still)**: Phase 5.3 (biome/mangrove row-offset check) is done —
**ruled out, not a bug**. Tested all 3,423 `WWF_biome == 'Mangroves'` cells in
`10k_change_calc.gpkg` against a fresh, independent `gpd.sjoin` against raw `Biome.gpkg`,
bypassing `build_master_grid.py`/`enrich_grid.py`'s merge chain entirely — zero mismatches.
Code review also confirmed the merge chain uses genuine index-label joins throughout (`gpd.sjoin`
preserves left-index values; the final restore-geometry step is `left_index=True,
right_index=True`, not a positional `cbind`/`concat`), so this was never structurally the same
failure pattern as the historical `seq_len()` bugs. Both known artifacts in the biome-level
change maps (this one and the Mongolia one) now trace to biome-level aggregation itself, not a
join/ID bug — see WORKLOG (2026-07-29, "Phase 5.3" entry) for full detail.

**Update (2026-07-29, later still, cont.)**: while waiting on Rich, did prep work so his water-
hotspot/access-hotspot beneficiary folders can be absorbed without another code edit. Found
`analysis/compare_exposure_serviceshed.R` was dead code (read a per-folder CSV layout that
`Python_scripts/extraction_script.py` replaced months ago; its own output was referenced nowhere
else in the repo) and removed it — a small piece of housekeeping item 3 done early. Generalized
the two scripts that are actually live: `extraction_script.py` now auto-discovers
`hotspot_beneficiaries/` subfolders instead of a hardcoded 4-item list, and
`plot_multiplier_effect.R`'s compound dumbbell section now drives its category handling off a
`category_defs` table (pre-populated with water/access/combined entries pointing at the 5-service
gpkg) instead of a hardcoded `case_when()`. Regression-verified: reran the script, zero byte
differences in any output vs. before the change. Full detail in WORKLOG (2026-07-29, "Removed
dead compare_exposure_serviceshed.R" entry).

**Update (2026-07-29, later still, cont. 2)**: Phase 5.1 + 5.2 are done. New script
`scripts/mapping/make_native_change_figure.R` plots `10k_change_calc.gpkg`'s native grid directly
(no dissolve step -- eliminates the Mongolia-class artifact and, separately, the mean-value
sign-flip risk that dissolved maps carry) in paired export/retention rows (nitrogen -> sediment ->
coastal -> pollination -> nature access). Rasterized via `terra` for ~5x the speed of a direct
vector render; basemap added so sparse-data services (Coastal Risk) don't look broken; standalone
per-service panels saved at native pixel resolution for zooming. Output filenames are new
(`map_native10km_*.png`), not a silent overwrite -- `map_biome_*.png` and its "WWF Biome" captions
are untouched. Full detail in WORKLOG (2026-07-29, "Phase 5.1 + 5.2" entry).

**Update (2026-07-29, evening close-out)**: native change figure switched to a 4-row layout
(Pollination + Nature Access sharing a row, per Becky's suggestion) instead of 5; goods/damages
color direction re-verified against the live `HOTS_CFG$loss`/`HOTS_CFG$gain` in
`hotspot_extraction.qmd` (not just cross-script consistency). Regeneration completed and was
visually confirmed correct after session sign-off. See WORKLOG.

**Update (2026-07-29, evening close-out, cont.)**: fixed a recurrence of a previously-solved
problem — opaque near-white fill at near-zero values was hiding the basemap in several panels
(N Ret Ratio, Sed Ret Ratio, Pollination, Nature Access worst affected). Same fade-to-transparent
fix already used in `make_paper_supplement_maps.py`, ported to this script. Regenerated and
visually confirmed. **Phase 5.1/5.2 fully done, nothing outstanding on this figure.** Flagged in
WORKLOG as a pattern to check for in any other basemap+diverging-scale script.

**Update (2026-07-30)**: Subregional (income/region/biome/country) hotspot reruns — **area/hotness
part done**, population-exposure part deliberately deferred again. See "NEXT SESSION" block above
and WORKLOG (2026-07-30) for the syntax bug + stale config found and fixed along the way. The
1,302,099-vs-1,372,621 denominator question (below, item #1) was resolved same-day — both numbers
are legitimate, for different purposes; not a live bug.

**Update (2026-08-06)**: Rich's beneficiary rerun (Phase 3) is done, data in hand and verified
(pct metric confirmed throughout). Phase 4 (Gini/HDI/GDP disproportionality test) is now
unblocked and is the next real piece of work — see Phase 4 section below for the plan.

**Not started yet**: subregional population exposure (HDI/GDP/Gini), Phase 4 (Gini/HDI/GDP test
on beneficiary masks — unblocked, not started), grid file naming housekeeping item.

**In progress**: Phase 1 (5-service extraction, global scope) — done via a new standalone script,
`scripts/extract_hotspots_5service.R`. Produced `data/processed/hotspots_5service/{pct,abs}/
global/hotspots_global_5service_{pct,abs}.gpkg` with the 3 new overlap columns (`count_water`,
`count_access`, `combined_cross`). pct metric: 189,927 hotspot cells (1.88%), water=110,756,
access=127,172, combined_cross=48,001. Not yet rasterized for Rich (next step) — see Phase 2.

**Near-miss caught and fixed during this step**: `landgrid_1_clean_enriched_4326.gpkg` (the file
the existing notebook falls back to when no in-memory grid is available) has **no ID column at
all** and would have silently produced a fake positional ID. Switched to `10k_change_calc.gpkg`
and verified its `grid_fid` matches the canonical hotspot output's identity space exactly (zero
mismatches across 225,113 cells, all shared attributes). Full detail in WORKLOG
(2026-07-28, "Near-miss" entry).

---

## Pending major paper edits (defer until this redesign settles — don't lose track)

A running list of substantive paper/book fixes identified during this redesign work, deliberately
**not** being done now — the paper is being left alone until the 5-service numbers are final and
Becky has weighed in on the open structural questions. Do these as one batch once Phase 1-4 land:

1. **The "19.37% / 1,302,099 evaluated cells" figure is wrong, not just old.** Traced to
   `Python_scripts/extract_book_data_fills.py`'s `total_cells_global = ...sum() / 8` — a flawed
   average that only works if every service has the same valid-cell count (they don't: Coastal
   Risk has ~80,000 valid cells vs. ~1.3-1.5M for the rest). The correct distinct-cell denominator
   (verified directly against `10k_change_calc.gpkg`) is **1,372,621** for the same
   Antarctica/Seven Seas/Lakes/Rock & Ice exclusion used throughout this analysis. Needs a proper
   recompute of the paper's hotspot-coverage percentage once the final (5-service) hotspot count
   is in hand — full detail in WORKLOG (2026-07-29 entry).
   **Confirmed still live (2026-08-05, while drafting the AGU abstract)**: `paper_draft.qmd` line
   231 still states "252,215 unique cells... (19.37% of evaluated cells)". Correct figure using
   the verified 1,372,621 denominator: **252,215 / 1,372,621 = 18.37%** (18.4% used in the AGU
   abstract, `docs/manuscript/agu_abstract_2026.md`). Not fixed in the paper draft itself yet —
   still deliberately deferred per this item's original plan (recompute once the 5-service count
   is final, since the percentage changes again under that definition), but now has the precise
   corrected number on record so it doesn't need re-deriving next time.
2. **LC-change/attribution section removal from the paper** — pending Becky's confirmation (message
   sent, not yet answered). Book keeps this content regardless.
3. **5-service methodology section — approach changed (2026-08-06).** Instead of staying staged in
   `docs/paper_5service_methodology_staging.md` (which the user found awkward — content that never
   actually gets used), forked a real second paper draft: `docs/manuscript/paper_draft_5service.qmd`,
   a full copy of `paper_draft.qmd` with a status callout marking it in-progress. Original
   `paper_draft.qmd` is untouched — matches what's safe in the pre-redesign branch. Abstract
   already rewritten with real, verified 5-service numbers (hotspot count/land share, water/access/
   combined-cross breakdown, beneficiary exposure, regional/income disparity — see WORKLOG
   2026-08-06), explicitly flagging LC-conversion attribution and Phase 4 as not yet rerun under
   this definition rather than reusing stale 8-service numbers. Body chapters still copied
   unmodified from the 8-service original — need a full pass once Phase 4 lands and Becky confirms
   the open structural questions. The staging doc can be retired once this file is the actual
   working draft.
4. **`audit_claims.R` full rerun** — every hotspot-count-derived number in the paper needs
   re-verifying once the 5-service definition is finalized (see reconciliation table below).
5. **Access map insight for the paper/book text**: the 3-way access map (Access+Pollination+Coastal
   Risk) undersells its own best signal — worth a sentence noting Access+Pollination is the real,
   broad terrestrial story (12,106 cells) while Access+Coastal Risk is a genuine but narrow
   shoreline phenomenon (967 cells), rather than presenting them as one blended category.
6. **Swap the main change figure reference from `map_biome_{pct,abs}.png` to
   `map_native10km_{pct,abs}.png`** in `paper_draft.qmd` (lines ~226/228) and
   `03-global-patterns-WHAT.qmd` (lines ~37/39) once ready — new figure is built and verified
   (Phase 5.1/5.2, 2026-07-29), captions will need updating too since "WWF Biome" no longer
   describes the new figure's content.
7. **GDP and Gini gridded-data citations are unresolved, not just unstated — new item (2026-08-05).**
   `02-methods.qmd` line 66 has a confirmed HDI citation (Sherman et al. 2026) and population is
   well-established (GHS-POP), but GDP is marked "citation TBD — verify source" (`01-problem.qmd`
   line 126, candidate: Kummu et al. 2018, not confirmed) and Gini has no citation at all yet
   (`02-methods.qmd` line 68: "citation — to verify: source of `rast_adm1_gini_disp_2020`").
   Per the user: this isn't a principled omission, the data was obtained from somewhere at the
   time and the source was never tracked back down since — needs an actual research pass to
   confirm/cite both before the paper can go out, not just a wording fix. Not blocking the AGU
   abstract (abstract-level detail doesn't need full source citations, unlike a full paper under
   peer review), but a real gap in `paper_draft.qmd`'s reference list and the two methods chapters
   above.

---

## Five housekeeping items to close out before this redesign is considered "done" (not blocking tonight's Rich handoff, but don't let them get lost)

0. **R-package hygiene (NAMESPACE/`.Rd` drift) — new item (2026-08-03), explicitly low priority.**
   `devtools::document()` hasn't been run in months; `NAMESPACE` now has real entries for functions
   that no longer exist in `R/` source (`identify_hotspots`, `make_hotspots`, `align_rasters`, plus
   some non-function garbage like `export("(simple,")`, likely a mangled roxygen comment
   somewhere) — visible as an "Objects listed as exports, but not present in namespace" warning on
   every `devtools::load_all()` call. Traced during the combos-documentation work (item 2 below);
   deliberately **not** fixed then, since running `document()` cleanly would delete ~19 unrelated
   `.Rd` files and rewrite `NAMESPACE` well beyond that task's scope — see WORKLOG (2026-08-03).
   Root cause per the user: this repo was originally scaffolded as an R package, but has grown far
   beyond that shape; full package hygiene isn't a current priority and a proper package
   restructure is a possible future project, not now. Not blocking anything — `devtools::load_all()`
   sources everything directly regardless of `NAMESPACE` state, which is why this has been silently
   fine for months. Fix whenever it becomes actually annoying, not before.

1. **Grid file naming/consolidation.** This repo has several similarly-named, undocumented
   grid-like gpkgs (`landgrid_1_clean_enriched_4326.gpkg` — no ID column — `10k_change_calc.gpkg`,
   `10k_lcc_granular_metrics.gpkg`, `AOOGrid_10x10km_land_4326_clean.gpkg`, one with a literal
   timestamp baked into the filename) with no documented hierarchy anywhere. This ambiguity is
   the root cause behind four separate grid-identity incidents in this project now (grid-ID
   crosswalk bug, many-to-one join bug, the 2026-07-08 WORKLOG `seq_len()` fallback, and tonight's
   near-miss). Needs a proper pass: document which file is canonical and why, and/or
   rename/consolidate so there's one unambiguous master grid file — not several
   similarly-named candidates of unknown relative authority. Deliberately not done tonight
   (renaming under time pressure risks breaking references across dozens of scripts) — flagged
   so it doesn't quietly disappear.

   **Same root cause, fresh instance (2026-08-11/12):** the ID column name itself is inconsistent
   across otherwise-related files — `10k_change_calc.gpkg` and the per-country hotspot gpkgs
   (`data/processed/hotspots/pct/nev_name/*.gpkg`) use `grid_fid`, while `plt_long.rds` carries
   both `fid` *and* `grid_fid` as separate columns. Cost real time twice while building the
   Colombia CLEC/Sandra scripts this session (wrong assumption `fid` was the universal ID,
   caught only when a join silently failed). Same underlying problem as the grid-file
   proliferation above — needs to be resolved together as one definitive pass (canonical grid
   file + canonical ID column name, documented), not patched file-by-file as scripts hit it.

2. **Document the `combos` mechanism as a first-class, user-facing capability — DONE (2026-08-03).**
   - **Written how-to**: added to `docs/methodology.md`, new "Multi-Service Overlap Combos" subsection
     under "Change Metrics & Hotspot Definition" — what `HOTS_CFG$combos` is, how a named list of
     service vectors becomes a `count_<name>` column automatically, and the water/access worked
     example from the 5-service redesign.
   - **Reusable helper, not just prose**: added `derive_cross_combo()` to `R/get_hotspots.R` —
     takes two (or more) combo names and returns the AND-derived column, replacing the hand-written
     `mutate(new_col = count_A > 0 & count_B > 0)` pattern in `scripts/extract_hotspots_5service.R`.
     Smoke-tested against a toy data frame, confirmed correct. Exported via `NAMESPACE` (added by
     hand, not via a full `devtools::document()` — see WORKLOG for why), `man/derive_cross_combo.Rd`
     generated properly.
   - **Deliberately NOT bundled with the 7-file service-config consolidation** mentioned in this
     item's original scope — that's a separate, larger, riskier task (touches config in 7 files at
     once, the same failure class as past grid-ID incidents) and wasn't part of what was picked up
     this session. Still open, on its own.

3. **Script consolidation/cleanup.** This session (and the sessions before it) have accumulated
   standalone diagnostic/one-off scripts (`scripts/extract_hotspots_5service.R`,
   `scripts/compute_attribution_true_union.R`, various ad-hoc `check_*.R` scratch scripts) written
   quickly to answer an immediate question or unblock a specific step. Before this redesign is
   considered finished, do a pass to: (a) decide which of these are genuinely reusable pipeline
   steps that should be properly named, documented, and either merged into the relevant `.qmd`
   notebook or kept as a permanent, well-documented script; and (b) delete anything that was
   truly one-off/diagnostic and has served its purpose. Don't let the repo accumulate an
   ever-growing pile of similarly-named scratch scripts — that's the same class of problem as
   item 1 above, just for code instead of data files. **Partial progress (2026-07-29):**
   `analysis/compare_exposure_serviceshed.R` identified as dead (superseded by
   `extraction_script.py` + `plot_multiplier_effect.R`, referenced nowhere else) and removed —
   see WORKLOG. **Done (2026-07-30):** audited every remaining standalone script — none turned
   out to be genuinely dead/one-off (the `check_*.R` scripts mentioned above no longer exist in
   the repo); this was actually a documentation gap, not a deletion job. Added ~12 missing
   entries to `scripts/README.md`, fixed a stale entry (`audit_claims.R`/`export_reclass_table.R`
   wrongly listed as archived when both are active), and removed one unrelated stray file
   (`Python_scripts/photo_processing.py`, a real-estate photo resizer with no connection to this
   project). See WORKLOG (2026-07-30, "Housekeeping item 3"). **This item is now closed.**

4. **`data/` vs `outputs/` split is undocumented and arbitrary — new item (2026-08-05), not started,
   scoped down after discussion same day (see below).**
   Two parallel "tables" directories exist with no written rule for which analysis outputs go in
   which: `data/processed/tables/` (gitignored, 11 entries — `hotspot_area_stats.csv`,
   `ks_results_hot_vs_non.csv`, `lcc_*` summaries, the 219-file `regional_subsets/`, plus a
   `_deprecated/` folder — evidence this exact staleness problem already happened once and was
   hand-managed rather than fixed) and `outputs/tables/` (git-tracked, 11 entries — `multiplier_summary_*`,
   `exposure_comparison*`, `lcc_reclassification_table.csv`, and the new `hotspot_5service_category_shares_*`
   files from 2026-08-05). The only real distinction findable is that `data/` is entirely gitignored
   and `outputs/` is git-tracked (`.gitignore` line 43) — but that's never written down as the actual
   rule anywhere, so every script picks a location by feel, not convention. Same root cause as
   housekeeping items 1 and 3 (undocumented parallel structures inviting silent drift), just for
   directory layout instead of grid files or scripts.

   **Scoping discussion (2026-08-05):** first instinct was "move all the tables to the tracked
   folder since they're small" — checked, and they are small (`data/processed/tables/` is 1.4MB
   total including all 219 `regional_subsets/` files; `outputs/tables/` is also 1.4MB) so size was
   never actually the constraint. But a bulk move isn't the right fix anyway — the real concern,
   per the user, is **consistency, traceability, and avoiding duplicate/stale versions of the same
   thing**, not git-tracking everything. A blanket move would also add real risk: **16 files
   reference `data/processed/tables/` paths**, including several actual manuscript chapters
   (`docs/manuscript/chapters/04-hotspot-WHERE.qmd`, `06-hotspot-WHO.qmd`, `07-regional-profiles.qmd`)
   — not something to rename casually.

   **Revised plan**: keep the existing two-tier split (it's already roughly right) but (a) write
   it down explicitly in `docs/methodology.md`'s "Output Directory Structure" section — `data/` =
   pipeline-stage/regenerable intermediate outputs (high file-churn per rerun, e.g. the 219
   `regional_subsets/` files, not worth git-tracking), `outputs/` = curated/final/citable
   deliverables meant for the paper/stakeholders — and (b) do a reconciliation pass checking for
   actual near-duplicate concepts living in both places under different names. One concrete
   candidate found while comparing the two folders, **not yet verified**:
   `data/processed/tables/hotspot_pop_exposure.csv` vs `outputs/tables/regional_pop_exposure.csv` —
   similar enough names that it's not obvious without reading both generating scripts whether
   these are genuinely different analyses or a near-duplicate under two names.
   Deliberately not started — flagged per the user so it doesn't get lost, same as items 0-1.

5. **Country-report toolbox — new item (2026-08-12), not started.** The Colombia CLEC/Sandra
   session (2026-08-11/12) produced 5 standalone, Colombia-hardcoded scripts:
   `scripts/mapping/make_colombia_report_maps.R` (change panels, hotspot map, beneficiary map),
   `make_colombia_relative_intensity_chart.R` (share-vs-expected and coverage bar charts, split
   focus/others), `make_colombia_biome_analysis.R` (intra-country biome breakdown), `make_colombia_
   critical_assets_map.R`, and `make_colombia_priority_overlap.R` (critical assets ∩ hotspots of
   change). Each duplicates the same grid-loading (`10k_change_calc.gpkg` filtered to one country),
   WWF color palette, and output-path logic, hardcoded to `nev_name == "Colombia"` — none of it is
   reusable for another country/region without rebuilding from scratch. User flagged this directly:
   "are we going to have a set toolbox of functions to create maps in which we only give e.g. the
   target country/polygon variables and it generates the maps?" — yes, that's the right shape.
   **Scope for the eventual pass**: extract a parameterized function (or small set of functions,
   one per map/chart type) taking a country/polygon filter and the standard set of variables, that
   reproduces this exact map/chart set for any country. Needs real design thought (which parts stay
   fixed vs. parameterized — service list, biome grouping, focus-vs-other split — and how a country
   with a different biome/service composition than Colombia should degrade gracefully), so
   deliberately not done alongside the CLEC/Sandra deadline. Same underlying pattern as items 1 and
   3 above (undocumented one-off proliferation), just for country-scoped analysis scripts.

---

## Reconciliation with the pre-meeting plan (memory, as of ~2026-07-09)

Before last week's Becky/Steve meeting, the working assumption (tracked in project memory, not
in this repo) was that the paper was close to submission-ready, pending a short list of
confirmations from Becky. That list needs to be re-read against what actually came out of last
week's meeting — some items are now superseded, some still stand independently, and at least one
needs to be *redone*, not just answered, because the hotspot definition itself is changing.

| Old open item (pre-2026-07-09 "MUST ASK BECKY" list) | Status now |
|---|---|
| #0 Attribution-gap sign-off (34.5%/65.5% framing) | **Superseded.** Those exact numbers were since corrected again (2026-07-24, now 34.2%/65.8%) — but more importantly, last week's meeting raised moving attribution analysis out of the main paper entirely (supplement, country/biome aggregation, or a separate follow-on paper). The sign-off question is moot until that structural decision is made; don't chase the old framing further. |
| #1 Paper structure (methods to supplementary? target journal?) | **Still open, untouched by this redesign.** Independent question, still needs Becky's answer whenever she's back. |
| #2 SDR/NDR climate inputs (fixed vs. era-specific) | **Still open, untouched.** Independent of the hotspot redesign. |
| #3 Ch05 scatterplot (replace X-axis or drop) | **Likely moot** if attribution moves out of the main paper — revisit only if attribution stays in. |
| #4 Literature validation of hotspot locations | **Do not do this yet.** It would validate the *current* 8-service hotspot clusters — pointless to spend time on before the 5-service redesign lands, since the clusters themselves are about to change. Explicitly deferred until Phase 1-2 complete. |
| #5 Rt_serv confirmation (C_Risk/C_Risk_Red_Ratio only) | **Still open, independent** — though note C_Risk_Red_Ratio is one of the 3 services being dropped from the hotspot definition (not from the underlying data), so this question may partly resolve itself. |
| `audit_claims.R` / systematic claim verification pass | **Must be rerun from scratch** once the 5-service redesign lands — every hotspot-count-derived number in the paper changes when the service set changes. Don't spend time re-verifying 8-service-era numbers now. |
| Biome boxplots → Annex (deferred item) | **Related but distinct** from the new "replace biome-level change maps with 10km-native maps" ask — check whether these are the same assets or two separate things once Phase 5.1 starts. |
| Phase 5 candidates (WWF Colombia deck, corporate supply chain, GDP exposure) | **Unaffected for now**, but any of these picked up later will need to reflect the new 5-service hotspot definition, not the old 8-service one. |

**The honest summary**: the paper was in "final polish, minor confirmations" mode before last
week. It is not anymore — the 5-service redesign changes the hotspot definition itself, which
cascades into every hotspot-count, intensity, and beneficiary number in the book and paper. That
said, per the technical research already done (see Phases 1-2 above), this is a **bounded,
mechanical effort** — a config change plus a rerun, not a rebuild — the underlying pipeline
architecture holds up fine.

---

## Two things to verify before or alongside execution (not blocking, but worth a quick check)

1. **Buffer distance discrepancy.** Meeting notes say the downstream buffer may be "too
   generous" at 500km. This repo's own methods documentation (`02-methods.qmd`) and the actual
   output filenames (`full_raster_extent_downstream_50k_population.tif`) both say **50km**
   flow-accumulation threshold, not 500km. Either the notes are misremembering, or Rich's
   `wwf_es_beneficiaries` repo uses a different/newer value not reflected in this repo's docs.
   **Action:** ask Rich directly what the current downstream threshold actually is before
   assuming either number.

2. **The 96% / 10B+ / double-counting numbers likely aren't a data bug.** The existing
   beneficiary output structure has four overlap-tier folders (`all hotspots`, `2+`, `3+`, `4+`
   overlapping) that are **nested subsets, not mutually exclusive bins** (4+ ⊂ 3+ ⊂ 2+ ⊂ all).
   Summing "beneficiary count" across those four tiers as if additive would overshoot world
   population immediately — this matches the notes' own "scale error, not a data error"
   instinct closely. **Action:** before rebuilding anything, find out exactly which figure/sheet
   produced the 96%/10B+ numbers and check whether it summed across tiers instead of treating
   them as nested.

---

## Phase 1 — Hotspot redesign: 5 services, 3 new overlap map types

**Goal (from Becky's Slack message, verbatim instructions):**

Keep only: Nitrogen Export, Sediment Export, Coastal Risk, Pollination, Nature Access.
Drop: Nitrogen Retention Ratio, Sediment Retention Ratio, Coastal Risk Reduction Ratio (the
"same pollutant, not independent" problem — retention increases can reflect upstream
degradation, not local improvement, and explaining that nuance would cost too much space in the
paper).

Produce exactly 3 new overlap map outputs:

1. **Water overlap hotspots** — N export + Sed export, hotspot in either or both.
2. **Access/coastal/pollination overlap hotspots** — Nature Access + Pollination + Coastal
   Risk, hotspot in any one, two, or all three.
3. **Combined cross-category overlap hotspots** — across all 5, but keep only cells with **at
   least one water-service hotspot AND at least one non-water-service hotspot** (exclude
   water-only or access-only cells).

**How this maps onto the existing code** (traced directly, not guessed):

- `analysis/hotspot_extraction.qmd`'s existing `combo` mechanism (`HOTS_CFG$combos`) already
  does exactly "count how many named services are hotspots in this cell" — this is the right
  mechanism for categories 1 and 2:
  ```r
  combos = list(
    water  = c("N_export", "Sed_export"),
    access = c("Nature_Access", "Pollination", "C_Risk")
  )
  ```
  This gives `count_water` (0-2) and `count_access` (0-3) columns automatically, no core-logic
  changes needed.
- Category 3 (the cross-category AND) is **not** natively supported by the combo mechanism —
  needs a small addition: either a patch to `R/get_hotspots.R` for a new "cross" combo type, or
  a one-line derived column after extraction: `combined_cross = count_water > 0 & count_access > 0`.
- Dropping the 3 retention/ratio services means editing `loss`/`gain`/`combos` in `HOTS_CFG` —
  the underlying threshold/union logic in `extract_hotspots()` needs no changes, since it's
  already generic over whatever's in `loss`/`gain`.

**Real risk found during research — config duplication.** The service list + direction-of-bad
logic is currently copy-pasted across **7 separate files**, and one of them
(`analysis/compare_exposure_serviceshed.R`) already has an inverted (wrong) copy relative to the
others:

| File | What it holds |
|---|---|
| `R/utils_hotspot.R` | `svc_order` (canonical 8-service order) |
| `analysis/hotspot_extraction.qmd` | `svc_order`, `canonical_lookup`, `HOTS_CFG` (the live config) |
| `analysis/hotspot_synthesis.qmd` | a second, independent copy of `HOTS_CFG` |
| `scripts/mapping/make_faceted_maps.R` | `goods`/`damages`/`canon_order` |
| `scripts/mapping/make_paper_supplement_maps.py` | Python copy of GOODS/DAMAGES + name mapping |
| `gdal_rasterize_hotspots.sh` | `COLUMNS` array |
| `analysis/compare_exposure_serviceshed.R` | **inverted** loss/gain lists (pre-existing bug, unrelated to this redesign, found in passing) |

This project has already had three separate incidents caused by exactly this kind of silent,
duplicated-source-of-truth drift (the grid-ID crosswalk bug, the many-to-one join bug, and now
this). **Recommendation: while touching all 7 files anyway for the 8→5 cut, consolidate into
one canonical service-config object** (a single YAML or R list, sourced everywhere) so this
can't silently drift again. This is optional extra scope — flagging it as a recommendation, not
assuming you want it. Either way, the pre-existing inverted bug in
`compare_exposure_serviceshed.R` should get fixed while we're in there regardless.

**Deliverables for this phase:**
- Updated `HOTS_CFG` (5 services, 2 new combos + 1 derived cross-category column) applied
  consistently across all touched files.
- Three new hotspot gpkgs (global scope first, matching existing `hotspots_global_{pct,abs}.gpkg`
  pattern) with the new binary/count columns.
- Rerun `gdal_rasterize_hotspots.sh` to produce rasters for the new columns
  (`count_water`, `count_access`, `combined_cross`), in the same `data/processed/hotspots/rasters/`
  location Rich's pipeline already expects.

---

## Phase 2 — Map outputs for Becky/Rich

Three visual map outputs matching the three overlap categories above (water, access, combined),
plus rerunning the existing suite of hotspot maps against the new 5-service definition. Target:
**next week**, per the meeting notes — this is the most time-sensitive deliverable since Rich is
blocked on it for the beneficiaries rerun.

---

## Phase 3 — Handoff to Rich: beneficiaries rerun

Becky's Slack message names 4 existing config files in Rich's repo
(`jeronimo_{2,3,4,all}hotspot_beneficiaries.yaml`) and asks for **2 new, similar configs**:

- A **water-hotspot** config — downstream-only buffer, access/travel-time buffer turned off.
- An **access-hotspot** config — travel-time-only buffer, downstream buffer turned off.

This matches the meeting notes' service-specific buffer instruction exactly: water services
(N export, Sed export) → downstream only; coastal, pollination, nature access → travel-time
only. **This repo has no visibility into `wwf_es_beneficiaries`'s YAML schema or README** (it
lives on Rich's machine, not accessible from here) — so this repo's job is only to hand Rich
correctly-structured hotspot rasters/masks; the actual new YAML configs are Rich's to write
(Becky's message already asks him directly: "are we ready... can you do that for me"). Our
concrete deliverable here is making sure the water-only and access-only hotspot rasters exist in
a form his pipeline can point a config at.

**Output Becky wants once Rich reruns**: a table of beneficiary counts by overlap level, plus
downstream/access/combined beneficiary masks.

**Phase 3 is DONE (2026-08-06).** Rich returned all 7 categories (water, access, combined-cross,
and 5 nested hotspot-count tiers) — see `data/processed/hotspots_5service/rasters_5_var/` and his
actual configs in `data/jeronimo_2026_07_beneficiaries_analysis_configs/`. Which metric (pct/abs)
he used was initially unclear (both were sent to him in the same folder, undifferentiated — see
WORKLOG 2026-08-05) but is now confirmed: **every config uses the `pct` metric**, verified
directly against all 7 YAML files, not just his chat summary. Buffer logic also verified correct
against Becky's original spec. Area-percentage and population-exposure numbers already computed —
see WORKLOG (2026-08-05, "Becky's follow-up questions") and
`outputs/tables/hotspot_5service_beneficiary_area_pct.csv`.

---

## Phase 4 — Gini/HDI analysis on beneficiary masks

**DONE (2026-08-07).** Becky's ask: apply the union coverage masks
(`full_raster_extent_union_coverage.tif` per category) to the HDI, Gini, and GDP rasters to test
whether beneficiary areas (combined cross-category, or 3+/4+ tiers) are disproportionately high
or low compared to outside. Built as planned: zonal-extracted the fine-resolution (~30 arcsec)
union coverage masks onto the 10km analysis grid (`Python_scripts/zonal_extract_beneficiary_masks.py`,
run inside Rich's Docker environment — see WORKLOG for the local-toolchain detour), producing a
per-`grid_fid` coverage fraction, thresholded at ≥0.5 for a binary "inside beneficiary mask" flag,
then ran the existing KS/Cliff's Delta machinery (`R/ks_hotspots.R::run_ks_hot_vs_non()`, same
function the WHO chapter uses) via a new notebook, `analysis/KS_tests_beneficiary_masks.qmd`.

**Validated against an independent reference** before trusting any KS result: the zonal-extracted
coverage, aggregated back to % of land area, matches the already-published fine-resolution area
percentages (`outputs/tables/hotspot_5service_beneficiary_area_pct.csv`, 2026-08-05, computed via
a completely different code path — `terra::expanse()` directly on the rasters, no grid, no
exactextract) to within 0.5 percentage points for all 3 target categories. Full detail in WORKLOG
(2026-08-07).

**Headline result**: beneficiary-mask areas (combined-cross, 3+, 4+ tiers) are strongly,
significantly wealthier and more populated than the rest of the landscape (Cliff's δ ≈ 0.48–0.53
for both GDP and population, large effect; p_adj effectively 0 for all 12 service × variable
combinations tested, no exceptions) — consistent with buffers concentrating around where people
already are. Gini shows a smaller but real effect in the same direction (δ ≈ 0.22–0.33,
small–medium, significant at every tier) — beneficiary areas skew toward *more* unequal regions,
not less, and the effect *grows* with tier exclusivity rather than weakening. HDI shows the
weakest signal by far (δ ≈ 0.04–0.10, negligible–small) — beneficiary status is not strongly tied
to human development level one way or the other. Full table: `data/processed/tables/
ks_results_beneficiary_masks.csv`; plots under `outputs/plots/ks_beneficiary_masks/`.

---

## Phase 5 — Figure/map changes (can run in parallel with Phases 1-2)

1. **Replace biome-level change maps with 10km-native change maps — DONE (2026-07-29).** See
   `scripts/mapping/make_native_change_figure.R` and WORKLOG. Reason given: biome
   aggregation misrepresents fine-scale results (e.g., Coastal Risk appearing to affect
   Mongolia). **Gap resolved (2026-07-29): the missing generator script is
   `zonal_stats_toolkit/generate_map_gpkgs.py`** (the sibling repo at `/c/projects/
   zonal_stats_toolkit`) — it dissolves the raw global `Biome.gpkg` polygons by `WWF_biome` name
   and merges in per-biome service stats. **The Mongolia artifact itself is very likely not a
   bug** — dissolving by biome NAME merges every landlocked instance of a biome (Mongolia's Gobi)
   with every coastal instance of that same biome worldwide (Atacama, Namib), so the whole
   dissolved shape gets one blended value and landlocked land visually "inherits" a coastal
   signal that belongs to a different desert on another continent. This is exactly the
   representational limitation 10km-native maps are meant to fix — not something to debug
   further, just confirms the redesign direction is right.
2. **Main change figure restructured — DONE (2026-07-29), same script as item 1.** Paired rows, export next to retention for each service
   (e.g., N export | N retention), red = increases in bad things / green = enhancement, ordered
   nitrogen → sediment → coastal → pollination → nature access. Note: this figure still shows
   retention for context even though retention is dropped from the *hotspot definition* — two
   different roles for the same variable, worth being explicit about in the paper text so it
   doesn't read as inconsistent with the 5-service hotspot set.
3. **Mangrove/Coastal-Risk mismatch check — RESOLVED (2026-07-29), not a bug.**
   `generate_map_gpkgs.py`'s own join is by string name (not positional), so it's not the cause
   here. The suspected culprit — `build_master_grid.py`/`enrich_grid.py`'s `gpd.sjoin` against
   `Biome.gpkg` followed by an index-based merge back onto the grid — was checked both by code
   review (the merge is a genuine index-label join, not a positional `cbind`/`concat`) and by
   direct empirical test (all 3,423 `WWF_biome == 'Mangroves'` cells re-verified against a fresh,
   independent spatial join, zero mismatches). Not the source of the mismatch — see WORKLOG
   (2026-07-29, "Phase 5.3" entry).

---

## Deprioritized / paused, not part of this plan's active scope

- **SWY model integration** — explicitly paused again per your message; picked back up after
  this redesign lands.
- **Attribution analysis (LCC drivers) likely moving out of the main paper** — flagged in the
  meeting notes as a structural decision (supplement-only vs. country/biome aggregation vs.
  separate follow-on paper), with "consensus leaning toward a cleaner main paper." This is a
  discussion item for Becky, not something to execute unilaterally — noting it here so it isn't
  lost, not scoping work against it yet.
- **CN/Kc work for SWY** (three problematic biomes: mangroves, flooded forests, flooded
  savannas) — also paused, noted only for continuity with the already-recovered
  `swy_becky_meeting.qmd`.

---

## Suggested order of execution

1. ~~Checkpoint + branch~~ — done (`711a621` on `feature/swy-model-integration`; this branch is
   `feature/hotspot-5service-redesign`).
2. Consolidate/fix the 7-file service-config duplication (if you want that scope) + fix the
   inverted `compare_exposure_serviceshed.R` bug either way.
3. Phase 1: 5-service hotspot re-extraction + 3 new overlap categories.
4. Phase 2: regenerate maps (highest time-pressure item — Rich is blocked on this).
5. ~~Phase 5.3: biome/mangrove row-offset check~~ — done, 2026-07-29, ruled out (not a bug).
6. ~~Phase 5.1/5.2: 10km-native change maps + restructured export/retention figure~~ — done,
   2026-07-29.
7. Send Rich the water/access rasters; he writes the 2 new beneficiary configs and reruns.
8. ~~Phase 4: Gini/HDI KS test on the new beneficiary masks~~ — done, 2026-08-07.

---

## Open questions to fold into the message back to Becky

1. ~~Confirm the actual downstream buffer distance in current use (50km per this repo's docs, or
   500km per the meeting notes) — ask Rich directly.~~ **RESOLVED (2026-08-07)**: checked
   directly against Rich's actual configs rather than asking — `max_downstream_distance_m: 50000`
   (50km) and `max_hours: 1.0` (1-hour travel-time) are both explicit, consistent parameters
   across all 7 config files (`data/jeronimo_2026_07_beneficiaries_analysis_configs/*.yaml`).
   50km is correct; the 500km figure from the meeting notes doesn't match anything in the actual
   run configuration.
2. Confirm which output (figure/sheet) produced the 96%/10B+ numbers, to verify the
   nested-tier double-counting hypothesis before assuming it's the explanation.
3. Whether to consolidate the 7-file service-config duplication now (recommended, given this
   project's history with this exact failure mode) or defer as a separate cleanup pass.
4. Whether `biome_change_map.gpkg`'s generation step should be rebuilt as tracked code (it's
   currently not reproducible from anything visible in the repo) as part of this work, or
   whether that's acceptable technical debt to leave for later since the plan is to replace it
   with 10km-native maps anyway.
