# Devstack Compatibility — Research Notes

Status: research complete (2026-09-08), corrected same day (2026-09-08, see below), adoption work
not started. This branch (`feature/devstack-compat`) exists to do the actual adoption work
described below — read this file first in a fresh session, don't re-derive from scratch.

**CORRECTED 2026-09-08, same day as the original pass**: the research below was originally done by
reading `global_invest_dev`'s `main` branch. It has since been discovered that `main` is **707
commits behind `origin/develop`**, which is the actual actively-developed branch (0 commits on
`main` that aren't already in `develop` — `main` is simply stale, not a diverged release line).
Everything code-level below has been re-verified directly against `develop`. Two concrete
consequences: (1) both of the original "first PR" bug candidates (a `coastal_protection`
dead-code bug, a `pollination` `numpy.bool` crash) turned out to be **already fixed or entirely
absent on `develop`** — both modules were rewritten there — so they've been replaced below with
two newly-verified bugs; (2) the maturity/comparison read, done on `main` from 2 of 8 visible
service directories, undersold the codebase — `develop` has **24 service directories**, of which
**22 of 23 real services** (everything but the `example_service` template) follow the full
`run_/tasks_/functions_/initialize_/test_` five-file convention, and every one read in depth was
substantive, working science, not a stub. Sections below are marked inline where a specific claim
changed. The strategic/CLA/verdict decisions (adopt conventions, don't migrate to
ProjectFlow/Hazelbean, pursue a real PR rather than formal Derivative Project status) do not depend
on which branch was read and are unchanged.

**Also new as of this correction — the PR strategy itself changed.** The original plan was
"first PR = fix one of the two known bugs." Since both known bugs turned out to be invalid (the
team had already caught and fixed them), that plan changed direction: a small, active team (Justin
Johnson + Chiara Colesanti Senni doing the bulk of `develop`'s recent commits) is evidently fast at
catching exactly this class of mechanical bug in normal development, so "find a bug they missed" is
competing with people who are already good at that. **The primary first-PR candidate is now an
added capability** global_NCP has hardened that global_invest_dev's own code visibly wants but
doesn't fully have — see "Contribution candidate" under the Verdict section below. The two newly-
verified bugs are kept as a secondary, lower-priority mention, not the headline.

## Why this exists

While comparing notes on the SWY integration work, discovered `C:\projects\global_invest_dev`
(cloned locally, not part of this repo) — a parallel NatCap TEEMs effort by Justin Andrew Johnson
(University of Minnesota), part of a broader shared framework ("the devstack":
`justinandrewjohnson.com/earth_economy_devstack/`) underlying three projects: SEALS (spatial
land-use allocation), GTAP-InVEST (CGE + ecosystem services), and Global InVEST (`global_invest_dev`
itself). A background research agent did a thorough, code-level comparison — this file is that
report, condensed and organized for future reference, plus the resulting action plan.

**Personal/relationship context, not to be forgotten**: Justin reportedly respects Rich Sharp
(this project's co-author, whose `taskgraph`/`inspring`/`swy_global` tools this project already
builds on) as a developer, but their devstacks diverged at some point for unknown reasons — never
asked, don't know why. Given the user's direct working relationship and mentorship history is with
Rich, not Justin, the decision (2026-09-08) is: **adopt Justin's naming/structure conventions for
compatibility, but do not replace Rich's tooling** (`ecoshard.taskgraph`, `exactextract`, the
R-native statistical pipeline) with Justin's `ProjectFlow`/`Hazelbean`. Stay on Rich's stack; get
compatible with Justin's conventions, not migrated onto his engine.

**Strategic framing, stated directly by the user**: this could be a concrete, structured
contribution to bring to Justin after "months and months of apparently silent work" — a way to
demonstrate real capability regardless of what happens with the NatCap postdoc position. Also the
natural on-ramp into the previously-deferred NatCap tools crash course (see memory
`project_natcap_tools_crash_course.md`).

## What the devstack actually is

- **Hazelbean**: shared Python library wrapping GDAL, NumPy, SciPy, Cython, `pygeoprocessing`,
  `taskgraph`, `natcap.invest`, `geopandas`. Core value: `hb.raster_calculator()` streams global
  rasters chunk-by-chunk so they never fully load into memory — Justin's own answer to the
  "global scale" problem, independent of (not the same as) Rich's `swy_global` tiling/batching
  approach. Different author, different library — don't conflate the two NatCap-adjacent toolchains.
- **ProjectFlow**: workflow engine, explicitly modeled on Rich's `taskgraph` but diverges — uses
  **file-existence checks** to decide whether to skip a task (vs. `taskgraph`'s content-hash
  caching), and `p.get_path()` does cascading resolution (project dir → base_data → cloud bucket).
- **The prescribed per-service convention** (the part actually worth adopting): every service gets
  its own directory with exactly `run_<service>.py`, `<service>_tasks.py`,
  `<service>_functions.py`, increasingly a `<service>_initialization.py`. Canonical shape:
  `build_task_tree(p)` (declares the DAG via `p.add_task(..., parent=...)`) + `run_project(p)`
  (fixed config) + `if __name__=='__main__':` guard (variant-specific config). `functions.py` is
  terminal domain logic; a `utils.py`, when present, is explicitly a "promotion queue" into
  Hazelbean once a second project needs the same code.
- **The naming spec ("EE Spec")**: `id`/`index`/`label`/`name`/`description` as a five-tier ladder,
  singular directory names (`output/` not `outputs/`), `lon` never `long`, 160-char lines,
  Google-style docstrings, UTF-8-sig CSV I/O, and a `*_ref_path` (unresolved) vs. `*_path`
  (resolved) distinction.
- **Git workflow**: Git Flow (`develop` integration branch, `main` for releases, PR review + local
  test-run required before merge).
- **Testing story — CORRECTED 2026-09-08**: the original pass, reading `main`, found no pytest
  suite anywhere and concluded the docs' pytest + `manual_t_*.py` marker convention was aspirational.
  Wrong: on `develop`, **22 of the 23 real service directories** (every one but the
  `example_service` template) ship a `test_<service>.py`, and the ones read in full
  (`test_coastal_protection.py`, 207 lines / 13 tests) are genuine, substantive unit tests —
  hand-built fixture tables, `tmp_path`-based reader tests, edge cases named and asserted
  (split-country handling, missing-vs-zero semantics, deflator spans) — not stubs or smoke tests.
  What's still true: no `conftest.py`, no CI workflow wiring pytest in (`.github/` holds only an
  issue template), and the widely-shared `global_invest/utilities.py` (1875 lines, used by every
  service) has no test file of its own — it's only exercised indirectly through individual
  services' tests. Execution wasn't attempted in this pass (the suite imports `hazelbean`, which
  needs a GDAL-backed environment not set up in this research clone) — the verification here is a
  full read of the test file's content and assertions, not a pytest run.

## What the actual code looks like — CORRECTED 2026-09-08, re-read on `develop`

The original pass read `pollination/`, `coastal_protection/`, `terrestrial_carbon_tasks.py` and
`example_service/` on `main` and called maturity "genuinely bimodal." That read is retracted below.
This pass re-read `pollination/` and `coastal_protection/` in full on `develop`, read
`example_service/` (still the canonical template, unchanged in spirit), and read `crop_provision/`,
`livestock_provision/`, `timber_provision/`, `erosion/` and `terrestrial_carbon/` in depth for the
first time, plus a structural survey (file sizes, raster usage, test presence) of all 24 service
directories.

**Corrected finding: maturity is not bimodal — it's consistently substantive.** Every directory
surveyed, including the smallest ones, is real working science, not a stub:

- **`coastal_protection/`**: entirely rewritten since `main`. No trace of the old dead-code bug —
  `coastal_protection_functions.py` is now a clean, documented, pure-function module (mangrove
  value computed from area × price with an explicit note on why it's computed rather than read off
  the workbook; coral-reef value carried to the base year via a compounded World Bank GDP
  deflator), backed by 13 real unit tests. No hardcoded personal paths found anywhere in
  `run_coastal_protection.py` or `coastal_protection_initialize.py` — that finding doesn't hold on
  `develop` either.
- **`pollination/`**: also entirely rewritten. No `numpy.bool`, no undefined module-level constants,
  no stray `'fix' # TODOO` string, no `README.md` lab notebook (removed). In its place: real 300 m
  gridded pollination-sufficiency rasters (an elliptical foraging-radius kernel convolved over
  agricultural pixels, resampled to 5 km) feeding both the country-level GEP valuation and a
  per-scenario economic shock pipeline — genuinely spatially-explicit intermediate computation, not
  present in the `main`-era code. (Two new, real bugs were found here on this pass — see the
  "Bug candidates" list below — but they're unrelated to anything flagged previously.)
- **`crop_provision/` and `livestock_provision/`** (unread on `main`, flagged then as an open
  question): both are pure-pandas FAOSTAT valuations (gross production value × a Changing Wealth of
  Nations land-rental-rate coefficient), no raster component. **But they do produce a genuine
  multi-year time series** — `gep_by_country_year` and `gep_by_year` span FAOSTAT's full 1961–2022
  range at country resolution, not just the single base-year snapshot the old comparison table
  assumed. `livestock_provision` additionally computes a GLEAM-3-derived "ecosystem feed share"
  (`feed_lambda_by_country`) as a second, explicitly-flagged-as-provisional attribution factor
  alongside the rental rate. Answers the outreach-prep question below: no spatial/gridded layer in
  either, but "no time-series logic found" was wrong.
- **`timber_provision/` and `erosion/`** (new reads, chosen for size/substance): both run real
  raster science at fine resolution — timber sums a 10-arcsecond net-forestry-return raster
  (verified bit-exact against a committed reference raster on 265M+ pixels, per the module's own
  docstring) into country zones; erosion runs InVEST's actual SDR (sediment delivery ratio) model
  plus watershed repair, DEM handling and upstream-prevention-share accumulation
  (`erosion_tasks.py` is 155KB, the largest task file in the repo). Sediment retention is one of
  global_NCP's own five services — worth knowing this exists in enough depth to eventually compare
  numbers, though that comparison wasn't attempted here.
- Raster/spatial computation of some kind (not just country CSVs) appears in at least 10 of the 24
  services on a structural grep (`pollination`, `terrestrial_carbon`, `flood`, `landslide_mitigation`,
  `erosion`, `ntfp`, `recreation`, `coastal_carbon`, `timber_provision`, `stormwater`) — this was
  invisible from the `main`-era 2-directory read.
- **The consistent pattern across every service read**: raster/fine-resolution work, where it
  exists, is always an intermediate step feeding a country- or r264-region-level aggregate. **No
  service, on `develop`, publishes a gridded/hotspot map as its primary result** — the terminal
  `p.results[...]['gep_by_country_base_year']` artifact is a country (or r264 sub-country) table or
  choropleth in every service read, pollination and terrestrial_carbon included. This refines,
  rather than overturns, the original "no fine grid in services read" comparison-table row: fine
  grids are common as internal machinery, absent as a deliverable.
- Git history on `develop`: two people account for nearly all commits (Chiara Colesanti Senni ~682,
  Justin Johnson ~59 combined across two author identities), plus Yanxu Long and Marta Sylla with a
  handful each — a small, but on this branch clearly active and iterating, team. Recent commit
  messages read as a deliberate hardening pass, not "in-motion" chaos: e.g. "pollination: the last
  constants that a CSV row already spelled," "flood: the same class of bug, found by asking rather
  than by being told," "erosion drops its env banner" — commit messages that describe specific
  correctness fixes, several in the exact same categories (unbound variables, hardcoded constants
  that duplicate a config row) as the two new bugs found in this pass. `pyproject.toml` still claims
  `"Development Status :: 5 - Production/Stable"`, still generous for a repo mid hardening-pass, but
  the code substance is far closer to matching it than the `main`-era read suggested.
- **What every service actually publishes as its headline number**: still one CSV,
  `gep_by_country_base_year.csv` — a base-year (2019 in every service checked) GEP (Gross Ecosystem
  Product) valuation per country. That part of the original finding holds. What's added on top,
  inconsistently used but real: multi-year country panels (crop/livestock provision), fine-grained
  intermediate rasters (pollination, terrestrial_carbon, timber, erosion, flood, and others), and
  per-income-group/region/continent/subregion breakout tables (`utilities.gep_summary_tables`,
  used by 13 of the 24 services' results pages) — see the contribution candidate below, which grew
  directly out of reading that last piece.

## The actual comparison

**CORRECTED 2026-09-08**: two rows below were wrong on the `main`-based read and are fixed here;
the rest were re-checked against `develop` and hold.

| Dimension | global_NCP (this project) | global_invest_dev |
|---|---|---|
| Core question | Where/how much did ES *change* 1992→2020, who's exposed, attributable to LCC? | What is the current (2019) economic *value* of a service, by country |
| Temporal structure | Bitemporal change detection is the whole point | **CORRECTED**: not a single-year snapshot everywhere — `crop_provision`/`livestock_provision` carry a genuine country-level annual panel, 1961–2022 (FAOSTAT's full range). Still no bitemporal *change-detection* logic anywhere: the panel is a continuous series driven by yearly source data, not a before/after LCC-attributed comparison. |
| Spatial unit | 1.5M-cell IUCN AOO 10km equal-area grid + multi-dimensional "squash" grouping | Country/r264-region polygons for every published result. **CORRECTED**: "no fine grid in services read" was an artifact of reading 2 of 24 directories — real fine-resolution rasters exist internally in ~10 of 24 services (pollination: 300m/5km; timber_provision: 10 arcsec; terrestrial_carbon, erosion, flood, landslide_mitigation and others also raster-based) — but always as intermediate machinery, never as the final published deliverable. |
| Zonal stats | Hybrid: `exactextract` (simple grid) + rasterized `zonal_stats_toolkit` (complex multipolygons) — explicit reasoning: GEOS memory leaks | Confirmed on `develop`: single shared engine, `hb.zonal_statistics_flex`, wrapped once in `utilities.summarize_raster_by_region` and reused by every service that aggregates a raster to regions. Because their finest zonal unit is a ~250–264-polygon country/region layer (not a multi-million-cell grid), a second zonal *pass* per grouping dimension is never needed — see the contribution-candidate note below on why global_NCP's "squash" idea doesn't transplant literally. |
| Orchestration | `ecoshard.taskgraph` (content-hash caching) + R/Quarto notebooks as the actual analytical pipeline | `ProjectFlow` (file-existence caching); `.qmd` used only for final rendering |
| Config governance | Centralized `R/service_config.R` — incident-driven (3 files drifted silently before this existed) | No equivalent found; definitions hardcoded per-file |
| Socioeconomic/attribution layer | Full layer: KS tests, Cliff's Delta, population exposure/multiplier, LCC attribution | None found in any service directory |
| Environment reproducibility | Docker image pinned, documented gotchas | No hardcoded personal paths found on `develop` (the one instance previously flagged, in `coastal_protection`, doesn't exist in the rewritten module) |
| Test coverage | R-native tests + validation notebooks | **CORRECTED**: real, substantive pytest suites exist — 22 of 23 real service directories carry a `test_<service>.py` (confirmed by reading `test_coastal_protection.py` in full: 13 genuine unit tests). No CI wiring found, and the shared `utilities.py` (1875 lines, used by 13+ services) has no test file of its own. |

**The precise relationship**: global_invest_dev is a lower layer — closer to what feeds *into* a
project like this one (raw biophysical/valuation outputs) than a competing implementation of what
this project actually does. Both share Chaplin-Kramer et al. (2019) as a foundational reference
without either being downstream of the other's code.

## Verdict

**Adopt selectively. Do not migrate wholesale. Real things worth proposing back, not just adopting.**

**Worth adopting (cheap, low-risk, do this on this branch):**
1. The `run_/tasks_/functions_` file split + `build_task_tree()`/`run_project()`/`__main__`-guard
   convention, for any **new** Python script going forward. `Python_scripts/` is currently a flat
   directory without this discipline.
2. The EE Spec naming ladder (`id`/`label`/`name`/`description`, `*_ref_path` vs `*_path`, singular
   directory names) — adoptable piecemeal, doesn't conflict with anything already established here.

**Not worth adopting:**
- Wholesale migration to `ProjectFlow`/`Hazelbean`. No functional gain over the existing
  `taskgraph`+Docker setup, and there's no path to running the R-native statistical machinery
  (`R/get_hotspots.R`, KS tests, Cliff's Delta) inside it — would mean abandoning R entirely.
- Copying `pollination`/`coastal_protection` code directly. **CORRECTED**: not because of the bugs
  originally cited — those don't exist on `develop`, both modules were rewritten — but there's
  still no reason to copy rather than adopt-the-convention-and-write-fresh, since the two projects'
  actual science needs don't overlap (see the comparison table).

### Contribution candidate — the primary first-PR target (added 2026-09-08, supersedes the earlier bug-first plan)

The original plan was "first PR = fix a known bug." Both known bugs turned out to be already fixed
on `develop` (see the correction note at the top of this file), which is itself informative: a
2-person-doing-most-of-the-work team that reads recent `develop` commit messages like "the same
class of bug, found by asking rather than by being told" is evidently fast at catching exactly this
category of mechanical error. Competing on "found a bug you missed" against a team actively hunting
the same bugs is a worse opening move than **bringing a capability they don't have and have said,
in their own code comments, that they want.**

**The literal "squash" idea (one `exactextract` pass, simultaneous multi-dimensional
`pandas.groupby()`) does not transplant as-is** — checked concretely, not assumed. global_NCP's
version earns its keep because the *zonal extraction itself* is the expensive step (1.5M grid
cells), so doing it once and grouping many ways afterward saves real work. global_invest_dev's
zonal extraction already only happens once per service (raster → ~250–264 country/r264 polygons via
`hb.zonal_statistics_flex`, wrapped in `utilities.summarize_raster_by_region`); everything past that
point is a `pandas.groupby()` over an already-tiny (≤264-row) country table, which costs nothing
extra regardless of how many dimensions you group by. There is no repeated expensive pass to
collapse into one.

**What the code visibly wants instead — verified by reading, not inferred:**
`global_invest/utilities.py:918` (`gep_summary_tables`) exists specifically because, per its own
docstring, "a GEP account is read by region and by income group at least as often as by country, so
a page that shows only the country table is missing the view most people open it for." It groups the
country table by each of `('income_grp', 'region_un', 'continent', 'subregion')` **one dimension at
a time**, writing four separate two-column CSVs (`gep_by_<grouping>_base_year_table.csv`), each
carrying only the grouping key and a single value column. This exact call —
`utilities.gep_summary_tables(df, map_value_column, utilities.report_dir(), log=hb.log)` — is
copy-pasted verbatim (same call, same preceding comment) into **13 of the 24 services'
`_results.qmd` files**. `coastal_protection_results.qmd` (rewritten more recently, apparently
without knowledge of the shared utility) independently **hand-rolls the identical four-single-
dimension-groupby pattern** at lines 93–110 instead of calling it — direct evidence the need is
real and the current utility doesn't fully satisfy it even where people reach for it. Two concrete
gaps in the current implementation: (1) no cross-tab — nothing anywhere in the 24 `.qmd` files
groups by more than one dimension at once (e.g. income group *within* region), confirmed by
grepping every `.qmd` for a multi-column `groupby`; (2) only one value column at a time, so a
multi-component service like `coastal_protection` (mangrove + coral-reef + total) or
`crop_provision` (per-crop breakdown) can't get its components broken out per group without calling
the function once per component and re-merging by hand.

**Proposed PR, sized like a first external contribution should be**: a small, additive function
alongside `gep_summary_tables` in `global_invest/utilities.py` — e.g. `gep_summary_crosstabs(df,
value_columns, out_dir, dimensions=GEP_SUMMARY_GROUPINGS, combine=False, log=None)` — that (a)
accepts a list of value columns instead of one, so every component a service computes gets grouped
in the same pass; (b) optionally accepts dimension *combinations* (e.g. `[('income_grp',),
('region_un',), ('income_grp', 'region_un')]`) for genuine cross-tabs, each still just one
`df.groupby(dims)[value_columns].sum()` call; (c) adds a `n_countries` count column per group,
surfacing a fact the existing docstring already cares about ("fire_protection covers 161 countries
and does not reach every income group") but that no current output actually shows. Purely additive
— doesn't touch any of the 24 services' task files, existing `gep_summary_tables` calls keep working
unchanged, and it's testable with plain pandas fixtures (no `hazelbean`/GDAL environment needed,
unlike most of this codebase) — the same reason `utilities.py` has no test file yet would make a
first `test_utilities.py` a natural, low-controversy add alongside it. This is offered as the
concrete shape of a PR, not a final API — the actual PR would be scoped in conversation with
whichever of Justin/Chiara reviews it.

A parallel, secondary observation in the same vein: `terrestrial_carbon_tasks.py`'s
`stack_layers_summary` (line 18) is a hand-written, well-tested, streaming block-wise raster
summarizer grouped by **two** categorical rasters simultaneously plus a value raster — the actual
raster-level analog of the "squash" idea, and closer to what global_NCP's grid-cell version does.
It currently has exactly one caller (itself) and isn't promoted to `utilities.py`; `utilities.py`'s
own stated convention is to promote a helper "on its second caller" (see `summarize_raster_by_region`'s
docstring). Generalizing it to N categorical layers and promoting it would be a second, slightly
larger PR-shaped idea, but it's more speculative — no second caller was found (as opposed to the
`gep_summary_tables` gap, which has 14 live sites already reaching for it) — so it's a good
conversation topic before or alongside a PR rather than a first PR on its own.

### Bug candidates — secondary due diligence, not the headline (re-verified against `develop`)

The two originally-cited bugs are dead (see the correction note at the top). In their place, two
newly-verified, independent, self-contained bugs, both in `global_invest/pollination/pollination_tasks.py`,
both in **live, currently-wired task paths** (not dead code), neither covered by `test_pollination.py`
(grepped for the function names — no hits):

1. **`baseline_denominator` (lines 108–114) and `scenario_diff_raster` (lines 117–135) reference
   `p` without it being a parameter, a global, or otherwise in scope.** Both functions read
   `p.pollination_shock_baseline_label` (and, in `scenario_diff_raster`, other `p.*` attributes) but
   are defined as `def baseline_denominator(cfg, baseline_lulc_path, target_year):` and
   `def scenario_diff_raster(cfg, scenario, lulc_path, baseline_lulc_path, target_year):` — no `p`
   parameter. They're called from `pollination_shock(p)` (the live per-scenario ES-shock task,
   wired into the task tree at `pollination_initialize.py:88`) at lines 1847 and 1856, **without
   `p` being passed in** — Python functions don't inherit a caller's locals, so this is a plain
   `NameError: name 'p' is not defined` the moment `pollination_shock` actually runs. Notably, the
   very next line inside `pollination_shock` (1826) carries a comment — "`es_shock_scenarios =
   list(p.es_shock_scenarios)      # was read unbound: this task raised NameError`" — showing this
   exact task has already had this exact class of bug found and fixed once; this pass is a second,
   still-live instance the fix missed. **Fix**: add `p` as a parameter to both functions and pass it
   at the two call sites (or pass the two or three specific attributes each function actually needs,
   which would be the tidier fix and matches the module's own stated preference for passing settings
   objects rather than `p` into pure functions elsewhere).
2. **`_merge_production_value` (lines 637–728) references `outdir` at line 721
   (`os.makedirs(outdir, exist_ok=True)`), which is not a parameter of the function (`prod,
   price_country, price_subregion, price_region, price_world`), not assigned anywhere inside it, and
   not a module-level global** (grepped the whole file for `outdir` — every other occurrence is a
   parameter local to a *different* function). Called from `run_fao_values` (line ~755), which is
   called from the live, registered `fao_median_prices` task (`pollination_initialize.py:23`) —
   crashes with `NameError: name 'outdir' is not defined` as soon as that task runs past the FAOSTAT
   read/merge step. The function's own docstring effectively confirms this is leftover cruft from a
   refactor: "Returns the frame. It used to take an output directory and return a path, which put
   file writing inside what reads as arithmetic and made the fallback untestable without a disk." —
   i.e. `outdir` was a real parameter that got removed when the function was made pure, and this one
   `os.makedirs` line (which does nothing useful even if it worked — the function doesn't write
   anything) was missed. **Fix**: delete the line; nothing downstream depends on it, and the actual
   directory creation already happens in `run_fao_values`'s own `_save_price_outputs`/`save_csv`
   calls.

Both are small, mechanical, low-controversy, verifiable by reading (not by running — this pass
didn't have a working `hazelbean`/GDAL environment to execute them, but the failure mode follows
directly from Python scoping rules and needs no environment to confirm), and reasonable to mention
to Justin/Chiara alongside the contribution PR even though they're not the headline.

**Caveat on scope**: even after this correction pass, only 7 of 24 service directories were read in
full depth (`example_service`, `pollination`, `coastal_protection`, `crop_provision`,
`livestock_provision`, `timber_provision`, `erosion`), plus `terrestrial_carbon` re-checked at the
`stack_layers_summary`/task-tree level. The other 16 were surveyed structurally (file sizes, raster
usage, test/convention presence) but not read function-by-function. Nothing found there contradicts
the "consistently substantive" read above, but a service-specific claim about any of the other 16
should be re-verified before being relied on.

## Outreach prep — what to actually ask Justin (once the meeting happens)

- **CORRECTED, now answered by reading rather than needing to ask**: `crop_provision` and
  `livestock_provision` do NOT produce gridded/spatially-explicit output — both are pure country-
  level FAOSTAT valuations. They do carry a genuine multi-year (1961–2022) country-level time
  series, which the original scoping caveat here flagged as an open question — now closed.
- Whether GEP accounting has any current/planned use for a change-detection/exposure layer, or is
  intentionally scoped to stay separate from that kind of work.
- What Derivative Projects (per his CLA, `global_invest_dev/contributor_license_agreement.md`)
  touch global ecosystem-service *hotspot* or *change* work specifically, as opposed to GEP
  valuation.
- What the CLA's co-authorship requirement looks like concretely in practice — worth naming
  directly that this project's own paper is exactly the kind of in-progress, pre-peer-review
  manuscript the CLA exists to protect against, rather than dancing around the symmetry.

**Don't ask** (already fully answered by his docs site or the code itself): the run/tasks/functions
pattern, the Hazelbean geospatial stack, Git Flow/PR process, or whether Global InVEST does
multi-year *change-detection* (checked directly on `develop` — it doesn't; `crop_provision`/
`livestock_provision` carry a multi-year country panel, but nothing bitemporal or LCC-attributed).
**CORRECTED**: don't ask "is there a testing framework" either, but for the opposite reason
originally written here — there demonstrably is one (22 of 23 services have a real pytest suite,
confirmed by reading one in full); what's still worth asking, since it isn't visible from the code
alone, is whether CI is planned or intentionally not set up yet.

## Decision (2026-09-08): the goal is a real, accepted PR to `global_invest_dev`, not just internal compatibility

User explicitly worked through whether global_NCP should become a formal "Derivative Project" of
the devstack (per the CLA's specific definition — see the outreach-prep section above) and
correctly concluded no: global_NCP doesn't derive from Justin's codebase, doing so on purpose would
mean the costly ProjectFlow/Hazelbean migration already ruled out above, and it risks an
unwarranted co-authorship implication (CLA §4) on a paper that has nothing to do with his code.
**Decision: keep global_NCP in `springinnovate` (Rich's org), stay on Rich's tooling, and pursue
the relationship with Justin through an actual accepted contribution instead of a formal
structural one.** Be transparent with Rich that this outreach is happening — naturally, not as a
secret — given the unexplained Rich/Justin history and that global_NCP lives in Rich's org.

**Why a real PR matters here, stated directly by the user**: this would be validation of real,
sustained effort ("this will validate all the time I have spent here"), a first-ever successfully
merged PR to another author's repo, and a permanent, visible footprint in a bigger shared codebase
— not just an internal refactor nobody outside this repo ever sees.

## Adoption work — the actual next-session checklist

Not started as of 2026-09-08. In rough order:

1. Read the EE Spec naming pages in full detail (`conventions.html`, `code_standards.html` on the
   devstack site) — this file's summary is a condensation, not a substitute for the primary source
   before actually renaming things.
2. Pick one real, currently-flat script in `Python_scripts/` as the pilot for the
   `run_/tasks_/functions_` restructure — don't attempt the whole directory at once.
3. Apply the naming spec (`*_ref_path` vs `*_path`, singular directory names, `id`/`label`/`name`
   ladder) to that pilot script and whatever config/output it touches.
4. **Test it actually runs end to end** before calling it done — the user's own stated concern
   going in: "apparently straightforward, but things never go exactly as planned... will require
   adjusting several parts of global_NCP and testing that everything runs." Budget real time for
   this, not just the restructuring itself.

**Pilot complete and verified, 2026-09-09.** `calculate_bitemporal_change.py` split into
`run_calculate_bitemporal_change.py` / `calculate_bitemporal_change_tasks.py` /
`calculate_bitemporal_change_functions.py`, Google-style docstrings added, `_path`-suffixed
variables, their `print ()`-with-space convention applied. Ran both the original and refactored
scripts in Docker against the real 924MB production input (`data/interim/10k_grid_services_base.gpkg`,
1,691,819 rows), then diffed all 20 output change columns directly via SQLite — **zero
mismatches**. Not just "it runs," confirmed byte-for-byte behavior-identical to the script it
replaced. First pytest tests added too (`Python_scripts_tests/`, pytest installed in the local
venv), 5 tests against the new pure-logic module, all passing. The naming-convention pilot is
done; the file-split pattern is now a proven, low-risk template for the rest of `Python_scripts/`
whenever there's appetite to apply it further — not urgent, no rush to do the rest now.

**Then, separately — the actual PR to `global_invest_dev`.** **CORRECTED 2026-09-08, strategy
revised**: the plan through the first research pass was "first PR = fix a known bug" against two
candidates found on `main`. Both turned out to already be fixed on the real branch (`develop`) —
evidence the small, active team there is already fast at catching exactly this class of mechanical
error, which makes "found a bug you missed" a weaker opening move than originally thought.

**Revised plan: lead with the contribution candidate, not a bug fix.** The added-capability idea
above (`gep_summary_crosstabs` / extending `gep_summary_tables` in `global_invest/utilities.py`) is
still small and self-contained the way a first external PR needs to be — a first-ever PR to
someone else's repo should still not be an architectural proposal — but it's a real capability add
grounded in the codebase's own stated want (its docstring) and already-duplicated need (14 call
sites reaching for the same single-dimension groupby, one of them by hand). That combination —
small, additive, wanted, and demonstrably non-trivial — is a better first move than either an
unsolicited architecture pitch or a bug fix that reads as "here's what's wrong with your code."
Target `develop` (not `main`, which has 0 commits `develop` lacks and exists only as the stale
release line) per their stated Git Flow.

The two newly-verified `pollination_tasks.py` bugs (NameError crashes in `baseline_denominator`/
`scenario_diff_raster` and in `_merge_production_value` — see "Bug candidates" above) are worth
mentioning alongside the contribution PR, or as a small separate fix-PR if that reads better in
context — genuine, verified, low-controversy — but are secondary, not the headline.

Practical note carried over from the original plan and still correct: **no push access** to
`NatCapTEEMs/global_invest_dev` was confirmed under the user's current SSH identity (a dry-run push
was denied), and there's no `CONTRIBUTING.md` in the repo, only `contributor_license_agreement.md`
— so the actual mechanics will be a personal GitHub fork + PR against `develop`, not a direct branch
push.

Once a small contribution like this is merged — the actual "permanent footprint" — that's the
credibility to then raise the bigger, more architectural ideas (config centralization, and a
genuine conversation about whether the terrestrial_carbon `stack_layers_summary` raster-level
grouping is worth generalizing) verbally, with a merged PR already behind you as proof of
good-faith, careful work.

5. Only after a real PR is at least submitted (merged not required to send the message, but
   stronger if it lands first): send the Justin message (already drafted, may want a small update
   once there's a specific PR to reference), referencing what was actually done, not what was
   planned.

## Future idea, not now — reorganizing global_NCP's own `analysis/*.qmd` files (2026-09-09)

Explicitly parked behind more urgent things (paper, SWY, the contribution PR) — captured here so
it isn't lost, not something to start on yet.

**The question**: inspired by Justin's per-service `method.qmd`/`results.qmd` separation, does
global_NCP's own `analysis/` folder need reorganizing? User's own read going in: the current qmds
are "the result of the process... not necessarily the most logical/efficient/clear for someone who
was not involved."

**Why a literal copy of Justin's pattern doesn't apply**: his decomposition axis is *per service*
(24 independent, parallel units). global_NCP's real axis is *per pipeline stage*, sequential and
stateful — every stage reads the previous stage's output off the same shared grid
(`10k_change_calc.gpkg`). There's no equivalent of "one service, one qmd" here; forcing that shape
would be wrong for this architecture, not just difficult.

**What does transfer, grounded in actually reading the current qmds (2026-09-09), not just the
runbook's prose description of them**:
- `hotspot_extraction.qmd` is the clearest candidate — it currently does at least four distinct
  things in one 1300+-line file: load/cache `plt_long`, run the core hotspot-extraction workflow
  (the actual GPKG-producing computation), render hotspot boxplots, and a whole separate
  "Attribution to Drivers of Change" section that conceptually belongs with the LC-driver/
  attribution-gap lineage (step 5), not with hotspot extraction. The team has *already* validated
  splitting computation from presentation once, organically: the boxplot logic was pulled out into
  standalone `scripts/mapping/make_hotspot_boxplots.R` / `run_hotspot_boxplots.R` specifically
  because re-rendering the whole notebook just to tweak plot styling was too slow — a real, lived
  precedent for exactly the method/results separation Justin's convention formalizes.
- `hotspot_synthesis.qmd` is already internally organized into four clearly-labeled Parts
  (Intensity/Share, Multi-service Overlap, Population Exposure, Regional Subsets Export) — it just
  hasn't been split into files yet. `docs/runbook.md`'s own partial-re-run table already treats
  "Regional Subsets Export" as independently re-runnable, which is the same signal that made the
  boxplot extraction worth doing.
- The `LC_change_preparation.qmd` -> `LC_change_rasters.qmd` -> `LC_change.qmd` ->
  `LC_change_granular.qmd` -> `viz_granular_lcc.qmd` lineage is, by contrast, already a reasonably
  clean example of staged separation — worth pointing to as the existing positive case, not
  something that needs fixing.
- `process_data.qmd` carries a real, flagged fragility worth isolating regardless of file-count
  decisions: a "[MANUAL UPDATE REQUIRED IF ADDING MULTIPLE YEARS]" block admitting the bitemporal
  (T0/T1-only) assumption is hardcoded and needs hand-editing (commenting out lines) to extend —
  a good candidate to pull into its own well-tested function the same way
  `calculate_bitemporal_change.py` just was, rather than left as inline notebook logic.

**The actual transferable principle, restated precisely**: not "one file per service" (doesn't
apply), but Justin's *computation vs. presentation* separation, applied per stage instead of per
service, targeted at the seams the team has already half-discovered on its own (the boxplot
extraction, the runbook's partial-re-run table) rather than a blanket rewrite.

Related: [[project_docs_cleanup_deferred]] — this is a specific, concrete instance of that same
deferred bucket, not a separate initiative.
