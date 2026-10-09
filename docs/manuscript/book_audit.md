# Book update: audit and plan (2026-10-09)

Decisions (user, 10-09): the book is the **long-form companion** to the paper (same three questions
and numbers, plus what the paper cannot hold); the drivers chapter is **shortened and moved after
WHO**; work goes **audit first, then one chapter at a time** on a new branch (`book-update`), render
after each, user reviews before the next.

Source of truth for every number: the paper (`paper_draft_5service.qmd`, version sent 10-05 plus
10-06/10-07/10-09 edits) and the tables it cites. The deck (`docs/presentations/presentation.qmd`)
has the newest framing and four analyses that are not in the paper yet (marked NEW below).

## Proposed structure

| # | Chapter | Status now | Plan |
|---|---|---|---|
| 0 | `index.qmd` Executive summary + glossary | 4-question framing, old numbers, "Attribution Gap" headline | Rewrite from the paper's abstract; glossary: drop Attribution Gap / Serviceshed Multiplier wording, add relative prevalence, local vs connected beneficiaries |
| 1 | `01-problem.qmd` The challenge | WHAT/WHERE/WHO/WHY, 8-service list | Three questions; sources as on deck slide 3 (MEA, IPBES, Brauman, Chaplin-Kramer 2019/2023, Kim, Peng); trim the "users" list |
| 2 | `02-methods.qmd` Methods | "Eight services", old socioeconomic sources, attribution section, process callouts | Align with paper Methods; cell universe (1,372,621); SPC rationale; metric choice; coastal risk = unitless 1-5 index (not wave energy); socioeconomic sources (GHS-POP, Kummu 2025, Sherman 2026, Gini placeholder, Mehrabi, LandScan 2023); move land cover methods to the drivers chapter; FOR BECKY callouts -> one "open questions" box |
| 3 | `03-global-patterns-WHAT.qmd` WHAT | Figures from `../../zonal_stats_toolkit/` (outside repo), "[Note for final draft]" placeholder | Paper's figures (`outputs/plots/output_plots_diff/*_combined_diffs.png`, change maps); write the actual findings; add NEW pollination vs cropland (deck slide 19) and coastal windows chosen by population (slide 20) |
| 4 | `04-hotspot-WHERE.qmd` WHERE | Old numbers (Mangroves 4.5×, 1.8×), coverage charts, hotness distributions, interactive tables on the old area-stats table | Current prevalence (LAC 1.9, EAP 1.3); frequency vs severity (paper 10-06); NEW SPC vs absolute profile (slide 25: overlap, baselines, people, map); keep the interactive tables but rebuilt on `hotspot_area_stats.csv` (10-05 rebuild); drop "Updated 2026-09-08" process notes |
| 5 | `06-hotspot-WHO.qmd` WHO (becomes ch. 5) | 3.1B / 7.6B (June run), 6 missing exposure figures, directionality chart | 2.4B local residents / 7.4B connected (07-29 run, LandScan 2023), `exposure_by_overlap_bars.png`, income split; KS/Cliff's δ with the plain reading (deck slides 14 and 28); Gini placeholder |
| 6 | `05-drivers-WHY.qmd` (becomes ch. 6, shortened) | Full "monitoring gap" chapter, risk ratios, scatterplots | "What drives the change: a first look and next steps": 63% / 37%, null model 9% -> 25%, why cell-level overlay cannot follow flow paths, planned tests; co-occurrence only |
| 7 | `07-regional-profiles.qmd` | 8-service references (13), computed plots | Keep as the "explore by geography" chapter; update services and data source; point to the explorer |
| 8 | NEW `using-the-pipeline.qmd` | (scattered in 08) | The pipeline as a tool: every choice is an input, the change explorer, how to rerun (from `docs/how_to_extend.md` and the runbook) |
| 9 | `08-conclusions.qmd` | WHERE/WHO/WHY summary, long policy lists, old numbers | Synthesis table as in the deck; shorter implications; limitations as in the paper |
| A | `09-annex-methodology.qmd` | Grid attrition 1.5M -> 1.3M, 10 km signed bars | Update cell universe (`docs/cell_universe.md`); keep the Path B aggregation figures as the MAUP illustration |

## Numbers to replace (old -> current)

- Hotspot cells: 189,932 = 13.8% of 1,372,621 evaluated cells (not "~1.3M", not "250,000").
- Regional prevalence (land services): LAC 1.9, EAP 1.3, SSA 1.0, ECA / NA / MENA ~0.6 (old: 1.58, 1.21, 0.81...).
- Income: lower-middle vs high-income OECD ~2.3× per unit area (old: 1.8×); 15.6% of land, 21% of hotspot cells.
- Biomes: Tropical & Subtropical Moist Broadleaf 2.3 (sediment 3.0), coniferous 2.0, mangroves 1.8 (old: mangroves 4.5×; the coastal 15× was a denominator artefact).
- Exposure: 2,367 M local residents, 7,383 M connected (92.5% of LandScan 7,982 M), 5,949 M downstream, 7,164 M travel, ~3.1×; upper-middle most residents (919 M), lower-middle most connected (2,675 M), low-income ~13% (old: 3.1B / 7.6B / 97% / 12.4%).
- Socioeconomic tests: 24 of 25 (old: 39 of 40 in places); δ ranges as in the paper.
- Drivers: 63.3% / 36.7%; null model 9% (global) -> 25% (0.5° blocks). Check the risk ratio 7.3 / odds ratio 10.9 against the paper's Supplement before keeping them.
- Global trends: N export +1.6%, sediment export +1.4%, nature access −8.7%, pollination +6.9%, coastal risk <0.1%.
- Socioeconomic sources: GDP Kummu et al. 2025 (not 2018); population GHS-POP R2023A (Schiavina et al. 2023).

## Broken or outside-repo figures

- Ch. 3: three `../../../../zonal_stats_toolkit/output_plots_diff/*.png` (outside the repo) -> `outputs/plots/output_plots_diff/`.
- Ch. WHO: `pop_exposure_income.png`, `pop_exposure_hdi.png`, `multiplier_effect.png`, three `exposure_multiplier_dumbbell_*.png` (archived June run) -> `outputs/plots/exposure_by_overlap_bars.png` + an income-group exposure figure (`scripts/exposure_by_income.R`).
- Ch. WHERE and conclusions: `global_hotspot_count_heatmap_cap4_pct.png` has a stray line across the Arctic; regenerate it (or use the deck's redraw).
- Ch. WHO: `ks/directionality_cliffs_delta.png` was dropped from the paper.

## NEW analyses from the deck to bring in (not in the paper yet)

1. SPC vs absolute hotspots: overlap is the same both ways; SPC-only hotspots start 25-1,300× lower and hold 2-10× fewer people; map for sediment export (`scripts/mapping/make_deck_metric_overlap.R`). Candidate for the paper too.
2. Pollination vs cropland expansion per cell (`scripts/mapping/make_deck_pollination_cropland.R`); 58% of cells covered by the LC crosswalk; co-occurrence only.
3. Coastal windows chosen by population (`scripts/mapping/make_coastal_risk_zooms.R` rule; approximate ranking).
4. The change explorer (`scripts/dashboard/`).

## Style for the rewrite

Long-form but plain: findings stated directly, no process narration ("Updated 2026-09-08", "Fixed..."),
no "Why this matters:" framing, no em dashes in prose, terminology as in CLAUDE.md. Internal open
questions go in one callout per chapter, not scattered.

## Order of work

1. Branch `book-update`; fix broken paths so the book renders at all (baseline render).
2. Chapters in reading order: index -> 1 -> 2 -> 3 -> 4 -> 5 (WHO) -> 6 (drivers) -> 7 -> 8 (pipeline, new) -> 9 (conclusions) -> annex.
3. After each: render, screenshot-check figures, user review.
