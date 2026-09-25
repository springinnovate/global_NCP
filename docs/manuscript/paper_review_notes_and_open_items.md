# Paper: author's own review notes and open items

Extracted 2026-09-23 from the italic-gray note/status/important blocks inside
`paper_draft_5service.qmd`. These are the author's own working notes and open questions embedded
in the draft — not Becky's PDF comments (those live in `paper_draft_5service.pdf` itself and in
`docs/HANDOFF.md`'s 2026-09-23 entry). Kept here so they're easy to find without hunting through
the qmd; the blocks themselves stay in the qmd too (wrapped `{.content-visible when-format="html"}`
— visible when rendering to HTML, suppressed from the shared docx export, not deleted).

Organized in the paper's own reading order.

## Top of document — Status banner

**Status (2026-09-03): reverted to export/risk framing** — Becky overrode Steve's retention/
protection suggestion, Path B rerun complete under the reverted definition. Nitrogen, sediment,
coastal are export/risk *residuals* again (the pre-2026-08-28 scheme), not retention/protection
amounts. `R/service_config.R` flipped back; full Path B chain rerun end to end; every dependent
map/figure regenerated (189,932 hotspot cells, 63.3% attribution gap).

Still thin: Section 4.1 narrative hasn't had the "substantially expand" pass Becky/SP19 asked for.
Mangroves absolute-vs-relative decoupling explanation is a finding, not yet a sentence in the paper.

Still blocked: Figure 2 + siblings need Path A pixel-level rasters from Rich. New risk/service map
grid blocked on a pollination-sufficiency data question sent to Becky/Justin 2026-08-21, unanswered.
Per-service model-mechanics detail (Nature Access, Pollination, Coastal Risk) still thin — legitimate
to write without Becky's input (public InVEST methodology) but not done. Aggregate hotspot rasters
toward Rich still reflect the old retention/protection set, need regenerating — no rush.

Still genuinely open, needs a live conversation with Becky: sediment retention vs. retention-ratio
tradeoffs; "directionality ambiguity" in the change signal (a pixel's change can reflect upstream/
downstream redistribution, not a clean local signal).

## Abstract

Updated 2026-09-03 following the reversion. All four headline figures are real, from the reverted
Path B rerun. Multiplier-effect figure is Rich's existing beneficiary-buffer output, confirmed
current (built against export/risk, which the reversal restores exactly — no rerun needed).

## Methods — Biophysical Modeling of ES

**Model run provenance — real open question, not yet answered:** the 300m InVEST rasters used here
are not generic outputs — produced for a specific project, likely TNC partnership. Need: (1) exact
provenance of the 1992/2020 rasters — same runs as Chaplin-Kramer et al. 2019, a direct extension,
or new runs? (2) who produced them, under what project, citable dataset/paper? (3) fixed or
era-specific climate inputs for 1992 vs. 2020 — **this also gates the Discussion's Climate Forcing
mechanism** (see below). Citation structure may need: InVEST software (Sharp et al. 2020) + specific
model run/dataset (TNC/NatCap citation TBD).

Updated 2026-09-03, reverted: export/risk-residual framing for nitrogen/sediment/coastal settled
again. All three already computed under this framing pre-detour, nothing needed re-deriving.

**Why export, not the retention amount — a real finding, not just Becky's preference:** during the
brief retention-amount framing, found USLE is not land-cover-independent, so an increase in
sediment retention *amount* can reflect rising erosion pressure from conversion, not improved
retention. Not an edge case: decomposing Brazil's total change in that amount, 95% traced to rising
USLE, only 5% to an actual export decline, concentrated in the Amazon (Tropical Moist Broadleaf
biome). Exporting the raw load directly sidesteps this. Nitrogen export/retention doesn't share
this vulnerability (fixed fertilizer-load scenario across years).

## Methods — Ancillary Datasets

**`ee_correspondence` table provenance — needs a real citation, not just attribution.** Shared
directly by Justin as a finished GeoPackage, no paper/DOI/version note. Possible lead, not
confirmed (2026-08-28): a data inventory spreadsheet lists a `gtap_invest_seals_2023_04_21` bucket
entry "Canonical countries (Justin)" pointing to `ee_r264_correspondence.gpkg`
(`https://storage.googleapis.com/gtap_invest_seals_2023_04_21/cartographic/ee/ee_r264_correspondence.gpkg`)
— name matches closely but may not be the current version; needs verifying with Justin before citing.

**Questions for Justin:** (1) citable reference — paper, Zenodo/figshare DOI, internal note, or how
to cite? (2) provenance — source datasets, build date/version, known limitations, enough for a
1-2 sentence methods description? (3) should the exact version used here be archived somewhere
citable (OSF/Zenodo), given it's currently just a file with no version control?

## Methods — Hotspot Identification

As of 2026-09-03: subsection reflects the reverted export/risk framing. Exclusion rationale
predates and survives the brief retention/protection detour unchanged.

## Methods — Spatial Attribution Analysis

**[FOR BECKY — methodological validation needed].** The co-occurrence design compares hotspots
from two fundamentally different variable types: modelled continuous ES provision (SPC) vs.
observed categorical land cover transitions (Pontius contingency matrix). The justification —
independently reducing both to binary ranked maps at an equivalent threshold before comparison —
seems defensible, but no confirmed precedent found in the ES attribution literature.

**Questions:** (1) published methodological basis for comparing ranked continuous-variable
intensities against ranked categorical-transition intensities? (conservation biology hotspot
overlap, spatial epidemiology co-occurrence, ES attribution literatures may have this). (2) should
this be validated against a null model (expected overlap % under spatial independence)? Currently
cites the ~5% random-chance baseline implicitly only. (3) who in Becky's network could advise —
may warrant a brief expert consultation before submission.

## Results — Global and Regional Trajectories (Figure 2 / biome_combined_diffs.png)

**Placeholder, 2026-09-03 — stale for a different reason now.** Path A (300m) product from
`zonal_stats_toolkit` (sibling repo). Already reflects export/risk service definitions — the
retention/protection detour never propagated here, so the reversal needs no change. Remaining
staleness: need fresher raw 300m rasters (USLE, sediment/nitrogen export+retention, 1992+2020) from
Rich to regenerate at current data quality.

## Results — Global Pattern of Change

Replaced 2026-08-20 per Becky/Steve's review. Previous version showed only biome-aggregated
choropleths, obscuring the pixel-level pattern. Region/income/biome breakdowns now apply only to
Figure 2 and Annex bar charts, not maps.

## Results — Population Exposure and Serviceshed Multiplier

Current as of 2026-09-03, confirmed. Beneficiary/serviceshed figures (7.6B connected, 3,065M
in-situ, 2.5×) come from Rich's downstream-hydrological and travel-time beneficiary buffer
pipeline, run against the export/risk hotspot definition — same definition this paper uses again.
No rerun needed since the reversal restores the exact framing Rich's pipeline was built against.
Treated as final, not placeholder.

## Discussion — Land Cover Monitoring Sufficiency

**Two open questions for Becky, both affect how this section should read:**

1. Currently stated conservatively ("consistent with the view that LC monitoring is insufficient").
   A stronger version would cite that land conversion monitoring is a widely-used ES-health proxy
   (IPBES 2019, REDD+ literature) to frame this as an explicit contribution against that prior —
   does Becky have a specific citation for that framing?
2. **Required before submission:** were the InVEST SDR/NDR runs for 1992 and 2020 performed with
   the same climate inputs, or era-specific ones (year-matched CHELSA/ERA5)? If fixed, the Climate
   Forcing mechanism in this section is weaker (both runs see the same R factor) and should be
   softened or removed. See `analysis/WORKLOG.md`, 2026-06-19 entry, for context. **Same open
   question as the Methods model-run-provenance note above — one real answer resolves both.**

## Annex — Spatial Attribution Gap

Moved to Supplement 2026-08-20 per Becky/Steve's review. Presented for comparison only, not causal
attribution — co-location doesn't establish causation, and where a change *happens* isn't
necessarily where it *accrues* downstream.

Numbers corrected 2026-07-24 (a many-to-one crosswalk-join bug was inflating cell counts — see
WORKLOG 2026-07-24). **Known follow-up, not yet done:** the same bug may still affect
`analysis/hotspot_extraction.qmd`'s per-service driver-overlap table and some driver-hotspot maps
in this section — re-verify before treating as final.

Reading the "Risk Ratio vs. Background" column: risk ratio = (overlap rate among ES hotspot cells)
÷ (overlap rate among background cells). 1.0 = no association; 7.3 (union row) = ES hotspot cells
7.3× more likely to sit inside the five-driver conversion union than a random non-hotspot cell.
Equivalent odds ratio: 10.9 (95% CI 10.81–11.08) — larger than risk ratio since the outcome (36.7%)
isn't rare. Risk ratio reported as primary effect size; odds ratio noted for readers expecting the
standard contingency-table statistic.
