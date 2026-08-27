# Becky/Steve feedback — status and action plan

**Context**: Becky's summary email (received 2026-08-20, referencing a 2026-07-09 meeting with
Steve) plus the full annotated PDF (`NCP decline hotspots SP BCK.pdf`, draft dated 2026-06-24,
Becky's [BC] and Steve's [SP] inline comments) were reviewed together against the current
working file, `docs/manuscript/paper_draft_5service.qmd`.

## The one thing that changes everything else

Per the file's own status banner (lines 27–46, added 2026-08-06): **only the Abstract has
actually been rewritten for 5 services.** Everything from Methods through the Annex is still
the *unmodified 8-service text* — the exact same content Becky and Steve reviewed in the PDF.
Their comments therefore apply directly, unedited, to what's currently in the file. This also
resolves what "replace 4.3 and 4.4 with the new beneficiaries analysis" most likely means: the
Abstract already has real 5-service beneficiary numbers (water-overlap: 8.1% of land, 3.1B
people downstream; access-overlap: 9.3%, 7.1B people; combined: 3.5%, 4.1B people) that were
never written into the Results body — 4.3 (stale KS-diagnostics) and 4.4 (stale
population-exposure/multiplier) are almost certainly meant to be replaced by that. **Not yet
confirmed with Becky — see the open question below before committing to this reading.**

## Open question, blocking the rewrite — sent to Becky/Steve 2026-08-20

Steve's comment SP15 explicitly proposes the 5 services as *nature access, pollination, coastal
protection, nitrogen retention, sediment retention* (retention-ratio/protection framing). The
paper's actual current 5-service selection (in the Abstract right now) is *Nitrogen Export,
Sediment Export, Coastal Risk, Pollination, Nature Access* — export/risk framing instead. These
are materially different metrics (a retention ratio isolates landscape filtration efficiency; an
export value is the raw delivered load) and the choice affects every downstream figure, table,
and interpretation. Asked Becky (cc Steve) to confirm which framing is intended before any
rewrite proceeds — see `docs/manuscript/becky_reply_2026-08-20.md` for the draft sent.

**Found 2026-08-21, in a stale staging doc that was about to be deleted — the actual rationale for
the export/risk choice already exists, dated 2026-07-28** (`docs/paper_5service_methodology_staging.md`,
now retired, content migrated here):

> We exclude Nitrogen and Sediment Retention Ratios and the Coastal Risk Reduction Ratio from
> hotspot detection: export and retention of the same pollutant are not statistically independent,
> and an increasing retention ratio can indicate upstream degradation rather than local ecological
> improvement — a distinction that would require substantial additional explanation to interpret
> correctly and is out of scope for the main hotspot analysis. Retention ratios remain part of the
> broader change analysis, just not part of the hotspot *definition* itself.

This was written *after* Steve's PDF comment (drafted 2026-06-24) proposing the retention/protection
framing — someone already reasoned through why to override his suggestion, but that reasoning never
made it back to him or Becky. The email sent 2026-08-20 just asked "which did you mean?" without
this context.

**Follow-up sent 2026-08-24** (draft file deleted after sending, per this project's standing
practice — findings kept here, not in the draft). Two things were verified before sending, both
worth keeping as reference:

**Steve's exact wording, verbatim from the annotated `.docx`'s comments.xml** (the PDF's
annotations don't extract cleanly as text; the `.docx` version at
`OneDrive - World Wildlife Fund, Inc\Attachments\NCP decline hotspots SP BCK.docx` does):

- Comment, 2026-07-14 20:07 (flagging the redundancy): *"...3 of these 8 are different ways to
  express related concepts and are not independent services: coastal protection and coastal risk
  reduction ratio; nitrogen export and nitrogen retention ratio; sediment export and sediment
  retention ratio."*
- Comment, 2026-07-15 02:19 (the actual 5-service proposal): *"I would say you have five services
  modeled: nature access, pollination, coastal protection, nitrogen retention, and sediment
  retention."* — note "ratio" is explicit in the first comment, dropped in the second. Read as
  shorthand, not a change of metric, since neither nitrogen nor sediment has a separate non-ratio
  "retention" variable in the pipeline to shift to.

**"Coastal protection" doesn't exist as a computed variable — the paper's own Methods text was
wrong.** Checked `analysis_configs/c_protection_synth.yaml`: only two coastal variables are ever
computed, `C_Risk` and `C_Risk_Red_Ratio` (both derived from vector point data, rasterized — see
`Python_scripts/rasterize_coastal.py`). There is no wave-attenuation "coastal protection" raster
anywhere in the pipeline. The unmodified Methods text described one anyway (fixed 2026-08-24 in
`paper_draft_5service.qmd`'s Coastal Risk Mitigation bullet). So Steve's "coastal protection" was
almost certainly his shorthand for `C_Risk` — already what's in the 5-service Abstract — meaning
coastal was likely never an actual disagreement, only nitrogen/sediment retention is a live
question.

**Also worth knowing if this ever needs re-deriving**: nitrogen has a real standalone absolute
retention raster (`global_n_retention_ESAmar_*`, independently modeled, not derived) already
flowing through `Python_scripts/calculate_bitemporal_change.py`'s rename map — usable with no
upstream re-run if the absolute-amount reading turns out to be correct instead. Sediment has no
equivalent — `calculate_ratios.py` computes `sed_retention_ratio` directly as
`(USLE − sed_export) / USLE`, so the absolute quantity is mathematically implicit but was never
saved as its own raster; deriving it would need one small script addition, not an InVEST re-run.

## Still unanswered in the paper itself

The `[QUESTION FOR BECKY — required before submission]` on p.26 of the PDF (5.3/Discussion, the
attribution-gap section): were the 1992 and 2020 InVEST SDR/NDR runs performed with the same
climate inputs, or era-specific ones? This gates whether the "climate forcing" mechanism in the
attribution-gap discussion is defensible as written. Not bundled into the reply draft — flag to
the user whether it should be added.

## Section-by-section plan

Merges Becky's email, both reviewers' inline PDF comments, and what's already true in the
current Abstract. "BC"/"SP" numbers refer to the PDF's comment markers.

| Section | Action | Source |
|---|---|---|
| Title/subtitle | Drop "Time Series" — it's not a time series, it's change between two points | BC1 |
| Abstract | Reframe from "poorly characterized" to what we contribute; mark `{.unnumbered}` so Introduction becomes §1 | SP4, SP3 |
| Introduction | Rewrite around What/Where/Who/Why as the paper's actual contributions, not gaps in the literature; drop subheadings (section too short to need them); "landscapes don't have to be natural" to provide services; move the GEP/SEEA/IPBES policy paragraph to Discussion; add a lit-review sentence on what's actually novel (no prior quantitative global trends analysis) | SP5, SP6, SP7, SP8 |
| — WHO research question wording | SP9 flagged the original as hard to parse — current wording ("how does the scale of exposure change when downstream and travel-connected beneficiaries are included") already reads more clearly than the PDF's version. Likely already resolved; re-check against SP9's specific complaint before assuming so. | SP9 |
| Methods | Add real per-service biophysical modeling detail — currently only sediment/nitrogen retention ratios get real explanation, the rest are thin; clarify what "spatial data extraction pipeline" means; simplify the exclusion-criteria sentence (currently hard to parse); define "localized urban footprints" | SP10, SP12, SP13, SP16 |
| 4.1 (Global/Regional Trajectories) | Substantially expand narrative before hotspots are introduced; add un-faceted, original/10km-resolution change maps (not broken down by zone) — flagged independently by both Becky's email and SP19, a strong signal; lead with global trends before regional, and emphasize absolute change over percentage change | Becky email, SP17, SP19 |
| Figure 2 (biome_combined_diffs) | Reorient to 5×2: one row per service, risk/damage on the left, service on the right. Zone breakdowns (biome, income group, world region — all three, not just biome) apply only to the graphs, not the maps. Currently "opaque" per Steve. | Becky email, SP18 |
| New 5×2 risk/service map grid | Pollination: change in production (left) vs. change in pollination sufficiency (right) — **blocked, needs Becky's sufficiency layer, not yet sent**. Nature access: change in # people within 1hr (left, already have this) vs. change in natural land cover loss/gain (right, need to build) | Becky email |
| 4.3 + 4.4 | Replace with the water/access/combined beneficiary analysis already computed and sitting in the Abstract — pending Becky's confirmation this is what she means | Becky email |
| 4.5 (Spatial Attribution Gap) | Move wholesale to the Supplement; the causal-vs-co-occurrence caveat language is already mostly present, just needs relocating along with its table/figures; resolve/remove the existing `[FLAG FOR BECKY]` callout since her email now supersedes it | Becky email |
| All figures | Add panel labels (a/b/c-style), not just legends — current numbering is confusing per Steve | Becky email |
| Mangroves | Investigate why mangroves don't show up in absolute-change figures despite being a hotspot biome in the relative-intensity figures — not yet looked at | Becky email (Troubleshooting) |

## Progress (2026-08-20, same session as the plan itself)

Completed, none of these depend on the service-framing answer:
- Title: dropped "Time Series" per BC1 → "Global Change in Ecosystem Services: Hotspots of Decline, Exposure, and Attribution"
- Abstract: marked `{.unnumbered}` (SP3, confirmed in rendered TOC — Introduction is now §1) and reframed opening from "poorly characterized" to a contribution statement (SP4)
- Introduction: subheadings removed (SP6), "natural landscapes" → "landscapes" (SP7), rewritten around What/Where/Who/Why as explicit contributions with a flagged-not-asserted novelty claim (SP5 — the "first quantitative global analysis" framing needs a real lit-review pass before it can be stated as fact, left as an inline `[FLAG]` rather than asserted), GEP/SEEA paragraph moved to Discussion → Geographic Clustering section (SP8)
- Methods: "localized urban footprints" clarified to explain what GHSL-BUILT actually measures (SP16). SP12/SP13 (define "spatial data extraction pipeline," simplify the exclusion-criteria sentence) turned out to already be resolved — that language isn't in the current file.
- 4.5 (Spatial Attribution Gap) moved wholesale to the Annex, retitled "Spatial Attribution Gap: Land Cover Co-occurrence Analysis," with an explicit "presented for comparison only, not causal attribution" framing sentence added per Becky's direction. The stale `[FLAG FOR BECKY — meeting 2026-07-09]` callout was updated to reflect that her 2026-08-20 email is the review that landed; the still-unconfirmed crosswalk-bug follow-up caveat was kept since there's no evidence it's been fixed. Discussion §5.3 now points to the Annex instead of assuming the analysis is still in Results.
- Mangroves: investigated, not a bug — see below.
- Rendered clean after every change (`quarto render docs/manuscript/paper_draft_5service.qmd --to html`), confirmed via TOC inspection that Abstract/Introduction numbering behaves as intended.

**Mangroves bug (Becky's "Troubleshooting" ask) — resolved as a finding, not a fix.** `compute_service_limits()` in `scripts/mapping/make_faceted_maps.R` confirms the absolute-change color scale is driven by total magnitude per biome. Mangroves are one of the smallest biomes globally by land area — even with an extreme per-area signal (already reported elsewhere in the paper: 15× overrepresentation in coastal-risk hotspots), their aggregate absolute total will always be small next to large biomes like tropical moist forest, and mangroves' thin, fragmented coastal geometry is genuinely hard to resolve on a global choropleth regardless of the value. This is the same absolute-vs-relative decoupling the paper's own 4.1 narrative already argues for other biomes — not a contradiction of it. Not fixed in code; needs a sentence added when 4.1 is rebuilt (see below), explicitly naming mangroves as the sharpest example of the paper's own decoupling argument.

**Also done (2026-08-20, later same session):** replaced the biome-faceted choropleth maps in
"Global and Regional Trajectories" with the un-faceted, pixel-level 10km global change map already
built today for the Sandra deck (`global_change_5panel_en.png`) — this directly answers both
Becky's email and SP19, who independently asked for the same thing ("if analysis is done at pixel
level then why not show that?"). Subsection retitled "Global Pattern of Change." Flagged inline
that this map uses the current 5-service definition and will need regenerating if the
services-list answer changes it. Rendered clean, figure confirmed present in output HTML.

## What's still pending beyond the Abstract — read this list before your side-by-side review

**Blocked on the services-list answer (export/risk vs. retention/protection) — don't start until Becky/Steve reply:**
- Biophysical Modeling of Ecosystem Services (Methods §3.1.1): still literally says "eight
  simulated continuous ecosystem service variables" grouped into "three core thematic areas" —
  the exact framing SP14/SP15 objected to. Needs a full rewrite naming the actual 5 services, once
  known.
- Per-service methodological detail (SP10): only sediment/nitrogen retention ratios currently get
  real explanation. **Partially addressed 2026-08-25** — the "For Becky — model run provenance"
  callout (originally written in the book's `02-methods.qmd`, never ported to the paper, and never
  discussed with Becky since she hasn't seen the book) was found and copied into the paper's
  Biophysical Modeling section: explains *why* only the ratios get real formulas (this project
  received finished InVEST rasters, never configured/ran those models directly) and asks Becky for
  provenance, citation, and climate-input details. Still open: adding real per-service *model
  mechanics* (Nature Access, Pollination, Coastal Risk) from the published InVEST documentation —
  legitimate to write without Becky's input since it's public methodology, not run-specific
  parameters, but not yet done.
- Figure 2 (`biome_combined_diffs.png`) reorientation to 5×2 (service rows × risk/service columns),
  with biome+income+region breakdowns folded in as graph-only content.
- The new 5×2 risk/service map grid (pollination production vs. sufficiency; nature access people
  vs. land cover) — also separately blocked on Becky's pollination-sufficiency layer, not sent yet
  regardless of the framing answer.
- 4.3 (Socioeconomic Profiling: KS Diagnostics) + 4.4 (Population Exposure/Serviceshed Multiplier):
  still the old 8-service text. Planned replacement is the water/access/combined beneficiary
  analysis already in the Abstract — **not yet confirmed with Becky that this is what "the new
  beneficiaries analysis" means**, worth explicitly checking during the side-by-side read.
- Section 4.1's narrative text (the two paragraphs) is still old 8-service prose — the map swap is
  done, the writing around it isn't yet rewritten to match.
- The mangroves explanation (see above) hasn't been written into the text yet — it's a finding, not
  yet a sentence in the paper.

**Not blocked, not yet done (fair game to pick up anytime):**
- Discussion §5.2's HDI/GINI typology and §5.4 Limitations still reference the old 8-service
  Cliff's-Delta numbers — will need updating once 4.3/4.4 are rebuilt, so they stay consistent.
- Conclusions section: not yet checked against the 5-service framing or Becky/Steve's comments at
  all — worth a read during tomorrow's pass specifically for stale 8-service numbers (252,215
  cells, 76% attribution gap, etc. — the old figures, not the corrected 189,927/13.8%/34.2%/65.8%
  ones already in the Abstract and the moved Annex section).
- References/citations: SP5's novelty claim and the REDD+ citation flag (Discussion §5.3, still
  present) both need literature verification before anything gets asserted as fact — flagged
  in-line in both places, not resolved.
- Author list, `[Additional Authors TBD]` placeholder — untouched.

## Suggested order of operations (not yet started)

1. Get Becky/Steve's answer on the service-framing question (blocks almost everything else).
2. Quick, low-risk fixes first: title, abstract unnumbering, Introduction rewrite, Methods
   clarifications — none of this depends on the service-framing answer.
3. Move 4.5 to Supplement — mechanical, no new data needed.
4. Once the framing question is answered: rebuild 4.1 (narrative + un-faceted maps), Figure 2
   (5×2), and 4.3+4.4 replacement together, since they all depend on which 5 services are final.
5. The new 5×2 risk/service map grid waits on Becky's pollination-sufficiency layer regardless.
6. Mangroves bug — can be investigated independently, anytime.

## Parked, not written into the paper (2026-08-25)

**Data Sources and Study Design — pollination NA-masking special case.** The original 8-service
text carried an unresolved bracketed author note: *"[double check, for the specific case of
pollination, I had to create a mask with all valid values for both dates, and reclass as zero, to
avoid NA values in the difference calculation. This is a special case, and I should clarify it in
the methods.]"* Decided not relevant enough to include in the streamlined exclusion-criteria
sentence (also fixed the same day — the sentence had been citing the wrong denominator, 1,302,099,
which `analysis/WORKLOG.md`'s 2026-07-29 entry documents as a stale/flawed figure for a different
question; corrected to 1,372,621, the actual valid-land denominator used throughout the current
5-service analysis). Parked here rather than deleted outright — if pollination's NA-handling ever
becomes a live question again, this is the pointer back to where it was raised and why it was
dropped.
