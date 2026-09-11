# Tuesday 2026-07-21, 11:30 — SWY meeting with Becky: talking points

Purpose of this meeting: get Becky's sign-off on three real decisions, not deliver a finished
model. Reference document to walk through live: `docs/swy/model_specification.md`. Visual
version: `docs/swy/swy_becky_meeting.qmd`.

## Opening (30 seconds)

"Before diving in — this turned out to be a bigger thing than I think either of us initially
framed it as. The other 8 services were InVEST runs you and others already did elsewhere; this
project only ever post-processed the outputs. SWY means actually standing up and running InVEST
ourselves, globally, for the first time — and as far as I can tell, nobody's done that anywhere,
which is exactly why a single global calibration was never going to be defensible."

## Precedent (1 minute)

- No published global-scale InVEST SWY application found anywhere.
- Closest precedent: Hamel et al. 2020, Peru + Myanmar multi-region study — NatCap-authored.
  **Full text now obtained and read**, not just the abstract.
- The closest thing to an actual *global* precedent isn't InVEST-specific at all — it's GCN250,
  a general hydrology curve-number product. Explain briefly (see model_specification.md).

## What reading Hamel et al. 2020 in full actually changed (2 minutes — this is new, flag it as such)

- Neither of their case studies derived CN/Kc from literature tables cold. Peru reused CN from
  an existing calibrated SWAT model of that exact basin. Myanmar reused CN/Kc from an existing
  national ecosystem-service assessment (Mandle et al. 2017).
- **This answers the "how do we operationalize subregional tables" question**: search for an
  existing calibrated study covering a representative watershed in each flagged biome, don't
  derive from TR-55 tables cold. Tropical forest already has candidates; mangroves are still a
  gap.
- CN sensitivity is dramatic, not theoretical — their own test found quickflow changed by
  **10.1-13.1x** between antecedent-moisture CN settings.
- Model reliability appears to degrade at large basin scale — their largest basin was 114,000
  km², and their own guidance recommends caution with absolute values in new geographies, with
  results under 10,000 km² best supported by their data.

## Decision 1 — the CN approach

Walk through the GCN250 tradeoff, then present the recommendation: **adopt GCN250 as the
baseline everywhere, patch only the ~3 biome classes its own authors flag as least validated**
(tropical moist broadleaf forest, mangroves, flooded grasslands/savannas), building each patch
by finding existing calibrated regional studies (per Hamel's precedent), not deriving from
scratch.

**The actual question for Becky**: does baseline-plus-targeted-patch satisfy the original
"subregional tables" concern, or is full from-scratch stratification still wanted regardless of
the time cost? Don't pre-answer this — it's her call to make, this is presenting the tradeoff
clearly enough that she can make it fast.

**Say this part out loud, don't let it pass as just another bullet**: the flooded
grasslands/savannas patch already has a direct hit — Oliveira et al. 2016, real measured CN
values from the Brazilian Cerrado, the closest ecological analog to the Colombian
Llanos/Orinoquía. Worth naming explicitly: offer to personally lead this specific patch, given
the PhD research background is directly on this exact system. Natural, genuine opening to ask
Becky about her grasslands working group and whether there's room to connect there — backed by
real expertise and an actual finding, not a favor being asked cold.

## Decision 2 — is "one global run" even the right framing?

New, surfaced directly by reading the full paper, not something scoped going in. Hamel's own
largest basin (114,000 km²) already showed spatial-variation problems. A genuinely global run is
planetary scale — orders of magnitude beyond anything validated anywhere in this review.

**The question for Becky**: run this as one true global model, or as basin-by-basin/regional-
mosaic runs (closer to what Hamel et al. actually did) combined afterward? Doesn't need to be
resolved today, but name it explicitly rather than assuming a single global run is obviously
right.

## Decision 3 — climate data provenance (kills two birds)

Flag that SWY's precipitation/ET0 needs overlap directly with the already-open WORKLOG.md
question about whether the SDR/NDR climate input is a fixed WorldClim raster or era-specific.
Worth asking if this can be resolved as one conversation rather than two separate blockers.

## What's already resolved (quick, don't over-linger here)

- HYSOGs250m (soil hydrologic group) — confirmed available, no research question left.
- NDVI-Kc regression for non-crop vegetation — confirmed still valid in 2020s literature.
- Ruled out SWAT's global datasets as a CN shortcut — checked, genuinely not there.
- WWF_biome stratification input confirmed real and already integrated in this pipeline.
- `kc_calculator.xlsx` reviewed, candidate sources identified for NDVI/watersheds/precipitation.

## What's still open after this meeting, regardless of the CN and scale decisions

- Building the lucode master table (doesn't exist anywhere in this project, for any service).
- Searching for calibrated regional precedent for the mangrove and flooded-grassland patches
  (Pantanal/Llanos hydrology literature, per Hamel's own strategy — not yet searched).
- Acquiring NDVI, HydroBASINS, and the settled precipitation source.
- The rain-events / climate-zone table, derived once precipitation is settled.

## Close

Confirm next check-in cadence for SWY (not deciding here whether that's weekly or tied to the
next Becky meeting) and note this documentation is deliberately kept standalone in `docs/swy/`,
not merged into the paper/book, until the model is actually built and this approach is validated.
