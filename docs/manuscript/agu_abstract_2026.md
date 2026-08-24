---
title: "AGU 2026 Abstract Submission Draft"
status: "FINAL wording agreed 2026-08-06 -- submission itself blocked, see note below"
date: "2026-08-06"
---

Adapted from the paper's existing abstract (`docs/manuscript/paper_draft.qmd`), tightened to AGU's
rules: title <=300 characters (title case), abstract <=2,000 characters excluding spaces
(including punctuation), ASCII only (no em dashes, no curly quotes, no superscripts).

**Submission status (2026-08-06): the AGU portal crashed before the original 2026-08-05 deadline
after the user had already paid the abstract fee under a newly created account (original AGU
account inaccessible, tied to a deactivated school email). Contacting AGU's helpdesk to request
the submission be allowed to complete or for a short technical grace period, citing the paid,
in-progress submission and the crash as evidence of a system-side failure, not lateness.** Wording
below is agreed-final regardless of that outcome -- ready to paste the moment submission is
possible again.

Verified 2026-08-06 (final wording): title 84/300 chars, abstract 1,825/2,000 chars excl. spaces,
ASCII-clean (checked with `iconv -f UTF-8 -t ASCII`, not just eyeballed). The 18.4% figure was
independently verified against the correct 1,372,621-cell land denominator, not the flawed
19.37%/1,302,099 figure still live in `paper_draft.qmd` line 231 -- see
`docs/hotspot_redesign_plan.md`, "pending major paper edits" item 1, for that open correction.
"Deteriorating globally" is deliberately framed as widely-reported prior consensus (IPBES 2019,
Millennium Ecosystem Assessment), not this paper's own claim to defend. Title deliberately avoids
"equity" (only appears in the abstract's closing application sentence, not as a finding) in favor
of "Unequal Exposure," which maps directly onto the body's actual disproportionate-share and
inequality findings.

Reflects the current, already-vetted 8-service paper numbers deliberately -- not the in-progress
5-service redesign, which still isn't finalized (Phase 4 KS/Gini test not started, though now
unblocked -- Rich's pct-vs-abs question was resolved 2026-08-06, he used pct throughout). See
`docs/hotspot_redesign_plan.md` for that status.

## Title

Global Hotspots of Ecosystem Service Decline: Unequal Exposure Worldwide (1992-2020)

## Abstract

Global assessments report widespread deterioration in ecosystem service (ES) provision, but key aspects such as the spatial distribution of decline, which populations are most exposed, and how far the effects of decline extend beyond the specific site where it occurs remain poorly characterized at the global scale. We integrate global InVEST ecosystem service models, ESA CCI/Copernicus C3S land cover data, and gridded socioeconomic layers across approximately 1.3 million 100 km2 equal-area grid cells worldwide, comparing conditions at two time points, 1992 and 2020, via a Symmetric Percentage Change (SPC) approach. We assess eight metrics spanning five distinct ecosystem services and define hotspots as the top 5% of cells showing the most severe change per metric, whether declining service provision or increasing risk or damage. We identify 252,215 such cells (18.4% of assessed land area), tallying how many of the eight metrics overlap in each, along with their spatial distribution, socioeconomic exposure, downstream beneficiary reach, and relationship to land cover conversion. The underlying analytical pipeline supports flexible aggregation, from native pixel resolution to custom groupings such as biomes, income levels, or administrative units, enabling targeted analysis for specific geographies and decision contexts.

This approach provides comparable, quantified figures to assess impact. Latin America and East Asia-Pacific carry a disproportionate share of these hotspots relative to their land area, and lower-middle-income countries bear 1.6 times the intensity of high-income countries. The same globally consistent method applies to any region. Effects extend far beyond these cells themselves: 3.1 billion people live directly within them, rising to approximately 7.6 billion when downstream hydrological and travel-time connectivity are included, with lower-income and higher-inequality populations consistently overrepresented. These results provide spatially explicit inputs for nature accounting, subnational conservation finance targeting, and equity-weighted prioritization.

## Plain Language Summary (optional AGU field, max 200 words)

Built per AGU's own "Creating a Plain Language Summary" toolkit (develop a take-home message,
avoid jargon including words with different everyday meanings, define terms inline, test on a
non-scientist). Verified 2026-08-06: 194/200 words, ASCII-clean. Went through several rounds
catching real precision issues, not just style: "green space" was too narrow versus what the
InVEST Urban Nature Access model actually measures (confirmed directly against the InVEST
documentation -- it explicitly covers parks, wetlands, and shorelines, not just vegetated areas,
each land-cover type getting a configurable "naturalness" score); "Nature provides... access to
natural areas" was circular; "outdoor spaces" didn't imply natural character (a parking lot is
outdoors too); "time spent outdoors" implied measured behavior the model doesn't actually track
(it measures proximity/availability, not time-use). Settled on "open spaces to enjoy" -- a
geography/planning term covering the full range (parks, wetlands, shorelines, non-pristine areas)
without repeating "nature" or overclaiming.

Nature provides essential benefits to people, including clean water, coastal protection, pollination, and open spaces to enjoy. These benefits are declining worldwide, but where decline is worst, who is most affected, and how far impacts reach beyond their source have been hard to characterize.

We used satellite data and computer models to measure how nature's benefits changed worldwide between 1992 and 2020, dividing the world's land into a grid of 1.3 million equal-sized cells. We identified over 250,000 cells, about 18% of the world's land area, where decline is most severe, and tracked how many types of decline overlap in the same place.

Decline areas are present worldwide but are especially concentrated in Latin America and East Asia-Pacific, with lower-income countries affected more than higher-income ones. Impacts reach far beyond these locations: at least 3.1 billion people live directly in affected areas, rising to 7.6 billion when people connected by water flow or travel are included, with poorer and more unequal communities affected most.

The same approach can be applied anywhere in the world, making it a practical tool for directing conservation funding and policy to the places and people who need it most.
