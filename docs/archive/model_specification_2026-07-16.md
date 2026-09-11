# Global InVEST Seasonal Water Yield: Model Specification (draft for Becky, 2026-07-16)

## Framing

This is not an extension of the existing 8-service pipeline in the way it might sound. The
other 8 services' InVEST runs were done by Becky and collaborators in earlier, separate work —
this project only ever post-processed the finished output rasters. **Nobody has run InVEST SWY
globally before, on this project or, as far as this review found, anywhere else** — precisely
because a single global calibration isn't considered defensible (see Precedent below). What
follows is a full accounting of what a real global run requires, what already exists, what's
been de-risked this week, and what's still a genuine research/engineering task.

## Precedent review

No published global-scale InVEST SWY application was found. Every InVEST SWY case study
located is watershed/country scale: West Bengal (India), a multi-region Peru + Myanmar study
(Hamel et al. 2020, *Journal of Environmental Management* 270:110792 — NatCap-authored, full
text obtained and read), and UK-catchment validation work. **The closest thing to a "global"
precedent isn't InVEST-specific at all — it's GCN250** (Ross et al., a global gridded
curve-number product, detailed below), which solves the same underlying problem (CN varies by
land cover and geography, no single number works everywhere) but as a general hydrology product,
not inside InVEST's own workflow.

**Hamel et al. 2020, now read in full, changes the framing in two important ways:**

1. **Neither of their case studies derived CN/Kc from literature tables cold.** Peru reused CN
   values from an existing calibrated SWAT model of that exact basin (Uribe et al. 2013, built
   years earlier for a payments-for-ecosystem-services program). Myanmar reused CN/Kc from an
   existing national ecosystem-service assessment (Mandle et al. 2017). **This is the actual
   operational answer to "how do we build subregional tables"**: search for an existing
   calibrated hydrologic study or ecosystem-service assessment covering a representative
   watershed in each flagged biome, and borrow its parameterization, rather than deriving values
   from TR-55 cover-type tables from scratch. See the CN decision section below for how this
   applies to our specific biome patches.
2. **Model reliability appears to degrade at large basin scale — and their largest basin was
   only 114,000 km².** Their own conclusion recommends caution with absolute values "in new
   geographies," with basins under 10,000 km² best supported by their validation data. A
   genuinely global run is planetary scale — orders of magnitude beyond anything validated here
   or found anywhere else in this review. **Worth treating "one global run" itself as an open
   question for Becky, not just the CN table** — a basin-by-basin or regional-mosaic approach
   (closer to what Hamel et al. actually did: two separate basin-scale applications, not one
   global one) may be the more defensible framing than a single planetary-scale run.

Also worth adding to the reading list: **Guswa et al. 2018** (*J. Hydrologic Engineering*
23(2)) — the actual paper behind InVEST-SWY's monthly CN-based quickflow method, more specific
than the general user guide. Validated against 544 US watersheds — another reminder that even
the model's core method's empirical grounding is American, not tropical/global.

## Full input requirements

| Input | Format | Status | Subregional customization needed? |
|---|---|---|---|
| **DEM** | Raster, projected CRS, meters | User has a global DEM available to provide | No — projection/alignment only |
| **LULC** | Raster, integer lucode matching biophysical table | Exists as 300m source rasters (ESA CCI 1992 / Copernicus C3S 2020) already used elsewhere in this project, but **no lucode master table exists yet anywhere in this pipeline** — building one is a new task regardless of SWY | The lucode scheme itself, not the raster |
| **Soil Hydrologic Group** | Raster, values 1-4 (A-D) | **HYSOGs250m confirmed available** (ORNL DAAC, current) | No — direct download |
| **Biophysical table: CN_A/B/C/D** | CSV, per lucode | **Open decision** — GCN250 (fast, ready-made) vs. literature-built biome-stratified table (slower, closer to Becky's original ask); see decision section below | Yes — this is the actual crux |
| **Biophysical table: kc_1...kc_12** | CSV, per lucode, monthly | Cropland: `kc_calculator.xlsx` (NatCap's own tool, downloaded/reviewed) has FAO-56 parameters for ~17 crops, but its built-in planting dates are Northern-Hemisphere defaults — usable as a formula/template, needs re-running per region with correct planting dates, not usable as-is. Non-crop vegetation: NDVI-based regression (Kamble et al. 2013, confirmed still valid through 2020s replications) — **NDVI not yet acquired**; candidate source identified: **MOD13A3** (MODIS monthly NDVI, 1km, global, free, via NASA LP DAAC/Earth Engine). Non-vegetated: fixed literature values, no work needed | Yes for cropland timing; no for non-crop (NDVI is inherently spatially resolved already) |
| **Monthly precipitation (12 rasters, mm)** | Raster per month | **Candidate identified, not yet acquired: CHIRPS** (0.05°/~5.5km, daily-to-monthly, 1981-present) — but CHIRPS **only covers 60°N-60°S**, missing boreal/tundra biomes entirely. CHELSA (global monthly climatology, 1979-2013) is the fallback for high latitudes, but is a fixed climatology, not era-specific — same open question already flagged for Becky in `analysis/WORKLOG.md` about the SDR/NDR climate input's temporal resolution | No — these products already carry spatial variation; this is an acquisition/blending/provenance decision, not a table-building one |
| **Monthly ET0 (12 rasters, mm)** | Raster per month | Same status as precipitation — recommended to come from the same source | Same as above |
| **Rain events table** (or Climate Zone + Climate Zone Rain Events table for spatial variation) | CSV | Not directly available from any source — **must be derived** by counting >0.1mm days per month from a daily precipitation product (CHIRPS has daily granularity, so this is a derivation task once the precip source is settled, not a separate acquisition) | Yes, in effect — rain-day frequency genuinely varies by climate zone; InVEST's "climate zone" advanced option is the built-in mechanism for this |
| **Watersheds (AOI)** | Vector polygons | **Candidate identified: HydroBASINS** (part of HydroSHEDS, WWF-associated product), 15 arc-second (~500m), global, hierarchical, free, shapefile format — strong fit, not yet downloaded | No — standard existing product |
| **Threshold flow accumulation** | Number (pixels) | Default exists; needs checking against whatever resolution the run is actually done at | Not customization, a resolution-tuning parameter |
| **Alpha_m / Beta_i / Gamma** | Numeric/table | Defaults exist (1/12, 1, 1) | Defaults likely fine to start |
| **Flow direction algorithm** | D8 or MFD | Model choice, not data | n/a |

## The CN decision — for Becky

Note this decision only concerns CN. Kc (cropland via FAO-56, non-crop vegetation via the NDVI
regression) is a fully separate input and proceeds unchanged regardless of what's decided here.

**Recommended approach: GCN250 as baseline, targeted biome-level correction on top — not a
flat adopt-or-rebuild choice.** GCN250 (Figshare, DOI 10.6084/m9.figshare.7756202; ESA CCI-LC
2015 crosswalked to NRCS NEH-630 categories + HYSOGs250m soil groups, three
antecedent-runoff-condition variants) is real, fast, peer-reviewed, and far more granular than a
flat global default — no reason to discard it. But its regional sensitivity comes entirely from
land-cover-type + soil + wetness scenario, never from geography: the same land-cover class gets
the same base CN everywhere in the world. The authors' own caution specifically names
forested/humid tropical environments as least validated — which maps onto a short list of this
pipeline's WWF_biome classes (already confirmed integrated, `data/vector_basedata/Biome.gpkg`),
not all 16: **Tropical & Subtropical Moist Broadleaf Forests, Mangroves, and Flooded Grasslands
& Savannas** are the priority candidates for a literature-sourced correction layered on top of
the GCN250 baseline, rather than rebuilding all 16 biomes from scratch. This directly answers
Becky's original "same land cover, different place, different real behavior" concern, scoped to
where the evidence says it actually matters, instead of spreading research effort thin across
biomes that are probably already reasonably represented (temperate/boreal forests, deserts,
grasslands — better covered by the underlying US-derived TR-55/NEH-630 data to begin with).

**This isn't a hypothetical fix — Hamel et al. 2020 found CN sensitivity to be dramatic**:
quickflow changed by factors of 10.1-13.1x between antecedent-moisture-condition CN settings in
their Chindwin case study. Getting the patched biomes wrong isn't a marginal error.

**How to actually build the patch, per Hamel's own precedent**: neither of their case studies
derived CN from literature tables cold — Peru borrowed from an existing calibrated SWAT model of
that basin, Myanmar borrowed from an existing national ecosystem-service assessment. Same
strategy here: search for an existing calibrated hydrologic study or ecosystem assessment
covering a representative watershed in each flagged biome, rather than deriving from TR-55
cover-type tables directly.
- Tropical moist broadleaf forest: already has candidates (Calero Mosquera et al. 2021, Fábrega
  et al. 2012 — real measured/evaluated tropical forest CN).
- **Flooded grasslands & savannas: direct hit found — Oliveira et al. 2016**, real measured CN
  values from Brazilian Cerrado (undisturbed savanna CN ~81.2 on soil group B, plus converted
  land-use comparisons). Cerrado is the closest ecological analog to the Colombian
  Llanos/Orinoquía — same broad Neotropical savanna biome. A second Llanos-specific paper
  (Nogales Pimentel et al. 2021, hydrological modeling in the Orinoquía) was also found, but
  **is not a CN source** — it uses a different model (ABCD + floodplain interaction) with no
  vegetation-based parameterization at all. Valuable as regional precedent/cross-validation, not
  as a parameter source — don't conflate the two.
- Mangroves: still an open gap, not yet solved.

Still worth Becky's explicit sign-off before proceeding, since it's a real methodological choice,
not a formality: does "baseline + targeted patch on ~3 biomes" satisfy the original subregional
concern, or does she want the full from-scratch build regardless?

**Mechanical note on how GCN250 would actually plug into InVEST** (worth having an answer ready
if asked, not yet a decision to make): InVEST's SWY model expects a lucode-indexed biophysical
table (CN_A/B/C/D per lucode), not an arbitrary per-pixel raster. GCN250 is a per-pixel raster,
not a table — so using it means summarizing it into that table format (sampling GCN250's values
within each of our lucode classes, per hydrologic soil group, to derive representative CN_A-D
values), not feeding it in directly. The alternative — bypassing InVEST's internal CN/runoff
calculation entirely via its "User-Defined Local Recharge" advanced option — would mean
computing quickflow ourselves outside InVEST, which is a much bigger custom-engineering path and
not recommended given this is already a first-time model run.

**This also shapes the lucode master table decision (see input table above)**: rather than
inventing a new land-cover classification scheme from scratch, the cleanest path is to **reuse
GCN250's own published ESA-CCI-to-NEH-630 crosswalk as this pipeline's lucode scheme** — it's
already peer-reviewed, already exactly matches the categories GCN250 itself uses, and makes the
"summarize GCN250 into a table" step above consistent by construction rather than requiring a
second independent crosswalk to reconcile against.

## A second real decision for Becky: is "one global run" even the right framing?

Surfaced by reading Hamel et al. 2020 in full, not something we'd scoped going in. Their largest
basin (Chindwin, Myanmar) was 114,000 km², and even at that scale they found spatial variation
poorly captured and recommended caution with absolute values "in new geographies." Their own
guidance: relative baseflow results in basins **under 10,000 km²** are best supported by their
validation data. A genuinely global run is planetary scale — several orders of magnitude beyond
anything validated in the literature found in this review, InVEST-specific or otherwise.

Worth naming as its own open question, separate from the CN decision: run this as one true
global model, or as a set of basin-by-basin or regional-mosaic runs (closer to what Hamel et al.
actually did — two separate basin-scale applications, not one global one) and combine results
afterward? This doesn't need to be resolved Tuesday, but it should be named explicitly rather
than assumed away.

## What's genuinely resolved this week vs. still open

**Resolved:**
- Soil hydrologic group input (HYSOGs250m) — confirmed available, no research question left.
- Non-crop vegetation Kc method (NDVI regression) — confirmed still valid/replicated in 2020s literature.
- Confirmed no shortcut exists via SWAT's global land-cover products for CN — ruled out, not just unchecked.
- Found GCN250, which reframes the CN problem from "build 16 biomes from scratch" to "adopt as
  baseline, patch ~3 flagged biomes."
- Confirmed the WWF_biome stratification input is real and already integrated (not a hopeful assumption).

**Also resolved since (2026-07-16 evening)**: `kc_calculator.xlsx` reviewed (confirms the same
regional-transferability problem for Kc, reinforcing the NDVI-route decision); candidate sources
identified for NDVI (MOD13A3), watersheds (HydroBASINS), and precipitation (CHIRPS, with a
coverage gap above 60°N/below 60°S that CHELSA would need to fill).

**Also resolved since (2026-07-17)**: Hamel et al. 2020 obtained and read in full — answers the
"how to operationalize subregional tables" question (borrow from existing calibrated regional
studies, don't derive from scratch) and surfaces two new considerations: CN sensitivity is
dramatic (10-13x quickflow swings, not theoretical), and whether "one global run" is even the
right framing given the model's own documented reliability limits at large basin scale.

**Still open, real work:**
- The CN decision above (needs Becky's input).
- The "one global run vs. basin-mosaic" question above (needs Becky's input).
- Building the actual lucode master table — doesn't exist anywhere in this project yet, for any service.
- Downloading and integrating NDVI (MOD13A3 identified, not yet acquired).
- Resolving the climate-data question (precip/ET0 source and provenance) — tied to the existing WORKLOG.md blocker, needs Becky.
- Downloading HydroBASINS.
- Deriving the rain-events table from daily CHIRPS once the precip source is settled.
- Re-running the FAO-56 Kc parameters with region-correct planting dates (not the spreadsheet's Northern-Hemisphere defaults).
- Sourcing CN precedent for the flooded grasslands/savannas biome (Pantanal/Llanos hydrology
  literature, not yet searched) and the mangroves biome (no precedent found yet at all).

## How to apply

This is a specification, not a build — no model has been run, no table has been assembled.
The goal for Tuesday is to walk in with this document and get Becky's call on the CN decision
and the climate-data provenance question, since both block real progress and both need her
input specifically, not just more literature review.
