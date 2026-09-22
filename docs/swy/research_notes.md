# Seasonal Water Yield: Biophysical Table Research Notes

**Folder layout note**: this folder (`docs/swy/`) holds SWY's *working* materials — methods
reference, this reasoning log, pipeline-status diagrams, correspondence drafts. The shareable,
rendered status report lives separately at `docs/reports/swy/` — same split as `colombia_clec/`
and `phase4_beneficiary/` elsewhere in `docs/reports/`, where reports for every topic live apart
from that topic's own working docs. The two folders sharing the name "swy" is coincidental, not a
sign one is stale or duplicated.

Status: research in progress, not started implementation. First pass 2026-07-10.

## Critical framing, clarified by user 2026-07-16 — read this before anything else below

**This is fundamentally different work from the other 8 services, not an incremental addition
to the same kind of task.** The other 8 services' InVEST model runs were never done by this
project — Becky and collaborators ran those models in earlier, separate research, and handed
over the finished output rasters "as-is." This repo's actual job (hotspot identification,
change detection, synthesis, everything documented elsewhere in this repo) is post-processing
of pre-computed outputs — it never needed the original biophysical tables, LULC master lookup,
or climate inputs, which is why none of that exists in this checkout (confirmed by direct
repo search, 2026-07-16).

**SWY has never been run before, globally, by anyone on this project — precisely because a
single global calibration isn't feasible**, which is the whole reason a subregional/stratified
approach is even under discussion. Producing an SWY output means **actually running the InVEST
SWY model from scratch, globally, for the first time** — not reprocessing an existing raster.
That requires assembling every real input (DEM, LULC at model resolution, soil hydrologic
group, monthly precip/ET0, rain events, biophysical table) and running the model, which is a
qualitatively bigger undertaking than anything done in this project so far. GCN250 (see below)
would remove the single hardest piece of that (CN), but does not remove the rest of the model
run itself. **This distinction needs to be explicit with Becky — the honest framing for Tuesday
is "here's what a new model run requires and what we've de-risked so far," not "here's a table
update."**

## Context

Priority #2 after the IDB deck (see `docs/presentations/idb_wwf_workshop1_case_study.qmd`): add InVEST
Seasonal Water Yield (SWY) as a 9th service. The blocker isn't the pipeline architecture — it's that SWY's
biophysical table needs curve numbers (CN) and monthly crop coefficients (Kc) per LULC class, and both are
known to vary regionally in ways the other 8 services' single global biophysical table doesn't have to deal
with. Becky's steer: build subregional tables and assemble them into global coverage, rather than one
universal table.

Full data requirements (DEM, LULC, soil group, precip/ET0, rain events, model params) are in the InVEST docs:
https://storage.googleapis.com/releases.naturalcapitalproject.org/invest-userguide/latest/en/seasonal_water_yield.html

## CN — Curve Number

- **Origin**: SCS/NRCS-USDA, developed for the United States. Primary reference: NRCS TR-55 (1999),
  *Urban Hydrology for Small Watersheds*. Full CN methodology in NRCS-USDA (2007) *National Engineering
  Handbook*.
- **InVEST's own guidance is explicit that there is no global shortcut**: "area-specific values are
  preferred" over applying TR-55 defaults everywhere. CN is a function of hydrologic soil group (HSG),
  cover type, treatment, hydrologic condition, and antecedent runoff condition — CN=99 for
  stream-connected water bodies/wetlands; wet-condition (ARC-III) values for flood modeling.
- **Practical implication**: CN genuinely needs literature-sourced, region-specific values. This is the
  harder of the two parameters and the one that actually justifies Becky's "subregional tables" framing.
- **SWAT checked, 2026-07-16 (see below) — no shortcut there.** SWAT's own global land cover
  databases (Abbaspour & Ashraf Vaghefi 2019) only crosswalk land cover classes to SWAT's own
  crop/landuse definitions, not curve number values. Useful negative result, not a building block —
  don't re-check this.
- Also checked: whether any published InVEST SWY case study had already assembled a multi-region
  CN table — yes, Hamel et al. 2020 (see below), by borrowing from existing calibrated regional
  studies rather than building one from scratch. This became the actual operating strategy.
- Source citations: NRCS TR-55 (https://www.nrc.gov/docs/ML1421/ML14219A437.pdf); CN tables also at
  https://www.hec.usace.army.mil/confluence/hmsdocs/hmstrm/cn-tables

## Kc — Crop/Plant Coefficient (monthly, kc_1...kc_12)

Two very different sub-problems here — cropland and everything else.

### Cropland
- **Standard reference**: Allen et al. (1998), FAO Irrigation and Drainage Paper 56 — InVEST cites this
  directly. FAO's online Kc resource: http://www.fao.org/3/X0490E/x0490e0b.htm
- NatCap provides a ready-made weighted-average Kc calculator spreadsheet:
  https://github.com/natcap/invest.users-guide/raw/main/data-sources/kc_calculator.xlsx (not yet
  downloaded/reviewed).
- Kc timing (which month is green-up vs. die-down) is genuinely region/hemisphere-dependent — needs local
  crop-calendar/phenology knowledge, which is where regional variation actually bites for crops.

### Non-crop vegetation (forest, grassland, shrubland — the bulk of global land area)
- **This may not need a hand-built regional table at all.** InVEST's own docs point to an NDVI/LAI-based
  satellite method as an alternative to literature lookup for non-crop vegetation.
- Confirmed in the literature: Kamble, Kilic et al. (2013), *Remote Sensing* 5(4):1588 — "Estimating Crop
  Coefficients Using Remote Sensing-Based Vegetation Index." Validated linear regression:
  **Kc = 1.457 × NDVI − 0.1725**, r² = 0.90–0.91 against AmeriFlux eddy-covariance flux tower measurements
  (MODIS NDVI, multiple crop/vegetation types).
- Why this matters: NDVI is already globally available at consistent resolution and (unlike CN) is
  inherently spatially and temporally resolved — using it sidesteps the "assemble N regional tables"
  problem entirely for whatever fraction of global LULC this regression is valid for. **This is the
  single most promising lead so far for keeping the Kc side of this manageable at global scale.**
- Not yet checked: how well this specific regression generalizes outside the original study's vegetation
  types/climate; whether more recent/better-validated versions of this regression exist since 2013.

### Non-vegetated (open water, wetlands, bare soil, built/impervious)
- InVEST's own guidance gives typical Kc ranges directly (0.3–1.2) — this is the easy part, no research
  needed.

## Soil Hydrologic Group (separate missing input, not yet in the pipeline)

SWY needs a raster of hydrologic soil group (A/B/C/D, encoded 1-4) — this input doesn't exist anywhere in
the current pipeline (all 8 existing services are soil-group-agnostic). Candidate: **HYSOGs250m** (Ross et
al. 2018), a global 250m hydrologic soil group product. **Confirmed still live and available**
(2026-07-16, see below) via ORNL DAAC (DOI 10.3334/ORNLDAAC/1566) and NASA Earthdata — this input is
unblocked, just not yet downloaded (see the "Open questions" checklist near the end of this file).

## Proposed approach (2026-07-10 draft — superseded, kept for history only)

**Superseded 2026-07-16 by the GCN250 milestone below** — item 1's "build subregional tables from
scratch, stratified by biome/Köppen" plan was replaced by "use GCN250 as the global baseline
everywhere, patch only the ~3 biomes GCN250's own authors flag as least validated." Items 2–4
(Kc) held up and are still the actual plan. Don't build against item 1 as written — see the
"Open questions / next steps" section near the end of this file for the actual current checklist.

1. ~~**CN**: build subregional tables, stratified by some combination of biome (WWF_biome, already in this
   pipeline's grid attributes) and/or Köppen climate zone.~~
2. **Kc, cropland fraction**: FAO-56 tables, crop-calendar-adjusted by hemisphere/climate zone at minimum.
3. **Kc, non-crop vegetation fraction**: NDVI-derived via the Kamble et al. regression (or a better
   updated one, if found) — likely avoids needing a regional lookup table at all for most of the land
   area modeled.
4. **Kc, non-vegetated**: fixed literature values per InVEST's own guidance, no further research needed.

## Update 2026-07-16 — three open questions resolved

- [x] **HYSOGs250m confirmed still available and current.** Actively archived at ORNL DAAC
  (DOI: 10.3334/ORNLDAAC/1566) and mirrored on NASA Earthdata/data.nasa.gov, openly licensed,
  250m GeoTIFF, derived from SoilGrids250m texture/depth-to-bedrock. Confirmed available as of
  2024-2025, not an orphaned/stale product. **This unblocks the soil hydrologic group input —
  no longer a research question, just an acquisition/integration task.**
- [x] **Kamble et al. (2013) NDVI-Kc regression holds up.** Not a one-off: follow-up studies
  through 2020-2023 replicate the same approach across other crops/vegetation types, with some
  (e.g. a 2023 Sentinel-2 wheat study) reporting even higher fit (r² > 0.93) than the original.
  This is a genuinely active, corroborated method, not a single dated citation — **raises
  confidence in this being the right call for the non-crop-vegetation Kc fraction.**
- [x] **Checked SWAT's global land cover/crosswalk datasets (Abbaspour & Ashraf Vaghefi 2019,
  "Global Land Cover for SWAT") as a possible CN shortcut — confirmed it is NOT one.** That
  dataset only crosswalks land cover classes to SWAT's own landuse/crop definitions; it does not
  carry curve number values or an HSG-stratified CN lookup. **This is a real, useful negative
  result: it confirms Becky's original "no global shortcut, build subregional tables" instinct
  was correct** — there's no existing global CN product to borrow instead of doing the work.

## Stratification scheme — decision made given the timeline

Going with **biome-based stratification** (WWF_biome, already present in this pipeline's grid
attributes) rather than Köppen climate zones, purely for delivery reasons: biome requires no new
spatial join, Köppen would be a new input to acquire and integrate. Flagging Köppen as a
possible future refinement if biome-level CN strata prove too coarse once real values are
assembled — not deciding that now, revisit after the first table pass.

## Major update 2026-07-16 (later same day) — GCN250 changes the CN plan

Found while double-checking whether SWAT had a CN shortcut: it doesn't, but a **dedicated,
purpose-built global gridded curve number product does** — **GCN250** (same research group as
HYSOGs250m, published as a follow-on). This was not on the radar before today.

- **What it is**: global CN rasters at 250m (7.5 arc-second, WGS84), built by crosswalking ESA
  CCI-LC 2015 land cover (36 classes) into the official NRCS NEH-630 land-cover categories, then
  combined with HYSOGs250m soil groups — i.e., it's the same lookup-table logic we were about to
  build ourselves, already done, peer-reviewed, and published. Delivered as **three rasters**
  for dry/average/wet antecedent runoff conditions (ARC), not one.
- **Public and downloadable now**: Figshare, DOI 10.6084/m9.figshare.7756202, R script for
  regeneration included.
- **The honest caveat, worth flagging explicitly to Becky**: GCN250's regional differentiation
  comes from *land-cover-type granularity + wetness scenario*, not from *biome/region-specific
  values for the same land-cover type*. A given land-cover class gets the same base CN everywhere
  in the world (modulated only by which ARC scenario is selected) — closer to "one well-built
  global scheme" than to the "subregional tables" framing Becky originally asked for. Authors'
  own caution: "hydrologists should proceed with caution... and should always compare generated
  runoff with observed values whenever possible," and note the underlying CN method itself was
  built for small US agricultural watersheds, with the most uncertainty in forested/humid
  environments — exactly the tropical forest biomes most relevant to this project's LAC focus.
- **What this means for the plan**: this is a real decision point, not an obvious win. Option A —
  adopt GCN250 directly (fast: an integration task, not a table-building research project, same
  pattern as using HYSOGs250m directly). Option B — still build the originally-planned
  biome-stratified table, using GCN250 as a validation/starting reference rather than a
  replacement, if Becky's original concern about regional specificity within a land-cover class
  is judged to matter more than shipping speed. **Not deciding this alone — this is exactly the
  kind of call to bring to Tuesday's meeting**, since it trades off timeline against fidelity to
  Becky's original methodological instinct.

## Update 2026-07-16 (evening) — kc_calculator.xlsx reviewed, remaining inputs sourced

**`kc_calculator.xlsx` downloaded and reviewed.** Confirms the same regional-transferability
problem exists for Kc as for CN, and confirms why the NDVI-regression route for non-crop
vegetation is the right call. Four sheets:
- `Kc calculator` — a working template with pre-filled monthly Kc profiles for corn, forest,
  grassland, stagnant water, wetlands, urban, soy, wheat, cotton, fallow, alfalfa/hay,
  switchgrass. **These profiles are baked to a Northern Hemisphere temperate growing season**
  (bare Jan-Apr, canopy peak Jul-Sep, harvest Oct-Nov) — e.g. "forest" is hardcoded as dormant
  in winter and full-canopy in summer, which is wrong for tropical evergreen forest (no
  dormant season) and inverted for the Southern Hemisphere. Useful as a Northern-temperate-biome
  reference/starting point, not a global table.
- `FAO tables` — proper FAO-56-style parameters (Kc_ini/mid/end, stage lengths, a default
  planting date) for ~17 crops (corn, soy, wheat, rice paddy, rye, oats, sorghum, cereals,
  potatoes, sugarcane, sugarbeets, oil seed, cotton, legume, vegetable, durum wheat, etc.) and
  the formula that converts stage-Kc + planting date + stage lengths into a monthly curve. This
  is directly usable for the cropland fraction — but the built-in planting dates are single
  defaults (also Northern-Hemisphere-biased) and need to be re-run per region/hemisphere with
  locally correct planting dates, not used as-is globally.
- `references` — source list for the ET/Kc values used (FAO-56, several US extension/irrigation
  sources, wetland ET literature — e.g. Lafleur & Rouse 1988, Lafleur 1990, Lott & Hunt 2001,
  Rosenberry et al. 2004 — all Northern temperate/subarctic wetland studies, another
  regional-transferability flag for wetlands specifically).
- `crops_inches day` — a temperature-indexed corn growth-stage lookup (US Midwest, source cited
  as a NetafimUSA corn manual) — not globally relevant, skip.

**Remaining inputs — sourcing options identified:**
- **NDVI**: MOD13A3 (MODIS monthly NDVI/EVI, 1km, global, 2000-present, no use restrictions),
  via NASA LP DAAC or Google Earth Engine. Not yet downloaded/integrated.
- **Global watersheds/AOI**: HydroBASINS (part of HydroSHEDS — note, a WWF-associated product),
  15 arc-second (~500m), global, hierarchical sub-basin polygons, shapefile format, freely
  available. Good candidate for the SWY "Area of Interest" input.
- **Monthly precipitation**: CHIRPS (0.05°, ~5.5km, daily-to-monthly, 1981-present) is the
  strongest global candidate, but **CHIRPS only covers 60°N-60°S** — misses boreal/arctic
  biomes (Boreal Forests/Taiga, Tundra) entirely, a real gap for a truly global run. CHELSA
  (monthly climatology, 1979-2013, full global coverage) is a fallback for those latitudes, but
  it's a fixed climatology, not era-specific (1992 vs. 2020) — same open provenance question
  already flagged for Becky in `analysis/WORKLOG.md`. May need a blended approach (CHIRPS +
  CHELSA for high latitudes) rather than one single source.
- **Rain events table** (count of days >0.1mm rain per month): not directly available from any
  source above as a pre-computed product — would need to be *derived* from a daily precipitation
  product (CHIRPS has daily granularity) by counting threshold-exceedance days per month. A
  real derivation step, not a download.

## Update 2026-07-17 — literature review expanded, 9 to 16 references

Added 7 references to `docs/swy/literature_review.ris`, found by searching specifically for
literature on the ~3 biomes flagged for the CN patch, updated NDVI-Kc validation, and proper
citations for the newly-identified data sources (NDVI, watersheds, precipitation):

- **Calero Mosquera et al. 2021** (tropical CN evaluation, *Earth Sciences Research Journal*)
  and **Fábrega et al. 2012** (measured CN in a Panama Canal tropical rainforest microbasin) —
  both directly support the tropical-forest patch with real evidence, not just GCN250's own
  caution. Calero Mosquera et al. found the SCS-CN method genuinely overestimates runoff under
  tropical conditions specifically because initial abstraction coefficients were calibrated on
  US watersheds — direct empirical confirmation, not just a theoretical caveat.
- **Mangroves and flooded grasslands/savannas — searched specifically, found nothing usable.**
  No literature surfaced giving CN or runoff-coefficient values specific to mangrove/saturated
  wetland systems. This is a real, still-open gap in the biome-patch plan, not yet solved —
  worth flagging to Becky directly rather than assuming it'll be easy once started.
- A 2025 grapevine Sentinel-2 Kc paper — recent evidence the NDVI-Kc approach keeps validating
  in current literature, though not itself a source of new global parameters (different crop,
  author list unconfirmed — flagged in the .ris entry to verify before formal citation).
- Proper citations added for the candidate data sources identified this week: MOD13A3 (NDVI,
  Didan 2021), HydroBASINS/HydroSHEDS (Lehner & Grill 2013 — a WWF-associated product), CHIRPS
  (Funk et al. 2015), CHELSA (Karger et al. 2017).
- Two entries (Fábrega et al. 2012, the grapevine 2025 paper) have incomplete author/venue
  metadata — search couldn't fully confirm them, marked "VERIFY BEFORE FORMAL CITATION" in the
  .ris file rather than guessed at.

## Update 2026-07-17 (later) — Hamel et al. 2020 obtained and read in full

User obtained the paywalled PDF directly. Changes the framing in three ways, not just fills in
the precedent citation:

1. **How they actually built CN/Kc tables, answering the "how do we operationalize subregional
   tables" question**: neither case study derived values from literature tables cold. Peru
   reused CN straight from an existing calibrated SWAT model of that exact basin (Uribe et al.
   2013, built years earlier for a payments-for-ecosystem-services program). Myanmar reused
   CN/Kc from an existing national ecosystem-service assessment (**Mandle et al. 2017** — same
   paper set aside earlier in the IDB deck work, now relevant again for a different reason:
   possibly usable as a real parameterization source, not just a citation). **Operational
   strategy going forward: search for an existing calibrated hydrologic study or ecosystem
   assessment covering a representative watershed in each flagged biome, and borrow its
   parameterization, rather than deriving from TR-55 cover-type tables from scratch.** Tropical
   moist broadleaf forest already has candidates (Calero Mosquera et al. 2021, Fábrega et al.
   2012). Flooded grasslands/savannas: worth searching Pantanal or Llanos/Orinoquía hydrology
   literature specifically (both named WWF priority landscapes; Llanos also personally relevant
   given the user's own research background there). Mangroves: still an open gap, not yet
   searched with this specific framing.
2. **CN sensitivity is dramatic, not theoretical, per Hamel's own sensitivity analysis**:
   quickflow changed by **factors of 10.1-13.1x** between antecedent-moisture-condition CN
   settings in the Chindwin basin. A concrete, citable number for why the CN decision matters,
   not just a conceptual concern.
3. **Model reliability appears to degrade at large basin scale, and Hamel's largest basin was
   only 114,000 km².** Their own conclusion: use with caution for absolute values "in new
   geographies," and that basins **under 10,000 km²** are best supported by their validation
   data. A genuinely global run is planetary scale — orders of magnitude beyond anything
   validated in this paper or found anywhere else in this review. **This is a real, separate
   caveat worth raising with Becky alongside the CN decision** — not just a CN-table problem,
   but a question of whether "one global run" is the right framing at all versus a basin-by-basin
   or regional-mosaic approach (closer to what Hamel et al. actually did: two separate
   basin-scale applications, not one global one).

New reference to add: **Guswa et al. 2018** (*J. Hydrologic Engineering*) — the actual paper
behind InVEST-SWY's monthly CN-based quickflow method, cited directly in Hamel et al., more
specific than the general user guide.

## Routing vs. parameterization — two separable layers (clarified 2026-08-24)

Worth stating explicitly, since it clarifies what's actually still open: this problem splits into
two layers that get assembled together, not one undifferentiated "run SWY globally" task.

1. **Routing** (basin-dependent): quickflow/baseflow accumulation only makes physical sense within
   a real, DEM-coherent watershed — this is why HydroBASINS/basin-by-basin execution is the plan
   (see open item below), not a single continuous global domain.
2. **Parameterization** (basin-independent): CN is assigned per pixel from the land-cover-keyed
   lookup table (`gcn250_esa_lc_cn_table.csv`, ESA LC class + soil group → CN_A/B/C/D), with a
   narrower biome-specific correction layered on top for the ~3 flagged biomes (tropical moist
   forest, mangroves, flooded grasslands/savannas — see open item below). This assignment has
   nothing to do with which basin a pixel falls in.

Assembly = the global CN/Kc raster (base table + biome corrections) gets clipped to each basin's
extent, then each basin is routed independently using those already-correct pixel values —
TaskGraph/ecoshard being the mechanism for running that per-unit at global scale. Sent to Rich
(and Becky, same channel) 2026-08-24 for confirmation.

**Confirmed by Rich (2026-09-03, Slack, verbatim "Yes!")**: the *mechanical* framing is correct
— `run_swy_global.py` really does batch the watershed vector into per-basin jobs, route each
independently through TaskGraph, stitch into one continuous global mosaic, and the CN/Kc raster
paths really do bypass the lookup table when supplied directly. Combined with the independent
code-level confirmation from reading `swy_global` directly the same day (see below), this half
of the architecture question is closed.

**Genuinely still open, confirmed NOT asked in the message that got sent**: the message as
actually sent only asked the mechanical question above — it dropped the Hamel et al. 2020
validation-scale nuance (whether stitching many small basin runs together satisfies what Hamel's
basin-size caution would consider a defensible "global" result, as opposed to just being
mechanically possible) that was in an earlier draft. Rich's "Yes!" cannot be read as covering a
question he was never asked. This remains a real open question, not just unconfirmed — worth a
separate, explicit ask before treating "global run" as scientifically validated rather than
merely mechanically executable.

## `swy_global` repo studied (2026-09-03) — confirms the routing/parameterization split at the code level

Cloned `github.com/springinnovate/swy_global` (Rich's prior global SWY framework, pointed to
2026-08-21) and read `README.md` + `run_swy_global.py` directly — not a summary from Rich's
description, the actual code. Findings:

- **The TaskGraph/ecoshard mechanism from the section above is now confirmed, not presumed.**
  `run_swy_global.py` imports `ecoshard.taskgraph` and `ecoshard.geoprocessing`, and calls
  `inspring.seasonal_water_yield.execute()` (a NatCap "inspring" package — an extended/research
  SWY implementation, not stock InVEST) once per watershed job.
- **Basin batching is real and already solved, not something to design from scratch.**
  `_batch_into_watershed_subsets()` loops over the global watershed vector (`.shp` files under
  `WATERSHEDS_VECTOR_PATH`, a HydroSHEDS-derived product per `swy_global.ini`), groups small
  watersheds into degree-separated tiles capped at 1000 features per job (large watersheds get
  their own job), reprojects each job to its local UTM zone, and schedules them largest-first for
  parallel execution via `multiprocessing` + `TaskGraph`. Results get stitched back into one
  continuous mosaic via a queue-based worker (`N_TO_BUFFER_STITCH`), with per-job workspaces
  cleaned up once stitching confirms completion. **This resolves the "one global run vs. regional
  mosaic" framing as a false binary** — the tool does both simultaneously: real per-basin
  computation (respecting Hamel et al. 2020's basin-scale validation caution) assembled into a
  seamless global output. Worth stating this precisely in any reply to Rich, not just "seems to
  work."
- **Continuous CN/Kc rasters bypass the lookup table entirely — confirmed at the `model_args`
  level.** `run_swy_global.py` builds one `model_args` dict per job containing *both*
  `lulc_raster_path` + `biophysical_table_path` (the standard InVEST route) *and* optional
  `cn_a_path`/`cn_b_path`/`cn_c_path`/`cn_d_path`/`kc_1_path`...`kc_12_path` overrides, commented
  "these keys are optional rasters that would replace lulc biophysical parameters." This is
  exactly the integration point for this project's plan: precompute CN_A-D and Kc_1-12 rasters by
  blending the GCN250 lookup table with the biome-specific literature corrections (tropical moist
  forest, mangroves, flooded grasslands/savannas), then feed them here directly — no lookup-table
  step needed at all.
- **`calculate_average_monthly_events.py` (the rain-events calculator) uses Google Earth Engine +
  CHIRPS DAILY, not ERA5.** There's a commented-out ERA5 dataset line (`# ('ERA5', ...)`,
  disabled) sitting next to the active CHIRPS entry — meaning **the CHIRPS 60°N/60°S coverage gap
  is NOT resolved by this script as shipped**; ERA5 blending was evidently tried or planned but is
  currently inactive scaffolding, not a working fallback. This answers the open question from the
  2026-08-21 Rich outreach ("does his calculator already blend ERA5 for high latitudes") — no, it
  doesn't, currently. Re-enabling it would need real work, not just an undocumented existing
  feature to switch on.
- **`base_data/biophysical_template_PH.csv`** — a sample biophysical table (Philippines), confirms
  the repo also supports the traditional LULC+table route as a fallback/reference format, useful
  for understanding the expected table schema even though this project plans to use the direct-
  raster route instead.

**Not yet done**: haven't traced through `inspring.seasonal_water_yield.execute()` itself (that
package isn't in this cloned repo — it's a separate dependency, `pip`-installed presumably from
NatCap's own package index or a private index; would need locating separately if the actual
model internals matter, as opposed to just the orchestration layer covered here).

## Open questions / next steps (remaining)

**Update 2026-09-09 — Becky meeting, real decision, supersedes the Hamel-basin-first plan below.**
Test basin is now **Borneo**, not Peru/Myanmar. Two things worth being precise about, since they
change the shape of the plan below rather than just the location:

1. **Becky is not treating the mangrove/flooded-savannas CN gap as a blocker.** The whole
   "resolve the biome patch before testing" framing in this file (and in the Indonesia-question
   thread) was this project's own caution, not a condition she imposed — she's comfortable testing
   without it resolved first.
2. **Rigorous validation-against-published-output is not the priority she cares about here either**
   ("even the validation does not bother that much"). This is a real shift from the reasoning below
   (Hamel's basins chosen specifically *because* they have published output to check against) — the
   Peru/Myanmar validation-test framing, and the Nash-Sutcliffe-efficiency comparison plan, was this
   project's own rigor standard, not hers. Keep the reasoning below for what it's worth
   scientifically, but don't present the Borneo run as still needing to clear that bar unless asked.

**Practical plan from the meeting**: get the data already in hand (CN tables, GCN250, HYSOGs250m,
HydroBASINS — see below) onto a shared Google Drive. Attempt the actual Borneo run in-house first;
Rich runs it if that doesn't work. **Meeting Friday with both Becky and Rich — be ready.**

**What this doesn't change**: the required-inputs list, the CN/Kc strategy (GCN250 + biome patches,
FAO-56 + Kamble NDVI regression), and the `swy_global`/`inspring` architecture findings below are
all still accurate and still needed for Borneo — only the target basin and the validation framing
changed, not the underlying mechanics.

**Update 2026-09-07 — test-basin question resolved (user decision), two open threads reconciled.**
This file had been carrying two different test-basin proposals in parallel without ever explicitly
choosing between them: the Llanos idea below (2026-09-03/04) and the separate Hamel-basin-
replication proposal sent to Becky 2026-09-04 (see the SWY section of `docs/HANDOFF_2026-09-07.md`
— replicate on Hamel's own Peru/Myanmar basin(s), compare against her actual published output).
**Resolved: Hamel's basin(s) first.** Reasoning: Llanos would be a real test-basin run, but there is
no existing SWY output for the Llanos to validate the result against — a coherent trial with nothing
to check it against. Hamel's basins have exactly that (her own published quickflow output), so they
validate the parametrization, not just prove the mechanism runs. Llanos stays a real, wanted next
step — it's still the concrete test case for the flooded-grasslands/savannas CN patch, and
personally motivated for the user — just sequenced after the Hamel-basin validation, not before it.

**Update 2026-09-08 — validation-test year question resolved, simply.** The test year(s) should
just be whatever period Hamel et al. 2020's own original SWY run used for the chosen basin (Peru or
Myanmar) — not 2020, not 1992, not a project convention. Match the validation target exactly, since
the point of this test is a direct comparison against her actual output. Asked Rich directly
(`docs/swy/rich_swy_status_and_asks_2026-09-08.draft.md`) for the exact year(s), alongside the
granular basin output itself. The 2020-vs-2000 decision below still applies separately, but only
once/if this moves beyond the validation test into this project's own multi-temporal SWY run.

**Same update — 1992 NDVI unavailability accepted as a real limitation, not a blocker.** MOD13A3
only covers 2000-present (see "Remaining inputs" below); MODIS didn't exist in 1992. Decided: don't
chase a cross-sensor 1992 substitute (e.g. GIMMS AVHRR) for now. Use **2020** as the anchor year
whenever NDVI is actually needed for this project's own comparative pipeline (matching the other
four services' 2020 snapshot) — separately decide later whether pulling **~2000** (the earliest
MOD13A3 year) as a second time point is worth it, once/if this moves beyond a validation test into
an actual multi-temporal SWY run. Note this doesn't apply to the Hamel-basin validation test itself,
which should use whatever period Hamel et al. 2020's own study covered, not 2020 or 1992 — check her
methods for the actual years before pulling anything for that test specifically.

**Previously (2026-09-03/04, kept for context)**: proposed to Becky, not yet confirmed — start with a real test run on
one representative Colombian Llanos basin rather than jumping to anything global. That scope
changes several items below from "must resolve first" to "doesn't apply at this scale, revisit
before going global": the precipitation-source blend question doesn't need resolving (Llanos is
well inside CHIRPS' coverage, no need for the CHELSA fallback yet), and the Hamel et al.
validation-scale question (still an open ask to Rich, see the section above) doesn't block a
single sub-10,000 km² basin either. What the Llanos scope can't dodge, and shouldn't: it *is* the
flooded-grasslands/savannas biome, so it's the concrete test case for that patch decision, not
something to defer further.

- [x] Download and review NatCap's `kc_calculator.xlsx` tool — done, see above
- [x] Confirm the routing-vs-parameterization architecture — done 2026-09-03, both at the
      `swy_global` code level and by Rich directly (see section above)
- [ ] **Mangrove / flooded-grasslands CN patch — leaning toward "structural limitation," not yet
      formally decided or communicated to Becky.** No usable literature found after three separate
      search angles; both are flood-pulse/tide-dominated systems where CN's local rainfall-runoff
      premise may just not apply (same treatment as GCN250's own "Water/wetlands" PFT row, CN≈100).
      The proposed Llanos test run is the concrete case to resolve this against, not an abstract call.
      **Asked Becky directly instead (2026-09-07, sent)**: whether NatCap's existing Indonesia SWY
      run touches mangroves or flooded/seasonal wetlands — would resolve this gap directly if so,
      without more searching. Not yet answered.
- [ ] Tropical moist broadleaf forest CN patch — this one has real candidate literature (Calero
      Mosquera et al. 2021, Fábrega et al. 2012, pending verification) and is a genuine "build the
      correction" task, unlike the item above.
- [x] **HYSOGs250m acquired — 2026-09-09.** Downloaded the full global, already-reclassified
      Cloud-Optimized GeoTIFF (`HYSOGs250m_Soil_Groups_reclassified.tif`, ~388MB) from NatCap's own
      Data Hub (`data.naturalcapitalalliance.stanford.edu`) rather than the raw ORNL DAAC source
      (DOI 10.3334/ORNLDAAC/1566) — same underlying data (Ross et al. 2018), but pre-formatted for
      InVEST use and no Earthdata login required. Pulled the **global** file, not basin-clipped —
      a one-time global download is simpler than re-fetching per basin; clip locally per-basin as
      needed once a basin is chosen. Saved to `data/raw/soil_hydrologic_group/`.
- [x] **HydroBASINS acquired — 2026-09-09.** Downloaded South America and Asia (all 12
      Pfafstetter levels each) directly from `hydrosheds.org` — the two continents covering every
      candidate basin actually discussed (Peru, Llanos in South America; Myanmar, Indonesia in
      Asia), not a full global pull. Saved to `data/raw/hydrobasins/hybas_sa/` and `hybas_as/`.
- [ ] Acquire MOD13A3 NDVI — **scope to the chosen test basin first**, not a global pull.
- [ ] Resolve the precipitation source (CHIRPS+CHELSA blend vs. single source) with Becky — only
      actually blocking once/if this moves beyond the Llanos test to anything outside CHIRPS'
      60°N/60°S coverage.
- [ ] Derive the rain-events table from daily CHIRPS via `calculate_average_monthly_events.py`
      (confirmed working, Earth Engine + CHIRPS daily) once the Llanos basin boundary is pulled.
- [ ] FAO-56 Kc tables for cropland fraction, re-run with region-correct planting dates
      (not the spreadsheet's Northern-Hemisphere defaults)
- [x] **Locate `inspring.seasonal_water_yield` — done 2026-09-08.** Public GitHub repo,
      `github.com/springinnovate/inspring` (the `Dockerfile`/`setup.py`'s `therealspring/inspring`
      URL is just Rich's old GitHub account name — same org, redirects, not a fork). No PyPI
      package, no NatCap credentials needed. **No maintained install manifest** — the repo's own
      `requirements.txt` was deleted in 2022 and never replaced; the `Dockerfile` is stale (pins
      InVEST 3.9.0, a specific old `ecoshard` fork commit, even a vestigial `torch` dependency).
      Real install path: `git clone` + `pip install .` (ships compiled Cython extensions —
      `seasonal_water_yield_core.pyx` is where the actual routing math lives — needs a C++
      compiler, `cython`, `numpy`, `setuptools_scm` at build time). One thing worth asking Rich
      directly: the Dockerfile pins a specific fork commit of `ecoshard`
      (`therealspring/ecoshard@b9b4580...`), not the stock PyPI `ecoshard` (currently 0.7.0) —
      unconfirmed whether PyPI's `routing` submodule API (`fill_pits`, `flow_dir_mfd`,
      `flow_accumulation_mfd`, `extract_streams_mfd`, `detect_lowest_drain_and_sink`) matches, or
      whether his fork is still required.

## `inspring.seasonal_water_yield.execute()` — full required-inputs list (2026-09-08, code-read)

Several of these were not previously compiled anywhere in this file:

- Always: `workspace_dir`, `dem_raster_path`, `aoi_path`, **`threshold_flow_accumulation`** (stream
  extraction threshold — was in `swy_global.ini`, never listed here), **`alpha_m`, `beta_i`,
  `gamma`** (the three SWY routing-partition parameters), `lulc_raster_path` (conditionally —
  only needed if any biophysical factor isn't directly raster-overridden).
- Unless `user_defined_local_recharge=True`: `et0_dir` (monthly reference-ET rasters, globbed +
  **alphabetically sorted** — filenames must sort Jan→Dec or months get silently misassigned;
  PET is computed inside the model as `PET_m = Kc_m × ET0_m`), `precip_dir` (same sorting
  convention), `soil_group_path` (HSG raster, 1–4).
- Rain events: exactly one of `rain_events_table_path` (CSV), `climate_zone_table_path` +
  `climate_zone_raster_path`, or `user_defined_rain_events_dir` — `swy_global` uses the last.
- Biophysical: `biophysical_table_path` (CSV — code requires a `root_depth` column too, not just
  `lucode`/`CN_A-D`/`Kc_1-12` as the docstring implies) OR direct raster overrides per factor
  (`root_depth_path`, `cn_a/b/c/d_path`, `kc_1...12_path`).
- Also real, previously unlisted: `max_pixel_fill_count` (caps pit-fill flood extent),
  `single_outlet` (forces one lowest-drain/sink pixel as sole outlet — `run_swy_global.py` sets
  this automatically for single-watershed jobs).

**Real finding: `root_depth` is required to satisfy the pipeline but is dead code in this fork** —
traced through `seasonal_water_yield.py` and the `.pyx` core: it's computed but never consumed in
the actual water-balance equations (not passed into `calculate_local_recharge`, not referenced in
quickflow/baseflow routing). Stock InVEST SWY uses root depth for AET; this variant doesn't appear
to have it wired in yet. Practical implication: still need *some* root-depth input to avoid an
error, but it's low-priority to source carefully — its values currently don't affect output.

**CN/Kc resolution/alignment question, resolved precisely — this was a real open question, now
answered**: **no, CN/Kc/root_depth override rasters do NOT need to be pre-aligned to the job's
grid.** `inspring`'s own `_reclassify_or_clip()` automatically warps any supplied override onto
the DEM's aligned grid via bilinear resampling — unconditional, regardless of the `prealigned`
flag. (Worth knowing: bilinear on curve number, a bounded 0–100 quantity, will blur values across
LULC-class/soil-group boundaries — not nearest-neighbor, which might be expected for a categorical-
origin variable.) Separately, `run_swy_global.py`'s per-basin worker always sets
`prealigned=True` and does its own complete pre-warp of every input to the watershed job's local
CRS before calling `execute()` — so in the actual `swy_global` pipeline, `inspring`'s internal
CN/Kc warp is a redundant-but-harmless second pass; it's the load-bearing mechanism only if
`inspring.seasonal_water_yield.execute()` is ever called directly, without the `swy_global`
wrapper.

**Guswa et al. 2018 confirmed at the code level, not just by citation-inference**:
`_calculate_monthly_quick_flow()` implements the exact closed-form stochastic-rainfall quickflow
expression (`Si = 1000/CN − 10`, mean rain depth per event, `scipy.special.expn(1, ...)` for the
exponential-integral E₁ term) — Guswa's analytical solution under exponentially-distributed daily
rainfall, matching InVEST User Guide Eq. [1].

Clone paths (session-scoped scratch, will not persist — cheap to re-clone from
`github.com/springinnovate/{swy_global,inspring}` if gone):
`.../scratchpad/swy_research/swy_global` and `.../scratchpad/swy_research/inspring`. Files worth
returning to directly: `inspring/src/inspring/seasonal_water_yield/seasonal_water_yield.py`
(`execute()`/`_execute()`, `_reclassify_or_clip`), `seasonal_water_yield_core.pyx` (the Cython
`calculate_local_recharge`/`route_baseflow_sum` MFD-routing kernels), `swy_global/run_swy_global.py`
(~line 640-805 for the per-basin pre-warp worker, ~873-912 for `model_args` construction).

## Sources consulted so far

- InVEST SWY User Guide: https://storage.googleapis.com/releases.naturalcapitalproject.org/invest-userguide/latest/en/seasonal_water_yield.html
- InVEST Appendix 1 (data sources/parameter guidance): https://storage.googleapis.com/releases.naturalcapitalproject.org/invest-userguide/latest/en/data_sources.html
- Kamble, B., Kilic, A., et al. (2013). Estimating Crop Coefficients Using Remote Sensing-Based Vegetation
  Index. *Remote Sensing*, 5(4), 1588-1602.
- Allen, R.G., et al. (1998). Crop evapotranspiration - Guidelines for computing crop water requirements.
  FAO Irrigation and Drainage Paper 56.
- NRCS-USDA (2007). National Engineering Handbook.
- NRCS TR-55 (1999). Urban Hydrology for Small Watersheds.

## 2026-09-09 — Borneo pivot, decided with Becky

Real pivot at the 13:30 call: test basin is **Borneo**, not Peru/Myanmar (the original
Hamel-basin validation plan) or the Colombian Llanos. Becky isn't treating the mangrove/
flooded-savannas CN gap or rigorous validation-against-a-published-result as blockers for this
round — both were this project's own rigor standard, not a condition she set. Meeting with Becky
and Rich together set for Friday 2026-09-12. The stricter Hamel-basin validation framing is kept
as a reference, explicitly not abandoned, for a later round — see `swy_methods.qmd`'s "Test
design" section.

## 2026-09-10 — full raw-input acquisition, a real architecture correction, and documentation restructuring

**Every raw input SWY needs is now downloaded and content-verified** (not just "request says
done" — actually opened and sanity-checked with `rasterio`/`geopandas`), closing out what had been
the last real acquisition gaps:

- **DEM**: SRTMGL3 (`SRTMGL3_NC.003`, 90m, via AppEEARS) — the earlier call to not source a DEM
  independently was about not duplicating Rich's *global* one; it doesn't hold for an in-house
  Borneo-only attempt, which needs its own AOI-scoped file. Verified: range -91–4041m against Mt.
  Kinabalu's real 4095m high point — a close, correct match. 149MB.
- **Precipitation, 2020**: CHIRPS v2.0, `data.chc.ucsb.edu`, no auth, global monthly GeoTIFFs
  (~14.5MB/month) clipped locally with the project's own geopandas/rasterio (CHIRPS isn't
  NASA-distributed, not reachable via AppEEARS). Verified: July mean 138.9mm, plausible for
  equatorial Borneo.
- **Reference ET0, 2020**: **TerraClimate `pet`** — a genuinely new source, not previously
  vetted anywhere in this project before today, needed because CHIRPS is precipitation-only.
  "Reference Evapotranspiration," ASCE Penman-Monteith corrected for CO2, ~4km
  (`thredds.northwestknowledge.net`, no auth, one global netCDF per year). Verified: monthly means
  97–118mm, plausible for tropical reference ET. Citation added: @abatzoglou2018 (Crossref-verified
  DOI `10.1038/sdata.2017.191`).
- **NDVI**: MOD13A3.061, both a 2020-only task and the full 2000–2026 record submitted and
  verified (636 GeoTIFFs for the full record). The full record is kept locally
  (`data/mod13a3_borneo_full_record/`) but deliberately **not** shipped in the shared Drive
  package — package scope is one test year, to stay manageable; 2020 was chosen to match the
  Copernicus C3S LULC anchor year and as a reasonable guess (unconfirmed) for the year the other
  8 pre-computed services actually used.

**A real correction to the CN/Kc plan**, found by re-reading this file's own 2026-09-08 code-read
entry above more carefully: `inspring.seasonal_water_yield.execute()` accepts CN and Kc as
**direct raster overrides** (`cn_a/b/c/d_path`, `kc_1...12_path`, `root_depth_path`), not only via
a lucode-indexed biophysical CSV — and `lulc_raster_path` itself becomes unnecessary once every
factor is raster-overridden. Since GCN250 and the Kamble NDVI regression are both inherently
per-pixel, the plan is now to build CN/Kc as rasters and feed them through that path directly,
rather than force either through a shared lucode scheme. This shrinks the earlier "no lucode
master table exists" gap substantially — only a small 3-way land-cover mask
(crop/non-crop-veg/non-vegetated) is actually needed for the Kc split, not a pipeline-wide table.
Worth being honest about what is and isn't novel here (a question the user raised directly):
continuous, gridded parameterization is the default for most physically-based distributed
hydrology models generally; it's InVEST's table-based convention that's the special case, suited
to its usual land-use-scenario comparison use case. Whether this raster-override path is a
pattern used elsewhere or bespoke engineering Rich built for his own global-run ambitions is an
open question, not yet asked of him directly.

**Real gotchas hit today, worth keeping**:
- AppEEARS's bundle-file download endpoint (`/api/bundle/{task_id}/{file_id}`) returns an HTTP
  redirect to a pre-signed S3 URL — `curl` needs `-L` or it silently saves the tiny HTML redirect
  page instead of the real file (a uniform ~3.3KB file size across "downloaded" files is the
  giveaway).
- Calling `curl` from *inside* a Python `subprocess` picked up a different, Windows-native
  `curl.exe` that couldn't read `.netrc` (return code 26) — had to call Git-Bash's `curl` directly
  from bash, not wrapped through Python subprocess.
- `Rscript`/`sf`/GDAL segfaults reading `borneo_aoi.gpkg` in this environment (same underlying
  crash noted in `project_phase4_status.md` memory's Session 18 entry) — used the project's
  Python `.venv` (`geopandas`) instead, which worked cleanly.
- One transient run where every downloaded filename picked up a stray trailing underscore
  (`....tif_`) — did not reproduce on retry with the identical script; root cause not identified,
  not currently a live issue.

**Documentation restructured today**, after the user flagged real fragmentation risk (three
different places were tracking "current status" in overlapping ways):
- `docs/swy/workflow.md` (new) — a mermaid pipeline diagram with live status coloring, now the
  one place "current status" should be tracked. Other docs point to it rather than duplicating it.
- `docs/swy/model_specification.md` archived to
  `docs/archive/model_specification_2026-07-16.md` (not deleted) and replaced by
  **`docs/swy/swy_methods.qmd`** — a permanent conceptual/methods reference, deliberately not tied
  to any single meeting's framing (unlike the old file, which was explicitly written "for Becky,
  Tuesday" and unlike `docs/reports/swy_status_report.qmd`, which stays a disposable, meeting-tied
  status memo, regenerated fresh rather than kept permanently current). Includes real typeset
  formulas (the Kamble Kc regression, $ET_c = K_c \times ET_0$), the raster-override architecture
  note above, and an open-questions section.
- **`docs/swy/references.bib`** (new) — 22 entries, generated from `literature_review.ris` via
  `pandoc -f ris -t biblatex` (confirmed: Quarto/pandoc can read `.ris` bibliographies directly,
  no conversion is strictly required — but RIS's auto-generated citation keys are unwieldy
  multi-author strings, e.g. `Hamel_Valencia_Schmitt_Shrestha_Piman_Sharp_Francesconi_Guswa_2020`,
  and two USDA entries collided on `United_*` — hand-cleaned to short keys like `hamel2020` after
  conversion). Two new sources added and Crossref-verified: `abatzoglou2018` (TerraClimate) and
  `farr2007` (the SRTM mission paper — was previously used in this project without ever being
  formally cited). Keep `.ris` and `.bib` in sync going forward: `.ris` is the reference-manager
  master, `.bib` is what Quarto actually cites from.

**The shared Drive package** (`data/swy_shared_package/`) grew from 892MB to ~2.1GB (adding
DEM/NDVI/precip/ET0) then back down to **~1.1GB** after deliberately trimming NDVI to 2020-only.
README.md fully rewritten with the complete data dictionary. **Uploaded to Drive and Becky
notified, 2026-09-11** (user's own action, confirmed done).

**Not yet done**: the CN/Kc rasters themselves (masking + regression + merge logic — all inputs
are in hand, this is implementation work now, not acquisition), the rain-events derivation
(script exists, CHIRPS is in hand), and the actual `inspring` model call.

## 2026-09-10/11 — first real run attempt: local LULC turned out corrupted, Docker path found, run in progress

**Real data-integrity finding**: `data/raw/LandCovers/landcover_gl_1992.tif`, an input this
project's other pipeline apparently depends on, turned out to be an **empty/corrupted stub** —
zero valid pixels found anywhere on Earth (checked Borneo, the Amazon, Colombia, the Congo — all
NaN), despite having correct-looking metadata (CRS, bounds, dtype). The user had removed the
real data for storage reasons at some point. Real gotcha worth generalizing: verify raster
*content*, not just file existence/metadata, before trusting an "this input already exists"
assumption. The real 2020 Copernicus C3S land cover (the year actually needed here, not 1992)
was located on the user's WWF OneDrive as a 2.3GB netCDF and extracted directly via a windowed
read (no need to copy the full global file) — see `Python_scripts/swy_borneo_run/06_extract_lulc_2020.py`.
Real validation: 48.1% valid fraction (matches every other Borneo raster exactly), and a
physically sensible class breakdown (58.3% broadleaf evergreen forest, real mangrove/swamp-forest
and oil-palm-consistent cropland fractions) — the C3S data and the clip are both correct.

**GCN250 gap found**: only the derived CN lookup CSVs were ever downloaded, never the actual
GCN250 per-pixel raster — irrelevant for this run (the CSV path was used, see below) but a real
gap if the raster-override path is attempted later.

**Docker/`inspring` investigation — tried Rich's own path first, per explicit user instruction,
before falling back.** Concrete, reproducible findings (not vague "it didn't work"):
1. `inspring`'s own `Dockerfile` fails immediately and reproducibly on `pip3 install -r
   requirements.txt` — that file was deleted from the repo in 2022 and never replaced. Confirmed
   by actually running the build.
2. `inspring`'s `setup.py` omits `inspring.seasonal_water_yield` from its `packages` list — a
   normal `pip install .` compiles the Cython extension but leaves the Python module itself
   unimportable. **Worked around**, not fixed upstream: `python setup.py build_ext --inplace` +
   `PYTHONPATH` pointed directly at the source tree.
3. **Good news**: the *current* upstream `ecoshard` (not the old pinned
   `therealspring/ecoshard@b9b4580` fork commit) has a working `ecoshard.geoprocessing` (with
   `routing`) and `ecoshard.taskgraph` — resolves the open question flagged 2026-09-08 about
   whether the old fork was still required. It isn't.
4. This project's *own* existing Docker image (`therealspring/global_ncp-computational-
   environment`, built from the repo's own `Dockerfile`/`environment.yml`) already had everything
   `inspring` actually needs (gcc/g++/cython/GDAL, a fresh unpinned `ecoshard`) — building
   `inspring` on top of that image (rather than debugging `inspring`'s own broken one) is what
   actually worked. `natcap.invest` itself was also tried directly via pip as a possible fallback
   and failed for an unrelated, more fundamental reason: **GDAL has no PyPI wheels on any
   platform** (confirmed on both native Windows and a plain Linux container) — this is a
   well-known GDAL-maintainer decision, not an environment bug. `conda-forge` (via
   `condaforge/miniforge3`) installs `natcap.invest` + `gdal` cleanly with no compilation, and
   would have been the fallback path if the `inspring`-on-existing-image approach hadn't worked.
5. Items 1 and 2 are small, well-scoped, non-scientific packaging fixes — real candidates for a
   PR back to `springinnovate/inspring`, not attempted yet (would need an actual GitHub fork,
   deferred until after the immediate Borneo test).

**Whole pipeline consolidated into real scripts**, not just interactive commands — see
`Python_scripts/swy_borneo_run/README.md` for the full pipeline table and known compromises in
this specific run (placeholder rain-events table, NDVI-regression Kc applied uniformly including
cropland, placeholder `root_depth`, CSV biophysical-table path rather than raster-overrides).

**Run submitted, outcome not yet known as of this entry** — check `docker ps`/the run's actual
output before assuming either success or failure.

## 2026-09-11 — the run actually finished (crash was infrastructure, not the model): real, mostly plausible results, one genuine diagnosable issue

**What actually happened**: the laptop went to sleep mid-run, which killed Docker Desktop's
backend (`error waiting for container: unexpected EOF`), which the harness reported as the run
"failing" (exit code 4). That's a real, legitimate confusion to have from the task notification
alone — but checking the actual workspace directory tells a very different story: **every
standard SWY output file exists and is non-empty**, including the 12 monthly `qf_*` rasters, AET,
CN, `L`/`L_avail`/`L_sum`/`L_sum_avail`, `Vri`, and — critically — the final
`aggregated_results_swy_borneo_2020_test.shp` with a real, non-null computed baseflow value
(`qb = 1705.005`, `vri_sum = 0.998`). Aggregation is one of the model's last computational steps,
so this strongly indicates the actual scientific computation completed; what died afterward was
some post-computation Python/taskgraph cleanup step, hit right as Docker's Linux backend
disappeared out from under it (the traceback that actually landed in the log is 100% thread-join/
deprecation-warning noise from an unclean interpreter shutdown, not a computation error).

**Real content check, not just "files exist"**:
- **QF (annual quickflow)**: clean stats (excluding nodata and 14 stray fill-value pixels out of
  182M, a negligible 0.00% artifact) — mean 406.8mm/year, range 2.3–4837.2mm. Plausible for
  tropical rainforest, where most precipitation infiltrates rather than running off quickly.
- **AET (actual evapotranspiration)**: mean 1288.0mm/year, range up to 1566mm. Squarely in the
  expected range for warm, humid, year-round tropical conditions.
- **L_sum (local recharge, accumulated downstream)**: a real, genuine anomaly — while the median
  (4646.87) and most of the distribution look reasonable, ~4.6% of valid pixels (1.76M of 38.3M)
  have runaway values into the millions/billions (p99.9 = 812M, max = 10.86B). This is the
  classic signature of a flow-accumulation blow-up — almost certainly an unresolved DEM
  sink/pit-fill issue or a flow-routing anomaly interacting with the placeholder rain-events
  table, not random file corruption (the bulk of the distribution is fine; this is localized).
  **Worth investigating before trusting L_sum-derived outputs (including the aggregated `qb`)
  quantitatively** — but doesn't change the headline finding that the pipeline runs end-to-end
  and QF/AET look right.

**Bottom line for tomorrow's meeting**: the actual answer to "does it run, and is our
parametrization plausible" is a real yes on both counts, with one specific, honestly-flagged
follow-up item (the L_sum anomaly) rather than a clean, unqualified success — which is a more
credible thing to bring to Becky and Rich than either "it's perfect" or "it crashed."

**Also worth logging**: `data/swy_borneo_workspace/` (gitignored) has the full output set if
anyone wants to inspect it directly — not copied into the shared Drive package (output, not
input; the package is scoped to inputs per its own README).

## 2026-09-11 (later) — rerun completed cleanly; the L_sum anomaly is real, not a crash artifact

Fixed the actual sleep cause before rerunning: `powercfg` showed AC sleep was already disabled,
but DC (battery) sleep was still on a 1-hour timer — almost certainly what caught the first run,
since it ran close to an hour before dying. Set `standby-timeout-dc 0` and, as a second layer,
started a `SetThreadExecutionState`-based active keep-awake process for the rerun's duration (the
same mechanism video players use — works under most managed-device policies since blocking it
would break legitimate business software). Moved the incomplete first workspace aside
(`data/swy_borneo_workspace_INCOMPLETE_run1_2026-09-10/`, preserved not deleted) before
rerunning, since `taskgraph`'s file-existence-based caching could otherwise have silently reused
the incomplete `L_sum` instead of genuinely recomputing it.

**The rerun completed cleanly (exit code 0, no crash).** Real, decisive results:

- **QF and AET are byte-identical to the first run** (mean 406.8mm/yr and 1288.0mm/yr
  respectively) — expected, since neither depends on the routing step that got interrupted before.
- **`L_sum` now has full spatial coverage** — 48.2% valid fraction matching the stream/quickflow
  rasters exactly, full latitude range (was cut off at the equator before). Confirms the crash
  really was the cause of the earlier coverage gap.
- **The flow-accumulation anomaly persists at the same order of magnitude with full coverage**:
  5.36% of valid pixels > 1,000,000 (vs. 4.58% in the incomplete run) — this is the real finding.
  It is **not** an artifact of the interrupted run; it reproduced independently once there was
  full coverage to check it against. Visually (via the interactive map — genuinely useful here,
  not just presentational), the clearest concentration sits at one specific river-mouth/estuary
  location on the island's east coast, not random scatter — consistent with a real hypothesis
  (SRTM's known difficulty resolving flat, tidally-influenced coastal terrain), not confirmed.
- **The aggregated `qb` (baseflow) value came out numerically identical between the incomplete
  and complete runs**: 1705.005127 both times. This is genuinely informative: it suggests the
  AOI-wide aggregate was already robust to the coverage gap (plausibly because the outlet-level
  routing calculation was already complete in-memory before the crash, even though the full
  raster hadn't finished flushing to disk) — worth being precise that this doesn't mean the
  *pixel-level* L_sum anomaly is resolved, just that the one aggregate number tested didn't
  visibly depend on it.

Status report (`docs/reports/swy_status_report.qmd`), its embedded interactive map, and the
shared Drive package have all been updated and re-rendered to reflect the complete run — the
version uploaded to Drive last night reflects the *incomplete* run's framing and should be
re-uploaded.

## 2026-09-11 (later still) — Becky/Rich meeting outcome: Philippines comparison, data reorg

Meeting with Becky and Rich happened, went well. Real outcome: Becky couldn't locate the raw
Borneo/Malaysia input data on her end, but did find and share an existing completed SWY model-run
workspace for the Philippines (`baseline_historical_climate`, exact year unconfirmed — assuming
2020 pending her reply). Plan: replicate this project's own from-scratch Borneo approach for the
Philippines too, then compare against her shared output as a first real external-benchmark data
point. Downloaded to `data/swy/philippines/rich_shared/` (after the reorg below).

**AOI-building false start, worth recording since it changes how future "use Rich's own output as
a template" attempts should be done.** First approach: dissolve the shared workspace's
`watershed_subset_files/*.gpkg` (the exact HydroBASINS/HydroSHEDS routing units `swy_global` used
for this job) into one polygon — same spirit as reusing Rich's own computational domain rather than
re-deriving a Philippines boundary independently. Got a ~439,000 km² polygon, ~46% over the
Philippines' real ~300,000 km² land area. Checked why rather than shrugging it off: the largest
single piece of that polygon (~118,000 km², bounds ~114–119°E/2.7–7°N) sits in northern Borneo, and
directly overlaps this project's own `borneo_aoi.gpkg` by a similar amount. Verified directly
against the actual output raster (`QF_wwf_PH_baseline_historical_climate.tif`) that this Borneo
chunk has **zero valid pixels** — it's upstream contributing-basin area the router needed for
physical correctness, never part of the retained output. **Lesson: the routing domain
(`watershed_subset_files`) and the retained-output footprint are different things — build a
comparison AOI from the output raster's own valid-data mask (polygonized), not from the routing
domain.** Corrected `Python_scripts/swy_philippines_run/01_build_aoi_from_rich_mask.py` to do
exactly that (decimated read of `QF.tif`'s non-nodata mask, ~20x downsample for tractability, then
polygonize + dissolve) — corrected area **284,981 km²**, 5% under real land area, very plausible
for an archipelago at ~600m polygon fidelity (small islands lost to decimation). The original
routing-domain polygon is kept, not deleted, as
`data/swy/philippines/inputs/ph_routing_domain_reference.gpkg`, in case the actual upstream routing
context matters later (this project's own new PH run won't need it — Philippine watersheds are
self-contained per-island, unlike Borneo needing Malaysia/Indonesia mainland context).

**A same-day "free" validation idea was tried and explicitly did not pan out**: since the routing
domain showed real overlap with the Borneo AOI, tested whether this project's own completed Borneo
run could be directly compared against Becky's PH workspace in that overlap zone, with zero new
downloads. Built and ran the comparison — but per the finding above, that overlap zone is exactly
the chunk with zero valid PH output pixels, so there was nothing real to compare (12.9M valid
Borneo pixels vs. 62K validless/near-empty PH pixels in the same nominal zone — the small residual
PH pixel count that did print before this was diagnosed traces to boundary/edge polygons, not real
interior coverage). Deleted the one-off script once the underlying premise was confirmed false
rather than leave a permanently-failing assertion in the repo; the finding itself is recorded here
and in `01_build_aoi_from_rich_mask.py`'s own docstring. **No shortcut exists — a genuine Philippines
comparison needs this project's own independent PH run** (new DEM/NDVI/precip/ET0/LULC downloads +
its own `inspring` call), the same lift as Borneo.

**Data reorg, same session, user-requested**: `data/` had become cluttered with SWY-specific
top-level folders (`swy_shared_package/`, `borneo_lulc/`, `dem_borneo/`, `chirps_borneo_2020/`,
`terraclimate_borneo_2020/`, `mod13a3_borneo_full_record/`, `swy_borneo_workspace/`, plus the new
Philippines folders) sitting alongside this project's other-service data. Consolidated all of it
under `data/swy/{shared,borneo,philippines}/` — `shared/` for genuinely cross-region products
(`cn_tables/`, `soil_hydrologic_group/`, `hydrobasins/`), `borneo/{inputs,raw_downloads,lulc,
workspace}/`, `philippines/{inputs,rich_shared}/`. Every path reference in
`Python_scripts/swy_borneo_run/` and `swy_philippines_run/` updated and re-verified against the new
layout (`01_build_aoi.py` for Borneo, `01_build_aoi_from_rich_mask.py` for Philippines both rerun
successfully post-move). Also fixed a real, previously-unnoticed bug while at it:
`07_build_biophysical_table.py`'s `NDVI_DIR` pointed at `data/mod13a3_borneo_2020_verify`, a path
that no longer existed (the actual 2020 NDVI+QA files live in the `mod13a3_ndvi_2020/` folder
inside the shared inputs package) — would have failed on any fresh reproduction attempt. Fixed to
point at the correct location as part of the same path update.

**Still genuinely open, not yet in this project's control**: the Philippines run's actual year
(asked Becky, no answer yet — proceeding on the assumption of 2020, matching this project's other
anchor-year conventions, until she confirms). Whether a single year or a time series is worth
pulling is also open, same open-ness as the earlier 2020-vs-2000 Borneo question.

## 2026-09-11 (evening) — AppEEARS API blocked mid-session; Rich raises a real Kc gap; PH baseline turns out to be a 30-year CMIP6 climatology, not a year — significant reframe needed

**AppEEARS API access broke between yesterday's Borneo fetch and today's Philippines fetch, same
account, same script.** `GET`/`POST /task` and `GET /bundle/{id}` all return a generic 403
("read-protected or not readable") with `X-Cache: Error from cloudfront` in the response headers —
CloudFront intercepting before the request reaches AppEEARS' own app. `GET /product` (metadata)
works fine throughout, ruling out a login/credentials problem. Best-supported explanation, not
confirmed from outside AWS's own logs: a bot/WAF rule on the CloudFront distribution targeting the
`/task` and `/bundle` paths specifically, likely keyed on `python-requests`'s non-browser request
signature rather than a real account quota — the browser UI worked immediately once file-format
issues were sorted out. **Workaround that actually worked**: AppEEARS' web UI has a "load a
request" file-upload control that, despite an unhelpful `.geojson`-extension/legacy-`crs`-field
rejection at first, accepts a **complete request JSON** (the same `task_type`/`params.dates`/
`params.layers`/`params.geo`/`params.output` structure the API itself expects) and submits it as a
normal browser-authenticated request. Built these by hand for the DEM and NDVI requests
(`data/swy/philippines/inputs/ph_dem_full_request.json`, `ph_ndvi_full_request.json`) — both
processed normally once submitted this way. **Also learned**: submitting the AOI as its full
~115-part, ~96,000-vertex multipolygon produced a 4-6MB request that failed regardless of upload
path — not the actual cause of the 403 (confirmed separately, since even a 539-byte bounding-box
geometry got the same 403 via the API), but worth remembering for future AppEEARS submissions:
**use the bounding box for the AppEEARS request geometry**, clip precisely to the real AOI locally
afterward (already how `02_fetch_dem.py`/`03_fetch_ndvi.py` were rewritten). DEM and full NDVI
(12 months, NDVI+QA) both landed this way; LULC extraction (needs no AppEEARS access, just the
OneDrive C3S netCDF) ran cleanly in parallel. Philippines composition, for the record: 32.7%
broadleaf evergreen forest (class 50), ~40% combined mosaic cropland/natural-vegetation (classes
30+40), 15.0% herbaceous cover, 7.2% rainfed cropland, small mangrove/water/urban shares — notably
**class 20 (irrigated/post-flooding cropland — the CN table's distinct paddy category) doesn't
appear in the top classes**, meaning real paddy area is likely folded into the mosaic classes
rather than tagged distinctly; worth flagging to Becky rather than assuming class 20's absence
means no paddy.

**Rich raised a real, well-grounded Kc objection, live, while all this was happening** — asked
directly whether Kamble et al. (2013)'s regression is valid for grassland/forest/tropical forest,
not just agriculture, and named the actual mechanism: NDVI saturates in dense canopy, so a linear
fit calibrated on agricultural NDVI ranges could underestimate Kc once extrapolated to saturated
forest NDVI. Checked rather than reassured him blind:

- **Kamble et al. (2013) confirmed agriculture-only** (US High Plains cropping systems) via two
  independent web searches — direct PDF fetch blocked everywhere tried (MDPI, ResearchGate, UNL
  repository all 403'd WebFetch). The project's own "corroborated by multiple follow-up studies
  through 2025" line in `swy_methods.qmd` turned out to cite exactly one specific study by name
  (`grapevine_kc_2025`) — another agricultural crop, not independent forest evidence. That framing
  was an overstatement, now corrected in the doc.
- **Corbari et al. (2017)** (PDF obtained by the user, read in full) measured real Kc at four
  eddy-covariance sites (pasture, deciduous forest, evergreen conifer forest — Black Hills SD,
  none tropical). Finding: measured forest Kc came in **lower** than FAO/LAI-based assumptions
  (evergreen Kc_mid≈0.17–0.20, deciduous≈0.43–0.51, vs. FAO's ~0.78–0.9) — the opposite direction
  from Rich's underestimation hypothesis, though not tropical, so it doesn't settle the question.
  Notably, even the highest real-measured forest value here is less than half what the Kamble
  regression predicts at forest-level NDVI (~0.85–0.90 → Kc≈1.07–1.14) — a real, sizeable
  discrepancy worth flagging regardless of which direction turns out to be right.
- **Glenn et al. (2011)** (PDF obtained by the user, read in full) confirms Rich's saturation
  mechanism directly and independently: NDVI saturates around LAI≈3, EVI/SAVI don't, and EVI is
  established in this literature as the better-correlated choice for dense canopy specifically
  because of this — several cited studies found MODIS EVI significantly outperforming MODIS NDVI
  against ground-measured ET. **Cites an actual tropical rainforest precedent**: Juarez et al.
  (2008), Amazonia, MODIS EVI + net radiation, r²=0.72–0.86 against flux towers — not yet obtained
  or read, the next concrete source to chase. General accuracy bound from this literature:
  flux-tower-calibrated Kc-VI methods run 10–30% RMSE against measured ET (vs. 5–10% for
  lysimeter-calibrated single crops).
- **Decision, not yet implemented**: switch the Kc pipeline from NDVI to EVI — no new data source
  needed, `_1_km_monthly_EVI` is a layer in the same MOD13A3.061 product already fetched for both
  Borneo and the Philippines (confirmed against AppEEARS' own product catalog). Doesn't fully
  resolve the tropical-forest calibration gap by itself (EVI reduces saturation, it doesn't supply
  a validated tropical Kc-VI formula), but it's a concrete, well-supported, low-cost improvement,
  distinct from the deeper "read Juarez 2008 and find an actual tropical calibration" task.
- **Bottom line, stated to Rich directly rather than softened**: the mechanism he raised is real
  and independently documented; direction and size of its actual effect on Borneo/Philippines
  output specifically is still unknown. Current Borneo forest Kc/AET numbers should not be
  presented as validated.

**Separately, the original validation plan hit a different, unrelated wall**: asked Rich directly
for the Philippines baseline's year; his answer was that it isn't a year at all — it's a **30-year
CMIP6 multi-model average (1984–2014)**, per his own `fetch_precip_scenarios.bat`
(`github.com/springinnovate/wwf-sipa`), matching how CMIP6 ensembles are conventionally treated
(averaged across models, then across time). This project's Borneo and Philippines runs both use
real single-year 2020 data (CHIRPS, TerraClimate, MODIS) — **not directly comparable to Becky's
output as currently built**, a distinct problem from the Kc question above, not a variant of it.
Real options, none yet chosen: (a) present the 2020 runs as a second, honestly-labeled data point
rather than a validation; (b) rebuild a genuine climatology (CHIRPS/TerraClimate averaged
1984–2014; NDVI/EVI can only be averaged 2000–2014, MODIS doesn't go back further — a documented
compromise, not a true match); (c) ask Becky/Rich for their actual precip/ET0 input rasters
directly, cheaper than re-deriving their CMIP6 averaging from scratch. Message drafted to Rich
covering both threads plainly (`docs/swy/message_draft.md`), user reviewing/sending personally
rather than this session sending it.

**Net effect: this session's original "replicate Borneo's approach for the Philippines, compare
to Becky's output" plan needs a real reframe**, not just a status update — both the Kc methodology
and the comparison target turned out to be less settled than assumed going in. User is re-reading
`swy_methods.qmd` in full before deciding the next concrete steps; not decided as of this entry.

## 2026-09-14 evening — Becky's `INPUTS_SP` folder in hand; the isolated-variable comparison actually built and run

Resolves option (c) from the entry above: asked Becky directly for her real Philippines baseline
inputs rather than re-deriving her CMIP6 climatology from scratch. She delivered more than asked —
her real biophysical table, her CN rasters (the raster-override path this project had only
theorized about), her real ET0 product (Global-AI_PET_v3, not TerraClimate), and real
spatially-distributed monthly rain-event rasters (not a flat placeholder). Landed as
`data/swy/philippines/INPUTS_SP/` (renamed from `INPUTS` — Windows case-insensitivity collided with
the existing `inputs/`).

**First real finding, confirmed not assumed**: her LULC (`ph_baseline_lulc_md5_7f29da.tif`) is a
custom 12-class WWF-SIPA typology (Annual Crop, Brush/Shrubs, Built-up, Closed Forest, Fishpond,
Grassland, Inland Water, Mangrove Forest, Marshland/Swamp, Open Forest, Open/Barren, Perennial
Crop) — not ESA CCI, the scheme this project's own CN table is keyed to. Built a hand-matched
crosswalk (by land-cover semantics, not by numeric closeness to her own CN values) to still use
`gcn250_esa_lc_cn_table.csv` unmodified — full table in `swy_methods.qmd`.

**Second real finding, this one substantive rather than mechanical**: building her Kc via this
project's own EVI-regression method (rather than her flat per-class values) gives Closed Forest
Kc = 0.52–0.70 (monthly-varying) against her flat 1.0 — a large gap, and one that lands in the same
direction as Corbari et al. (2017)'s real forest Kc measurements discussed above. Not a validation
of either number (Kamble's regression is still cropland-calibrated), but independent evidence
pointing the same way as the one real measurement source already reviewed.

**A real documentation error was also caught and fixed today, unrelated to the Philippines work
itself but found while re-reading `swy_methods.qmd` to plan this comparison**: the document
claimed `inspring` supports direct raster overrides for CN and Kc (`cn_a/b/c/d_path` etc.),
"confirmed by reading the code directly." Re-reading `seasonal_water_yield.execute()`'s actual
source (a cached clone from an earlier session, same commit this project's own Dockerfile builds)
found no such parameters anywhere in the function — CN/Kc come only from the lucode-indexed CSV
table. That earlier claim was simply wrong. Corrected throughout `swy_methods.qmd`. Interesting
possible origin, not confirmed: Becky's own `.ini` for her Philippines run *does* use
`CN_A_PATH`...`CN_D_PATH` keys — but her `.ini` also uses several other parameter names
(`TARGET_PIXEL_SIZE`, `SOIL_HYDROLOGIC_MAP`, `MONTHLY_ALPHA` as a literal float rather than a
boolean) that don't match this project's actual `inspring` build either, so her workflow is most
likely a different wrapper or fork, not evidence this project's own build ever had the feature.

**Getting the actual run to execute surfaced three more real, unrelated problems**, all found by
just running it and reading the traceback rather than by inspection in advance — worth recording
because each easily could have been misdiagnosed as "the method is wrong" instead of "the software
has a bug" or "the inputs need a preprocessing step":

1. Two genuine upstream `inspring` bugs, both in the `user_defined_rain_events_dir` path (used to
   ingest Becky's real spatially-distributed rain events instead of a flat placeholder) — neither
   looks like it has ever actually been exercised end-to-end before. First: `interpolate_list` is
   sized from `input_align_list` *before* the rain-events block extends that same list by 12 more
   rasters, causing an immediate length-mismatch crash at the alignment step. Patched in
   `Python_scripts/swy_borneo_run/Dockerfile` (a small `python -c` source patch applied during the
   image build, same pattern already used there for the `setup.py` packaging bug). Second, found
   only after the first was fixed: `n_events_path_list` is built via bare `os.listdir()` (no
   directory prefix), unlike the precip/ET0 lists two lines above which build full paths — and
   that same bare-filename list is reused as both the alignment *input* and *output* target. Not
   patched — a naive fix would have `align_and_resize_raster_stack` write reprojected rasters back
   out on top of Becky's own original delivered files. Fixing it properly means giving the aligned
   copies genuinely separate output paths — real feature-completion work, deliberately not rushed.
   **Reverted to this project's own flat 18-events/month placeholder table for this run** — the
   real per-month upgrade stays a documented, open follow-up, not silently dropped.
2. CRS mismatch: her LULC is EPSG:32651 (UTM 51N, meters); the DEM/precip/ET0/soil-group are all
   WGS84 (degrees). `align_and_resize_raster_stack` doesn't reproject across CRSs on its own — it
   intersects each input's raw bounding box as reported in its own native CRS, which fails
   outright when one input is in meters and the rest in degrees ("Bounding boxes do not
   intersect", from a traceback that doesn't mention CRS at all — would have been a genuinely
   confusing thing to debug blind). Fixed by reprojecting her LULC to WGS84 first (GDAL Warp, mode
   resampling to preserve categorical values, matched to the DEM's own pixel size).
3. ET0 filename casing bug: her `Global-ET0_v3_monthly_tifs/` mixes `et0_v3_0X.tif` (most months)
   and `et0_V3_0X.tif` (May–Sept) casing. `execute()` sorts filenames as plain strings to infer
   calendar order — ASCII sorts capital `V` before lowercase `v`, so passed through raw, ET0 would
   have silently landed on the wrong months with no error at all. Fixed via a normalized staging
   copy, not by editing her delivered files.

Also decided, at the user's explicit direction rather than as a default assumption: use this
project's own already-fetched SRTMGL3 DEM, since neither her `INPUTS_SP/` folder nor her `.ini`
includes a DEM path at all — flag this assumption to Becky in the next communication rather than
treating it as silently resolved. And: don't spend more time on the mangrove/marshland CN
simplification (collapsed to the same ESA analog as Closed Forest) — SWY's typical
downstream-beneficiary use case cares about CN accuracy only where something is actually
downstream, and mangrove sits at the tidal/estuarine end of the watershed by definition, so this is
genuinely low-stakes here (noted as not necessarily true for flooded grasslands/savannas, which
don't share mangrove's structural coastal position).

Two real, clean upstream `inspring` bugs now found across this project's SWY work (the `setup.py`
packages-list omission from 2026-09-11, plus the `interpolate_list` ordering bug today) — good,
concrete candidates for a small PR back to Rich's repo once the comparison work itself settles.

Full technical detail (crosswalk table, Kc comparison table, all four fixes) is in
`swy_methods.qmd`'s new "Test design: Philippines — the Becky-inputs comparison" section — this
entry is the chronological account, that document is the reference.

**Second update, 2026-09-15 morning, after the user flagged that the first draft to Becky didn't
make clear whose numbers were whose**: pulled Becky's own baseline output (already shared,
`data/swy/philippines/rich_shared/...`, not previously compared pixel-for-pixel) and built a real
side-by-side — whole-AOI means, an interactive map with matched color scales, and a pixel-wise
scatter plot (her rasters area-averaged down to our coarser grid, colored by her own LULC class).
Real finding: her/our QF ratio is 0.35 in aggregate but swings from 0.195 to 1.31 depending on
land-cover class — too wide a spread for one uniform cause (like the flat, spatially-invariant
rain-events placeholder alone) to explain; the CN/Kc crosswalk's genuine class-dependence is a
real, additional contributor. B (baseflow) flips direction entirely — ours runs *higher* than
hers almost everywhere, opposite of QF — not understood yet, flagged as real follow-up work, not
resolved. Full tables and the scatter plots: `swy_methods.qmd`'s new comparison subsection and
`swy_status_report.qmd`.

**Update, same evening, after the run actually finished**: it completed successfully — full
spatial coverage, exit code 0 — but only after 37+ minutes with zero console output, all spent in
the GDAL LULC-reprojection step (her raster's real, mostly-nodata extent is large even after
mode-resampling down to the DEM's pixel size; genuinely slow, not stuck — worth remembering before
assuming a silent long-running step has hung). The alarming-looking wall of threading tracebacks
at the very start of the log (`Joining executor thread ... would have caused a deadlock, skipping`,
`currentThread() is deprecated`) is noise from `taskgraph`'s own thread-pool teardown colliding
with Python 3.13's threading deprecations — unrelated to this run's actual correctness, and worth
not mistaking for a real failure the next time it shows up (it will, on every run using this
`taskgraph` version under Python 3.13).

Results: QF mean 257.3mm/yr, AET mean 834.6mm/yr (a first pass at this number came out as a
misleadingly low 224mm/yr mean with median exactly 0 — turned out to be legitimate zero-value
ocean pixels outside the actual land mask diluting a naive whole-raster average; re-masked to
`QF`'s own valid-pixel extent, which is the correct mask, to get the real number), aggregate `qb`
1321.0mm/yr — physically plausible, same order of magnitude as Borneo's `qb` (1705mm/yr). The
Borneo `L_sum` flow-accumulation anomaly recurs here, worse: 11.5% of pixels exceed 100,000 against
a median of 4,465 (Borneo: ~5%) — consistent with (not proof of) the flat-tidal-coastal-terrain
hypothesis, since an archipelago has far more coastline per unit area than Borneo's single
landmass. New, unexplained: aggregated `vri_sum` reads exactly 0.0 — not investigated tonight,
flagged for next session. Full numbers and the water-balance sanity check are in
`swy_methods.qmd`'s new "Result" subsection.

## 2026-09-15/16 — DEM-resolution confound caught and fixed, alignment scare investigated and closed, three real inspring rain-events bugs found and fixed, message sent to Becky

**Caught before sending anything**: Becky's `.ini` sets `TARGET_PIXEL_SIZE = 30` (confirmed exactly
1 arc-second via `GLOBAL_PIXEL_SIZE_DEG = 0.0002777777777777`), but the first Becky-inputs run
reused this project's already-fetched SRTMGL3 (90m) DEM out of convenience — introducing a second,
unintended confound (resolution) on top of the intended one (Kc/CN). Re-fetched SRTMGL1 (30m) via
AppEEARS (same manual-web-UI submission workaround as before; had to split into two north/south
tiles after the full-AOI bbox exceeded AppEEARS' per-request size cap at 30m resolution — merged
with `rasterio.merge` locally). Confirmed directly against `inspring`'s actual source that
`TARGET_PIXEL_SIZE` isn't a real parameter of this build — resolution is always derived from
`dem_raster_path`'s own native pixel size — so swapping the DEM file was the only lever, and
sufficient. Full 30m re-run kicked off in Docker; still running as of this entry (started
2026-09-15 evening, still going ~24h later — a much bigger raster than any prior run).

**Alignment scare, investigated properly, closed out clean.** While waiting, a water body looked
visibly offset between the WWF-SIPA and NCP layers on the interactive map. Coastline transects and
a Laguna de Bay centroid comparison first suggested a genuine ~430m registration bug; a second
lake (Taal) gave a much smaller offset in a different direction, contradicting a simple systematic
shift; a whole-domain cross-correlation of both land/water masks settled it — best alignment at
zero shift, 99%+ agreement. The apparent misalignment was resolution-driven boundary quantization
on one large, complex lake shape, not a real bug. No co-registration needed.

**Three real, distinct upstream `inspring` bugs found and fixed in the `user_defined_rain_events_dir`
code path** (previously two were found 2026-09-14, one patched, one deliberately left unpatched
over a data-loss concern — re-examined and fixed too, see below):
1. `interpolate_list` sized before the rain-events extension (already patched 2026-09-14).
2. `n_events_path_list` built from bare `os.listdir()` filenames, reused as both alignment input
   *and* output target — risked overwriting source files by name collision. Re-examined: the risk
   was overstated (Becky's originals are safe on Drive regardless; worst case is re-downloading our
   own local copy), and the actual fix is small — `_TMP_BASE_FILES` already reserves a separate,
   safe output-path list for exactly this, mirroring the pattern already used correctly for
   precip/ET0 two lines above. Fixed: read from real full source paths, write to the pre-reserved
   safe paths.
3. **New, found immediately after fixing #2**: `reclassify_n_events_task_list` (each month's
   quickflow task depends on the matching entry) is only ever populated in the branch where
   `user_defined_rain_events_dir` is NOT set — when it is set, the list stays empty and the next
   step crashes with `IndexError`. `user_defined_rain_events_dir` was apparently never fully wired
   up for this non-`prealigned` code path, only stubbed at the alignment stage. Fixed by mirroring
   the `prealigned` branch's own precedent elsewhere in the same file
   (`reclassify_n_events_task_list = [align_task]*12` — the files are already in final aligned form,
   no further reclassification needed). All three patches are in
   `Python_scripts/swy_borneo_run/Dockerfile`, image tag `swy_borneo_run:rainfix2`.

Launched an isolated test (`09c_run_swy_ph_becky_inputs_90m_rainfix.py`) — old 90m DEM (fast,
resolution not the variable under test here), real WWF-SIPA rain events enabled — running in
parallel with the 30m resolution-fix run (plenty of spare CPU/memory headroom: 22 logical
processors, the first run only used ~30%). First attempt crashed on bug #3 above; second attempt
(post-fix) running as of this entry.

**Also found via WWF's own public SIPA page** (worldwildlife.org, SIPA program page): confirms
InVEST models were used for four services including water recharge (almost certainly SWY), and
that **University of the Philippines Los Baños Foundation Inc. (UPLBFI)** did the Philippines
technical analysis — a concrete, plausible lead (not confirmed) for why Becky's `.ini` uses
parameter names (`TARGET_PIXEL_SIZE`, `SOIL_HYDROLOGIC_MAP`, `MONTHLY_ALPHA` as a flat number) that
don't match this project's actual `inspring` build: likely a UPLB-built wrapper around InVEST, not
a different `inspring` fork. Put to Becky as a working-assumption question, not asserted as fact.

**Mangrove/flooded-savanna reasoning made more precise**: mangrove is a terminal (tidal-outlet)
position in the routing network, so a wrong CN there cannot propagate downstream — settled, low
stakes. Flooded savanna is mid-basin, not terminal — even where a dam is unlikely on flat
floodplain terrain, a wrong CN there could still bias routed values further downstream. Made
explicit in `swy_methods.qmd`, `swy_status_report.qmd`'s biome table, and the message to Becky.

**Status-update message sent to Becky 2026-09-16** (not final results — the 30m run and the
rain-events test are both still in progress). Archived at
`docs/archive/message_becky_ph_comparison_status_2026-09-16_sent.md`.

**Also this session**: committed the whole accumulated backlog (4 commits — Colombia/Phase4 report
reorg, the SWY Philippines pipeline + DEM fix + mermaid fixes, two small follow-up commits for a
staging correction and leftover archive-annotation edits). Merged the two map-HTML builders into
one (`11_build_output_map_html.py`), fixed real Leaflet rendering issues (crisp pixel rendering,
click-to-compare popup, opacity slider). Added a folder-convention note to the top of this file and
to HANDOFF.md, since `docs/swy/` vs. `docs/reports/swy/` confused a fresh read.

**Not yet done, next session**: (1) check both Docker runs for completion — if the 30m run's
numbers or the rain-events test change the picture, regenerate the maps/scatter plots/report
before sending final results; (2) the paper — a new, small task: Quarto review-comment blocks are
rendering visibly into the shared .docx export, confusing for external readers; the fix is to keep
them out of the docx and split them into a separate qmd (comments + the section they belong to)
instead of deleting them. Full detail in `docs/HANDOFF.md`'s LATEST entry.

## 2026-09-17 — the checkerboard chase concludes (two real, separate causes), a real WWF-SIPA masking bug found, alignment re-verified rigorously, a real undocumented inspring feature found, two more runs launched

Long session, full compact narrative in `docs/HANDOFF.md`'s new LATEST entry — this is the
technical log version, same content, denser.

**The checkerboard the user spotted in the rainfix2 run's own native-resolution QF/B pixels
(2026-09-16 entry above ended with these numbers, not yet visually checked) turned out to have
two separate, unrelated causes, both now fixed:**

1. `interpolate_list = ['near'] * len(input_align_list)` in `seasonal_water_yield.execute()`
   applies nearest-neighbor to every aligned input, including continuous ones (precip, ET0,
   rain-events) coarser than the target DEM grid (~1km native vs. 90m here). Fixed: those three
   get `'bilinear'` instead (`swy_borneo_run:rainfix4`). Verified by direct inspection of the
   aligned intermediate rasters (`prcp_a*`, `et0_a*`, `n_events*` in `cache_dir/`) — smooth
   gradients after the fix, confirmed at native resolution, not inferred from downstream output.
2. Did **not** fully fix it — a second, independent cause: `HYSOGs250m_Soil_Groups_reclassified.tif`
   (this project's soil-group input, from NatCap's Data Hub) has a real, regular block artifact
   *in its own native-resolution pixels*, confirmed by rendering a native crop with zero
   resampling applied. Compared directly against the raw ORNL DAAC original
   (`daac.ornl.gov/daacdata/global_soil/Global_Hydrologic_Soil_Group/data/HYSOGs250m.tif` —
   downloaded successfully; public data, Earthdata-login-gated; needed a cookie jar
   (`curl -n -c/-b cookiejar.txt -L`) to get through the OAuth redirect loop, plain `-n -L` alone
   loops 50x and fails) — the raw file shows genuine fine-scale texture at the identical crop
   location, no artifact. Whatever NatCap's "reclassified" repackaging did introduced this.
   **Becky's own `.ini` references `HYSOGs250m_md5_517bfa.tif`** — an MD5-named file (ORNL DAAC's
   own download-naming convention), not this project's NatCap-sourced copy — and her
   `SOIL_HYDROLOGIC_MAP` crosswalk (`1:'A',2:'B',3:'C',4:'D',11:'A',12:'B',13:'C',14:'D'`) matches
   the raw file's own documented pixel-value encoding exactly (confirmed from the dataset's own
   PDF documentation, fetched via `daac.ornl.gov/SOILS/guides/Global_Hydrologic_Soil_Group.html`
   → redirects to a signed, no-login-needed S3/CloudFront URL for the guide itself, though not for
   the actual `.tif`). Switched this project's own pipeline to the raw file
   (`data/swy/shared/soil_hydrologic_group_raw/HYSOGs250m.tif`, clipped to the Philippines AOI +
   0.05° buffer, dual-class codes 11-14 collapsed to 1-4 matching her exact crosswalk logic →
   `data/swy/philippines/inputs/soil_hydrologic_group_hysogs250m_raw_ph.tif`).
3. Combining both fixes (image `swy_borneo_run:rainfix4`, `09c_run_swy_ph_becky_inputs_90m_rainfix.py`
   updated to point at the raw soil file) eliminated the checkerboard entirely — confirmed by the
   same native-resolution visual check, same crop location, used throughout this investigation.

**A real bug found in WWF-SIPA's own shared baseline output, independent of anything above**: her
B raster's nodata flag (-9999) doesn't cover everything her own QF raster's nodata flag does (same
grid, transform/shape/crs verified identical) — 54,728,258 pixels she calls "valid" fall entirely
outside QF's own clean land mask (39,990,782 of those reading exactly 0.0 — ocean, not real
zero-baseflow land). Corrected by masking her B raster to QF's own valid extent before any stat is
computed (`data/swy/philippines/comparison_maps/B_wwf_PH_baseline_historical_climate_masked.tif`).
Moves her full-resolution B mean from 653.1 to 698.8mm/yr. Both `10c_render_comparison_maps.py`
and `12_scatter_comparison.py` updated to use the masked file.

**The user pushed back hard on two things, correctly, wanting actual verification not
reassurance — both checked properly, both came back clean, worth recording the method since it'll
be needed again:**
- *Registration*: a whole-domain land/water-mask cross-correlation (her QF mask reprojected onto
  the NCP grid via `Resampling.average`, then diffed against the NCP mask at shift offsets -3..+3
  in both axes) found best agreement at exactly zero shift, 99.44% — re-run from scratch on the
  final corrected data, not assumed from the 2026-09-15/16 investigation's own conclusion (which
  was on now-superseded data). A tight native-resolution crop (Manila Bay, `Resampling.nearest`
  for a clean categorical diff) rendered as a 3-color mask (white=agree, red=NCP-only,
  blue=baseline-only) showed every disagreement confined to a literal one-pixel-wide fringe along
  coastlines and river channels — real boundary quantization between 30m and 90m grids, nothing
  else. Exact counts: 923,270 NCP-only pixels (2.82% of NCP's valid area), 469,741 baseline-only
  (1.45% of baseline's).
- *Why the interactive map looked worse than this*: the map does not put both layers on a shared
  grid — `10c_render_comparison_maps.py`'s `_read_downsampled` reads each raster independently at
  its own native resolution/bounds, only downsampled for display size. Toggling between two
  independently-rendered images of the same coastline exaggerates a real-but-tiny edge difference
  far more than either a static view or the actual (shared-grid, averaged) numeric comparison
  would. Not yet fixed with a caption/note in the map itself.

**A real, previously-missed `inspring` capability, found by tracing `_reclassify_or_clip()`'s
actual runtime logic instead of `execute()`'s docstring**: for `cn_a`/`cn_b`/`cn_c`/`cn_d`/
`root_depth`/`kc_1`-`kc_12`, it checks `args[f'{key_field}_path']` *before* falling back to the
CSV table, and if set, warps that raster directly (`geoprocessing.warp_raster(..., 'bilinear',
...)`) and uses it as-is — no reclassification, no table involved. **This reverses the
2026-09-14 documentation-correction finding** ("confirmed by reading the code directly" that no
raster-override path exists) — that check evidently only looked at the documented `Args:` list,
not this function's body. Re-asserted incorrectly again in this session's own first pass at
today's status report, now fixed there too. Real implication: Becky's `.ini` (`CN_A_PATH` etc.)
may be using this exact mechanism directly, not "a different wrapper/fork" as previously assumed
— unconfirmed, but the parameter names map onto it precisely (case difference only).
`_TABLE_BASED_BIOPHYSICAL_FACTORS = ['root_depth','cn_a','cn_b','cn_c','cn_d','kc_1'..'kc_12']`
confirms the full list of factors this applies to.

GCN250 doesn't drop into this cleanly, though: delivered as three rasters by antecedent moisture
condition (ARC-I/II/III = dry/average/wet), not four by soil group — soil group is already baked
into GCN250's own values (crosswalked against HYSOGs250m during its construction, per the
Jaafar et al. 2019 methodology). Correct usage: point all four of `cn_a_path`-`cn_d_path` at the
same ARC-II ("average") raster. Downloaded via Figshare's public API
(`api.figshare.com/v2/articles/7756202` → file list with direct `ndownloader.figshare.com` URLs;
the article page itself 403s) — `GCN250_ARCII.tif`, 640MB, global, saved to
`data/swy/shared/gcn250/`. Not yet clipped or run.

**Three draft GitHub issues written** for `springinnovate/inspring`
(`docs/swy/inspring_github_issues_draft.md`), at the user's request — covers the Dockerfile/
packaging bugs, the three rain-events bugs + the resampling-method issue (with working patches),
and the undocumented raster-override feature (framed as a documentation-PR ask, since Rich
explicitly prefers formal issues/PRs). `gh` CLI isn't installed on this machine (checked both Git
Bash and PowerShell) — user will post manually or set up `gh` later. Not posted.

**A second 30m validation run launched** (`swy_ph_30m_final`, fresh workspace
`workspace_becky_inputs_30m`, image `swy_borneo_run:rainfix4`, real rain events + real soil group
this time, unlike the aborted 2026-09-16 attempt) at the user's explicit request — purpose:
confirm the 90m-aggregated comparison isn't itself an artifact of the resolution/aggregation
methodology, by checking whether a native 30m run gives materially different QF/B ratios.
**Still running as of this writing, 4+ hours in, stuck in the same DEM flow-routing stage
(pit-filling done ~1hr in, flow-direction not yet done)** that made the earlier 30m attempt take
24+ hours before being aborted — same flat/tidal-archipelago-terrain cause, unrelated to any of
today's fixes. Checked directly against the actual function signatures in this build
(`ecoshard.geoprocessing.routing.fill_pits`/`flow_dir_mfd`/`flow_accumulation_mfd`): **zero
threading/worker parameters exposed on any of them** — confirmed this bottleneck cannot be sped up
with more CPU cores, not assumed. **Do not restart this run for the GCN250 test** — DEM-routing is
independent of CN inputs, and TaskGraph's own task-level caching (`cache_dir/taskgraph_data.db`,
keyed on function+args+input-file state) should let a future GCN250-adjusted 30m variant reuse
this expensive work, *if* launched against the same workspace directory with only the CN args
changed, once this run actually reaches/finishes the routing stage.

**Current numbers, 90m, both fixes applied (before any GCN250 test)**: QF ratio (NCP/WWF-SIPA)
0.766 aggregate, r=0.567, per-class range 0.550 (Annual Crop) to 1.368 (Grassland). B ratio 1.510
aggregate, r=0.376, per-class range 0.708 (Fishpond) to 5.998 (Annual Crop). One coherent partial
explanation for B, not proof: this project's lower EVI-based forest Kc (0.52-0.70 vs. her flat
1.0) feeds AET not QF, so less water lost to evapotranspiration in forest classes specifically
should mean more left for recharge/baseflow — matches the elevated forest-class B ratios (Closed
Forest 1.69, Open Forest 1.78, Mangrove 2.94) directionally, doesn't explain B's overall erratic
pattern or its weak correlation.

**A sixth real bug found, immediately upon actually using the raster-override path**: the first
GCN250-direct run (`workspace_becky_inputs_90m_gcn250`, now archived with a `_nodata_bug_ARCHIVE`
suffix) produced a CN output with 455,198 pixels (1.17% of valid model pixels) reading a literal
CN of 255 — GCN250's own nodata sentinel, not a real curve number (CN can't exceed 100).
Root cause: `_reclassify_or_clip()`'s raster-override branch calls
`geoprocessing.warp_raster(args[key_path], ..., 'bilinear', ...)`, and `warp_raster`'s own
signature has no `nodata`/`src_nodata`/`dst_nodata` parameter at all — confirmed by inspecting it
directly. The call also passes no `gdal_warp_options`/`gdal_warp_kwargs` that could supply one
another way. Result: the output raster's declared nodata (-1.0) has nothing to do with the source
file's real nodata (255), so any 255-valued source pixel warped into the output is silently treated
as valid data downstream. Mapped where these pixels actually are (block-max-pooled downsample,
red/gray render): a thin, near-continuous fringe around every single island's coastline — GCN250's
own coverage has small gaps right at the coast that don't quite match this project's DEM/LULC-
derived land mask, not scattered random noise or deep-interior corruption. Fixed, not just masked
out after the fact (masking wouldn't undo contaminated routing/flow-accumulation propagating the
bad CN downstream to otherwise-clean pixels): filled GCN250's own nodata gaps with
`scipy.ndimage.distance_transform_edt`'s nearest-valid-neighbor fill *before* clipping to the AOI
(`data/swy/philippines/inputs/gcn250_arcii_ph_filled.tif`) — zero nodata pixels remain in the
clip, so there's nothing left for the broken warp to mishandle. Second attempt
(`swy_ph_90m_gcn250v2`) launched with the corrected input. Worth adding as a fourth item to the
GitHub issue about the undocumented raster-override path, not a separate issue — same code path,
same root cause category (undocumented and under-tested).

**Status report** (`docs/reports/swy/swy_status_report.qmd`) fully rewritten this session as a
clean current-state memo, no run-by-run history in the reader-facing text (direct user feedback:
"the report is again too convoluted... we tell too much stuff that looks more than obvious/
self-evident, considering the level of expertise of the people who is going to read" — Becky and
Rich are domain experts, the report should read as one practitioner briefing another, not explain
CN/Kc basics; see `feedback-report-vs-log-separation` memory, updated with this exact instance).
User then edited the file directly and left inline bracketed questions — all addressed: GCN250
citation added, tropical-forest-CN-patch status clarified as not-integrated (and found her own
Closed Forest CN isn't tropically-corrected either, 36/60/73/79, so this isn't contributing to the
gap), the CN raster-vs-table section corrected per the finding above, the Kamble/Rich/EVI
paragraph rewritten to attribute the objection to Rich correctly and state it was checked not
dismissed, the Kc-Corbari finding connected explicitly to the B-by-class numbers (where it
actually shows up, not in QF), a confusing "unrelated to the above" transition reworded. Renders
clean, zero remaining bracketed comments as of this writing. Still needs: the GCN250 result once
it exists, and a map-caption note about the toggle-misalignment perception.

## 2026-09-18 — GCN250 folded into the report, then three more real, deeper problems found by
actually checking the user's own doubts instead of reassuring: a map-display aliasing bug, a
25%-of-domain coastal NaN gap in NCP's own B, and a non-nested comparison grid

**Report maintenance first**: folded the (already-computed, from 2026-09-17) GCN250-direct result
into `swy_status_report.qmd`'s Curve Number section as a real comparison table (it had been sitting
finished but undocumented — the report still said "in progress"); fixed a genuine self-contradiction
in the report's own "Where we stand" summary, which still claimed *"`inspring` has no
raster-override capability"* several paragraphs above the section that correctly documents the real
one — a stale claim that had already been "fixed" once per this file's own 2026-09-17 entry, but
evidently not in this specific spot. Re-rendered.

**Checkerboard fix, re-verified independently rather than taken on faith from the entry above**:
rendered matched native-resolution before/after crops (same pixel window, pre-fix
`_nearest_ARCHIVE` vs. the corrected `_rainfix` run) at three levels — aligned continuous
intermediates (`n_events0`, `et0_a0`), and the QF/B outputs themselves. All four confirm the same
thing: hard rectangular block edges before, smooth organic texture after. This part really is fixed.

**A second, different checkerboard-looking bug, found because the user kept looking at the actual
map instead of accepting "fixed."** The interactive map still showed a speckled pattern on the NCP
B layer after the above was confirmed fixed. Root cause, found by checking `10b`/`10c`'s own
downsampling code: both used `rasterio` `Resampling.nearest` to shrink the full-resolution raster
for web display (~7-12x decimation at `MAX_DIM=1600`) — at that ratio, nearest-neighbor keeps one
raw pixel per display cell and discards the rest, aliasing real per-pixel texture into a
salt-and-pepper pattern that isn't in the underlying data at all. Confirmed by rendering the same
full-country B layer both ways: `nearest` reproduces the exact speckle; `average` shows smooth,
coherent terrain gradients instead. Fixed in both scripts (switched to `Resampling.average`), maps
regenerated, a caption added directly on the map explaining the distinction (display images are
downsampled/averaged; every number in the report comes from full-resolution rasters, never the
display image). Unrelated to the first bug and to anything from 2026-09-17 — purely a map-rendering
issue, never touched the actual comparison numbers.

**A third, real, still-open problem — found because the user checked the native GeoTIFF in QGIS
directly and still saw a fine speckle after both fixes above.** Traced it stage by stage on the
corrected run: QF is clean (no speckle). L (local recharge, pre-routing) already shows the same fine
speckle AET shows — so it's not a flow-routing/DEM artifact (ruled out by checking pre-accumulation
L, not just post-accumulation B/L_sum). Confirmed Kc is genuinely class-uniform (read
`07b_build_biophysical_table_becky_inputs.py`'s `compute_class_monthly_evi()` directly — it's a
zonal *mean* EVI per class per month, not per-pixel), so per-pixel Kc noise isn't the explanation
either. Confirmed the speckle is bit-for-bit present in the pre-checkerboard-fix archive too (not
introduced by anything this week). WWF-SIPA's own B at the identical location is comparatively much
smoother. **Net: real, unexplained, isolated to the AET/water-balance step specifically (present in
AET/L/B, absent from QF), not yet root-caused** — paused, not abandoned, to prioritize the two
findings below once the user pointed out they're more foundational. Worth revisiting: `inspring`'s
own Budyko-curve AET solver internals, which haven't been read yet.

**A fourth, bigger problem, found because the user pushed back hard on "the numbers are fine
because the resampling code looks right"**: checked NCP's own QF against its own B (same run) and
found **8.16 million pixels (~25% of the valid domain) where QF has a real value but B is literally
NaN** — not flagged nodata, actual NaN. Mapped it: forms a clean ring around every single
coastline in the country, not scattered noise. Confirmed AET and L are both valid at these exact
locations (so it's introduced at the routing/accumulation step specifically, like a classic
edge-of-domain flow-routing limitation — cells at a coastline may have no in-domain "downstream"
cell to route to). `12_scatter_comparison.py`'s own `np.isfinite()` filter already silently drops
these pixels from every reported B statistic, with zero documentation that this exclusion is
happening — and it's not a neutral 25%: the classes already flagged as having the most extreme,
least-understood B ratios (Mangrove, Fishpond, built-up) are inherently coastal, so this gap could
be distorting exactly the numbers already flagged as strangest.

**A fifth problem, the user's own catch, and probably the most consequential of the session**:
asked directly why the raw WWF-SIPA and NCP rasters don't report identical extents. First check
was wrong in a useful way — comparing `rasterio`'s `src.bounds` on both files suggested a ~3° gap
to the west, which would have been alarming (missing real land). Turned out to be an artifact of
*my own incomplete check*: her raster's rectangular file canvas pads a huge empty margin around the
real data. Recomputed her *actual valid-pixel* bounding box directly (not the file rectangle) and
it matches this project's own `ph_aoi.gpkg` almost exactly (~0.01°, the AOI script's own
documented simplify tolerance) — confirming `01_build_aoi_from_rich_mask.py`'s AOI-from-valid-
footprint approach genuinely works as designed, not a bug. But the user kept pushing (correctly):
checked the two grids' pixel *origins* precisely and found a real, non-integer offset of ~0.91 of a
90m pixel (~22m) between them — same CRS (EPSG:4326, confirmed identical), clean 3:1 pixel-size
ratio, but not nested. `12_scatter_comparison.py`'s `reproject()`-based averaging already handles
this correctly (verified by reproducing its exact numbers independently, and by a shift-search from
-2..+2 pixels confirming zero-shift really is the best alignment already) — but "correctly averaged
despite an offset" isn't the same as "actually nested," which is what the user wants.

**Traced exactly how to fix the nesting without touching `inspring` itself**: read
`seasonal_water_yield.execute()`'s alignment call directly —
`geoprocessing.align_and_resize_raster_stack(..., pixel_size=<from dem_raster_path>,
raster_align_index=<DEM's own index in the input list>)`. Both the pixel size AND the origin-
snapping for the model's entire output grid come from whichever raster is passed as the DEM. So a
DEM pre-warped onto an exactly-nested grid is sufficient on its own.

**Plan approved with the user** (full detail: `C:\Users\JerónimoRodríguezEsc\.claude\plans\curious-gliding-whisper.md`,
also mirrors `docs/HANDOFF.md`'s latest entry): (1) `02e_snap_dem_to_baseline_grid.py` — new script,
snaps the AOI bounds outward to the nearest exact multiple of 3 WWF-SIPA pixels from her own
raster's origin, warps the existing SRTMGL3 DEM onto that exact grid, hard-asserts zero fractional
remainder before anything else proceeds; (2) `09e_run_swy_ph_snapped_grid.py` — copy of `09c` with
only `dem_raster_path` changed, own workspace, re-run in the background (multi-hour); (3) per the
user's own explicit framing ("create a mask with the maximum extent in which both datasets have
valid pixels, that is the only valid comparison") — new `13_build_valid_comparison_mask.py`,
per-variable (QF, B separately, since their footprints differ), requiring a full 3x3 block of
baseline-valid pixels (not partial credit) AND NCP-valid, consumed by both `10c` and `12` in place
of each script's own ad hoc `isfinite` logic; (4) once the new run lands and passes the nesting
check, regenerate the scatter/map/report and compare the new numbers against today's as the real
payoff check; (5) `data/swy/philippines/` decluttered alongside this — a fresh inventory found
81GB total, several confirmed-superseded workspace copies from this week's iterative debugging
(`_nodata_bug_ARCHIVE`, `_nearest_ARCHIVE`, two undocumented `_precip_et0_near_ARCHIVE`/
`_srtmgl3_backup` directories never mentioned in any doc) — the documented-safe ones get deleted,
the undocumented ones get moved to a new `archive/` subfolder rather than deleted outright.

Not done yet as of this entry — this documents the plan, not its execution. `swy_ph_30m_final`
(the 30m validation run) is untouched throughout all of this and still running.

## 2026-09-21 — factorial CN/Kc decomposition completed, report reworked around a two-part question,
per-class regression breakdown

The plan above executed in full: `02e`/`09e` (grid-snapped DEM + re-run), `13_build_valid_
comparison_mask.py` (explicit valid-in-both masks, had to be rewritten from a full-array
`reproject()` to a chunked row-strip block-average after the first attempt got OOM-killed —
WWF-SIPA's native 30m raster is ~2.8 billion pixels, too large to hold in memory alongside whatever
else was running), then the full 2x2 CN/Kc factorial (`09f`/`09g`/`09h`, `07c_build_hybrid_
biophysical_tables.py` for the two hybrid CSVs). Real process mistake caught along the way:
double-backgrounding (`(...) &` plus `run_in_background: true` on the same shell call) made the
tool report the first two factorial runs "complete" almost instantly, before they'd actually
started — fixed with `docker wait <container>` instead, which blocks at the Docker-daemon level
regardless of shell lifecycle. Full factorial result:

| Combination | QF ratio | QF r | B ratio | B r |
|---|---:|---:|---:|---:|
| Our CN + our Kc | 0.77 | 0.56 | 1.48 | 0.39 |
| Her CN + our Kc | 0.99 | 0.92 | 1.27 | 0.70 |
| Our CN + her Kc | 0.77 | 0.56 | 1.21 | 0.82 |
| Her CN + her Kc together | 0.99 | 0.92 | 1.04 | 0.92 |

QF's gap is 100% CN (Kc structurally can't touch quickflow in this model). B's gap needs both —
either alone leaves a real, land-cover-class-dependent residual (visible in the scatter plots as
parallel-but-offset lines per class), which fully collapses once both are corrected together.

**Report reworked substantially, in stages, at the user's direction, each one a real correction**:
(1) first pass added the factorial table and scatter plots; (2) user pointed out the framing was
close to circular ("if the only things we change are these, the only reason it changes is because
we changed these only things") — the real gap was that this is a *model-to-model* comparison, never
checked against anything observed, so "the gap is explained" proves the pipeline is wired correctly
and tells us where to focus, but says nothing about which parametrization (or either) is closer to
real Philippine hydrology; added an explicit "what this does and doesn't establish" paragraph and
split "Where this puts us" into two tracks — fixes justified by other independent literature
regardless of validation outcome (tropical CN correction, crop calendar), versus real validation
(needs a third, independent, observational data source not yet identified) as a separate, harder,
unscoped undertaking; (3) user flagged "wrong direction" (B's 148%, opposite sign from QF's
undershoot) as smuggling in an unearned value judgment — no rule says baseflow should come out
lower, "wrong" just meant "opposite sign, needs explaining." Fixed by stating the mechanism instead:
this project's lower CN and lower Kc both push water toward recharge/baseflow rather than away from
it, so a lower QF and a higher B are the expected pair, not a contradiction; (4) user found the
"Validation checks performed" table too detailed/sophisticated for the actual audience ("the kind of
stuff she will not read") — collapsed to one short paragraph; (5) resolved a stray unresolved
bracket-comment the user had left inline (`[or we need to know if this has been validated]`) into
real prose, and in the process confirmed directly with the user that we'd both been editing
`swy_status_report.qmd` concurrently, which had silently clobbered one edit — resolved by re-reading
before re-applying, worth remembering as a real failure mode when the user has the file open too.

**Scatter plot legend fix, promoted to the real pipeline, not left as a one-off**: the user noticed
the legend swatches were nearly as hard to read as the plot itself, since matplotlib carries the
scatter points' own `alpha=0.25` (needed so overlapping clusters stay legible) into the legend key
by default. Fixed in `12_scatter_comparison.py` by grabbing the legend's handles after creation and
forcing `set_alpha(1)` on them specifically — plot points stay translucent, legend swatches read at
full color. Also used this moment to promote the factorial scatter-plotting code (previously three
different one-off scratchpad scripts across two sessions) into a real numbered script,
`14_factorial_scatter_comparison.py`, with the same legend fix applied.

**Per-class regression breakdown, done at the user's request after they spotted a real pattern by
eye in the QF scatter plot** (cropland systematically further from the 1:1 line than natural land
cover) — quantified directly rather than left as a visual impression:

QF: cropland (Annual Crop ratio 0.55, Perennial Crop 0.63) sits substantially below every natural/
vegetated class (0.80–1.37, group mean 0.95) — confirms the visual read exactly. B: fit quality
(r²) splits classes into two real, different categories — Closed Forest/Open Forest/Grassland/
Brush-Shrubs/Marshland/Perennial Crop all have r² 0.81–0.98 (a clean, consistent, *correctable*
offset), while Annual Crop/Built-up/Fishpond/Inland Water are r² 0.01–0.39 (genuinely unstructured,
not just offset — a harder problem than recalibration). Mangrove's own B fit is weak (r²=0.32) on
top of already being deprioritized on structural grounds (terminal, tidal-outlet routing position)
— the weak fit is now a second, independent reason it's not worth chasing.

**Directly confirmed, not assumed**: "Marshland/Swamp" (WWF-SIPA's 12-class scheme) is the
practical stand-in for the "flooded grassland/savanna" CN gap already named in the report's own
methodology section — checked `swy_methods.qmd`'s crosswalk table directly, and both this project's
CN table *and* WWF-SIPA's own collapse Marshland/Swamp to the same CN as Closed Forest, deliberately
mirroring what her table already does. Neither approach has actually given it flood-aware treatment.
That said, it isn't currently showing up as a problem class in the data (QF ratio 0.81, B ratio
0.995, both close to parity) — worth knowing before assuming it needs urgent attention alongside
the tropical CN correction and crop calendar work.

User's own framing of the strategic implication, worth preserving verbatim in spirit: the per-class
behavior "might provide a better perspective" on what to refine and which paths to try next — a
softer, more exploratory claim than "here is the fix," and the report was deliberately kept to a
one-line pointer at this raw detail rather than the full breakdown, per direct instruction that the
report is for someone (WWF-SIPA) without time to read a long, sophisticated document.
