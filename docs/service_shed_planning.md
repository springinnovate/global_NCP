# Service Shed Planning: Off-pixel Impact

## Overview
Currently, the hotspot extraction pipeline assigns a hotspot classification strictly based on the 10km grid cell where the ecosystem service decline occurs. However, ecosystem services like water retention or pollination provide benefits that extend beyond the boundaries of a single pixel ("Off-pixel Impact"). This plan outlines how to "bloom" these hotspots into their respective service sheds.

## Interception Point in the Pipeline
The most logical place to intercept the spatial logic is **after hotspot extraction but before spatial synthesis**.

### Proposed Workflow
1. **Upstream Extraction:** `analysis/hotspot_extraction.qmd` runs as usual, generating `plt_long.rds` and spatial grid files (e.g., `10k_change_calc.gpkg`) containing the base hotspot assignments per `fid`.
2. **Interception (New Module):** A new R or Python script, for example `analysis/service_shed_blooming.qmd` (or `.R`), will load the explicit hotspot flags from `plt_long.rds` and their geometry from the master grid.
3. **Blooming Operations:**
   - **Water Services (e.g., N Retention, Sediment Export, Coastal Risk):** Perform a spatial join or watershed-routing operation using **HydroBASINS** (level 8-12) or coastal watersheds. The decline pixel "blooms" to affect the respective downstream/down-basin beneficiary populations.
   - **Pollination & Nature Access:** Apply a distance-decay buffering logic or a simple focal buffer (e.g., a 2-5km radius) to expand the hotspot footprint outward, representing the service's foraging or access range.
4. **Downstream Synthesis:** `analysis/hotspot_synthesis.qmd` will be configured to load these "bloomed" service sheds (e.g., `plt_long_bloomed.rds`) instead of the base point-level `plt_long.rds` to calculate true off-pixel beneficiary exposure.

## Proposed File Structure

```text
c:\projects\global_NCP\
├───analysis\
│   ├───hotspot_extraction.qmd      <- (Existing) Base pixel extraction
│   ├───service_shed_blooming.qmd   <- (NEW) Intercepts outputs here
│   └───hotspot_synthesis.qmd       <- (Existing) Modified to accept bloomed inputs
├───data\
│   ├───vector_basedata\
│   │   ├───hydrobasins\            <- (NEW) Storage for HydroBASINS catchment polygons
│   └───processed\
│       ├───off_pixel_impact\       <- (NEW) Directory for intermediate bloomed vectors
│       │   ├───sheds_pollination.gpkg
│       │   ├───sheds_water.gpkg
│       │   └───plt_long_bloomed.rds <- (NEW) The final synthesized dataset
```
