# SWY Borneo test: workflow and status

Working diagram for the whole Borneo SWY pipeline, kept current as things move. Status colors:
🟩 done · 🟨 in progress / partial · 🟥 not started or blocked. Update this alongside `HANDOFF.md`
and `research_notes.md` rather than letting it drift — this is the map, those are the log.

```mermaid
flowchart TD
    subgraph INPUTS["Raw inputs — all downloaded and verified as of 2026-09-10"]
        AOI["AOI vector<br/>borneo_aoi.gpkg<br/>HydroBASINS AU/Oceania"]:::done
        DEM["DEM<br/>SRTMGL3, 90m<br/>verified: max 4041m vs. real 4095m"]:::done
        ROUTE["Routing parameters<br/>Rich's swy_global.ini defaults"]:::done
        LULC["LULC raster (2020)<br/>real C3S netCDF found on OneDrive —<br/>local .tif copy was empty/corrupted"]:::done
        SOIL["Soil hydrologic group<br/>HYSOGs250m"]:::done
        NDVI["NDVI + QA layer<br/>MOD13A3.061, full record 2000-2026<br/>verified: plausible values, correct extent"]:::done
        PRECIP["Precipitation, 2020<br/>CHIRPS, verified: July mean 138.9mm"]:::done
        ET0["Reference ET0, 2020<br/>TerraClimate pet — NEW source,<br/>not previously vetted before today<br/>verified: 97-118mm monthly means"]:::done
        GCN["CN base<br/>GCN250, global"]:::done
    end

    subgraph DERIVED["Derived layers — built for the first run attempt (simpler than the long-term plan)"]
        LULC2020["Real 2020 LULC extracted<br/>from OneDrive C3S netCDF —<br/>local .tif was an empty/corrupted stub"]:::done
        RAINEV["Rain events<br/>PLACEHOLDER (18/month uniform)<br/>not derived from real daily CHIRPS"]:::progress
        BIOTABLE["Biophysical table (CSV)<br/>lucode + CN_A-D (from GCN250 CSV)<br/>+ Kc_1-12 (NDVI regression per lucode,<br/>uniform across ALL vegetated classes —<br/>not split crop vs. non-crop this round)"]:::done
    end

    subgraph RUN["Model run — first attempt uses the CSV biophysical-table path, not raster-overrides (pragmatic choice for this round, see swy_methods.qmd)"]
        SWYRUN["inspring.seasonal_water_yield.execute()<br/>via Docker (project's own image + inspring built on top)<br/>COMPLETE, full coverage confirmed, 2026-09-11 (2nd attempt)<br/>QF/AET plausible; L_sum anomaly (~5% of pixels) confirmed<br/>real, not a crash artifact — clusters at one river mouth"]:::done
    end

    subgraph OUT["Output"]
        RESULT["Borneo quickflow / baseflow output<br/>full output set exists and is readable"]:::done
        QCHECK["Plausibility check<br/>QF/AET plausible; L_sum has a real,<br/>diagnosable flow-accumulation anomaly<br/>(NOT a published-benchmark validation —<br/>that's the deferred Hamel-basin test)"]:::done
        REPORT["Report to Becky + Rich, 2026-09-11"]:::progress
    end

    AOI --> SWYRUN
    DEM --> SWYRUN
    ROUTE --> SWYRUN
    LULC --> LULC2020
    LULC2020 --> BIOTABLE
    SOIL --> SWYRUN
    GCN --> BIOTABLE
    NDVI --> BIOTABLE
    BIOTABLE --> SWYRUN
    PRECIP --> RAINEV
    PRECIP --> SWYRUN
    ET0 --> SWYRUN
    RAINEV --> SWYRUN
    SWYRUN --> RESULT --> QCHECK --> REPORT

    classDef done fill:#c8e6c9,stroke:#2e7d32,color:#1b1b1b
    classDef progress fill:#fff9c4,stroke:#f9a825,color:#1b1b1b
    classDef blocked fill:#ffcdd2,stroke:#c62828,color:#1b1b1b
```

## Reading the current bottleneck (updated 2026-09-11)

**Every raw input is downloaded and content-verified**, and **the first actual model run has been
submitted** — see `Python_scripts/swy_borneo_run/` for the full, consolidated, re-runnable
pipeline (scripts 01-09) and its `README.md` for the exact known compromises in this run. Check
`docs/swy/research_notes.md`'s latest entry for the real outcome before assuming success or
failure either way.

**What changed from the raster-override plan described earlier in this file's history**: for
this first attempt, the simpler CSV `biophysical_table_path` was used instead of `inspring`'s
raster-override args (`cn_a/b/c/d_path`, `kc_1...12_path`) — already fully built, lower risk for
a first pass under real time pressure. The Kc regression was applied uniformly across all
vegetated classes (including cropland) rather than splitting cropland out to FAO-56, since
region-correct crop-calendar timing isn't built yet — a real, documented simplification, not an
oversight. The raster-override path remains the architecturally better long-term approach (see
`swy_methods.qmd`) and biome-specific CN patches (tropical forest buildable; mangroves and
flooded savannas still open gaps) are worth layering on in a follow-up round — neither was a
blocker Becky set for this test.

**Real environment finding along the way**: `inspring`'s own Dockerfile is broken (missing
`requirements.txt`, confirmed by actually running the build) and its `setup.py` has a real
packaging bug (`inspring.seasonal_water_yield` missing from the `packages` list). Both worked
around, not fixed upstream — real candidates for a small PR back to Rich later. This project's
own existing Docker image already had everything needed to build `inspring` once bypassing its
own broken Dockerfile.
