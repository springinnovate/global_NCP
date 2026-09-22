# SWY Philippines comparison: workflow and status

Working diagram for the Philippines Kc/CN comparison — both paths side by side, so the shared
inputs and the two divergence points (DEM, Kc/CN, rain events) are visible at a glance. Status
colors: 🟩 done · 🟨 in progress / partial or unconfirmed · 🟥 blocked. Update this alongside
`HANDOFF.md` and `research_notes.md` rather than letting it drift — this is the map, those are the
log. Terminology: **WWF-SIPA baseline** = the original run, WWF-SIPA's own Kc/CN. **NCP Kc/CN
run** = this project's isolated-variable run, same WWF-SIPA inputs with this project's own Kc/CN
substituted in.

```mermaid
flowchart TD
    subgraph SHARED["Reused directly from the WWF-SIPA INPUTS_SP folder, 2026-09-14"]
        LULC["WWF-SIPA LULC<br/>12-class typology, UTM 32651<br/>reprojected to WGS84 for the NCP run"]:::done
        PRECIP["Precipitation<br/>30-yr CMIP6 climatology (1985-2014, p50)"]:::done
        ET0["ET0<br/>Global-AI_PET_v3 / CGIAR-CSI"]:::done
        RAINEV["Real spatially-distributed rain events<br/>monthly rasters"]:::done
    end

    subgraph BASELINE["WWF-SIPA baseline path — already run, output shared earlier"]
        BASECNKC["WWF-SIPA's own Kc/CN<br/>biophysical_template_PH_revised.csv<br/>+ CN_A-D raster overrides"]:::done
        BASERUN["WWF-SIPA baseline run<br/>completed earlier, output shared 2026-09-14"]:::done
    end

    subgraph NCPPATH["NCP Kc/CN path — this project, 2026-09-14/15"]
        DEM["NCP's own SRTMGL3 DEM<br/>not in WWF-SIPA inputs — assumed,<br/>still needs confirmation"]:::progress
        CROSSWALK["NCP Kc/CN biophysical table<br/>hand-built 12-class to ESA CN crosswalk<br/>plus EVI-regression Kc<br/>07b_build_biophysical_table_becky_inputs.py"]:::done
        PLACEHOLDER["Flat 18-events/month placeholder<br/>real product blocked — 2 inspring bugs,<br/>one patched, one deliberately deferred"]:::blocked
        NCPRUN["inspring.execute()<br/>NCP Kc/CN run<br/>COMPLETE, 2026-09-14/15"]:::done
    end

    subgraph COMPARE["Direct comparison, 2026-09-15"]
        PIXEL["Pixel-wise comparison<br/>WWF-SIPA resampled to NCP's coarser grid"]:::done
        MAPS["Interactive map + scatter plots<br/>colored by WWF-SIPA LULC class"]:::done
        FINDING["Finding: QF ratio 0.20-1.31 by class,<br/>non-uniform — CN/Kc a real contributor<br/>B flips direction entirely — unexplained"]:::done
    end

    LULC --> BASECNKC
    LULC --> CROSSWALK
    PRECIP --> BASERUN
    PRECIP --> NCPRUN
    ET0 --> BASERUN
    ET0 --> NCPRUN
    RAINEV --> BASERUN
    RAINEV -.-> PLACEHOLDER
    BASECNKC --> BASERUN
    DEM --> NCPRUN
    CROSSWALK --> NCPRUN
    PLACEHOLDER --> NCPRUN
    BASERUN --> PIXEL
    NCPRUN --> PIXEL
    PIXEL --> MAPS --> FINDING

    classDef done fill:#c8e6c9,stroke:#2e7d32,color:#1b1b1b
    classDef progress fill:#fff9c4,stroke:#f9a825,color:#1b1b1b
    classDef blocked fill:#ffcdd2,stroke:#c62828,color:#1b1b1b
```

## Reading the diagram

**Two divergence points, both real and both flagged elsewhere in the comparison's caveats**: the
DEM (WWF-SIPA's inputs never included one — this project assumed standard SRTM, unconfirmed with
WWF-SIPA) and the rain events (WWF-SIPA's baseline presumably used the real spatially-distributed
product directly; the NCP run tried to reuse that same file and hit two real upstream `inspring`
bugs, falling back to a flat placeholder instead). Both are genuine, unintended departures from a
clean isolated-variable test — not modeling choices, real software/data compatibility problems,
documented in `swy_methods.qmd`'s "Real technical problems found and how each was handled."

**The LULC and climate stack are the one thing held constant** — reused directly, unmodified in
classification (only reprojected geometrically). That's what makes the CN/Kc crosswalk finding
meaningful at all: whatever else changed, the land-cover input itself didn't.

Full numbers, the crosswalk table, and the scatter-plot analysis: `swy_methods.qmd`'s "Test
design: Philippines" section and `docs/reports/swy_status_report.qmd`.
