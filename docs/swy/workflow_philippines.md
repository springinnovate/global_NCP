# SWY Philippines comparison: workflow and status

Working diagram for the Philippines Kc/CN comparison, status 2026-09-25. Status colors: 🟩 done ·
🟨 started / scoped · ⬜ not started. Update this alongside `HANDOFF.md` and `research_notes.md`;
this is the map, those are the log. Run and figure dictionary: `docs/reports/swy/FIGURES.md`.

```mermaid
flowchart TD
    subgraph SHARED["Shared by every run: WWF-SIPA inputs + our 90m grid"]
        LULC["WWF-SIPA LULC<br/>12 classes"]:::done
        CLIM["Precipitation + ET0<br/>30-yr CMIP6 climatology"]:::done
        RAINEV["Spatially distributed rain events<br/>(3 inspring bugs patched)"]:::done
        SOIL["Soil group: HYSOGs250m"]:::done
        DEM["SRTMGL3 DEM, 90m<br/>grid-nested to WWF-SIPA's grid"]:::done
    end

    subgraph REF["Reference: WWF-SIPA parameters"]
        HER30["WWF-SIPA's own 30m run"]:::done
        REFRUN["Her CN + Kc through our pipeline (09h)<br/>reproduces the 30m run, r=0.92<br/>= reference for all comparisons"]:::done
    end

    subgraph OURS["Our parameters (global data)"]
        CN["CN: GCN250 via ESA crosswalk<br/>+ HYSOGs soil group per pixel (07b)"]:::done
        KCF["Kc forest: Negrón Juárez 2008 (07d)"]:::done
        KCG["Kc grass/shrub: Oliveira 2015 (07e)"]:::done
        KCN["Kc other classes: Kamble on NDVI,<br/>per pixel (07g, fixes EVI error)"]:::done
        KCP["Kc paddy: FAO-56 rice + Sacks calendar<br/>+ MapSPAM rice share (07h)"]:::done
        OURRUN["Our current run (09n)"]:::done
    end

    subgraph RESULT["Comparison against the reference (20)"]
        OVERALL["Baseflow 1.03x overall (r=0.66)<br/>forest ~1.3x, most classes close"]:::done
        CROP["Gap concentrated in annual crop (2.0x)<br/>her CN alone brings it to 1.25x:<br/>cropland CN is the main open problem"]:::done
    end

    subgraph NEXT["Next"]
        CROPCN["Cropland CN (runoff):<br/>main remaining gap"]:::todo
        TABLE["Global cropland table:<br/>Köppen x crop group x water regime<br/>scoped: 9 cells = 80% of crop area"]:::progress
        REPO["Move SWY to its own repo<br/>for handover"]:::todo
    end

    LULC --> CN
    LULC --> OURRUN
    SHARED --> REFRUN
    HER30 -.->|check| REFRUN
    KCF --> OURRUN
    KCG --> OURRUN
    KCN --> OURRUN
    KCP --> OURRUN
    CN --> OURRUN
    SHARED --> OURRUN
    REFRUN --> OVERALL
    OURRUN --> OVERALL
    OVERALL --> CROP
    CROP --> CROPCN
    CROP --> TABLE

    subgraph LEGEND["Legend"]
        direction LR
        LDONE["Done"]:::done
        LPROG["Started / scoped"]:::progress
        LTODO["Not started"]:::todo
    end

    classDef done fill:#c8e6c9,stroke:#2e7d32,color:#1b1b1b
    classDef progress fill:#fff9c4,stroke:#f9a825,color:#1b1b1b
    classDef todo fill:#f5f5f5,stroke:#9e9e9e,color:#1b1b1b
```

## Reading the diagram

**Every run shares the same inputs and the same 90m grid.** The only thing that differs between
runs is CN and Kc. WWF-SIPA's own CN and Kc, run through this pipeline (09h), reproduce their 30m
run (r=0.92 for quickflow and baseflow), so the pipeline and resolution are ruled out and 09h is
the reference for every comparison.

**Our parameters come from global data.** CN from GCN250; Kc per pixel and per month from MODIS,
using the closest published calibration for each land-cover type, plus a paddy rice Kc for annual
crop.

**The result points to one place.** Overall baseflow is close to the reference (1.03x), but annual
crop is 2.0x, and swapping in WWF-SIPA's CN alone brings it to 1.25x. Cropland CN is the main open
problem; the global cropland table (climate x crop group x water regime) is the proposed way to
handle it at global scale, scoped in `docs/swy/global_cropland_parameterization_plan.md`.

Full numbers and figures: `docs/reports/swy/swy_status_report.qmd`.
