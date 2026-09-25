# Philippines SWY comparison: guide to the runs and figures

A companion to the status report (`swy_status_report.html`). It explains what each model run is, why it was run, and what it showed, and what each figure displays.

## The two parameters being compared

Every run uses the same model (InVEST Seasonal Water Yield, through `inspring`) and the same inputs: WWF-SIPA's land cover, climate and rain events, the HYSOGs250m soil map, and a 90m DEM. The runs differ only in two parameters, set per land-cover class or per pixel:

- **CN (curve number)** controls how much rain runs off right away (quickflow) instead of soaking in. Higher CN, more quickflow.
- **Kc (crop coefficient)** controls how much of the water that soaks in is used by vegetation (evapotranspiration). Higher Kc, more water used.

Baseflow is roughly what soaks in minus what vegetation uses, so it depends on both. Quickflow depends on CN only.

"Her" / "WWF-SIPA" parameters are the CN and Kc from WWF-SIPA's Philippines run. "Our" parameters are built from global data: CN from GCN250; Kc per pixel and per month from MODIS vegetation indices, using a published calibration matched to each land-cover type (tropical forest, savanna/shrub, cropland, and paddy rice).

## The runs

| Run | CN | Kc | Why it was run | What it showed |
|---------------|---------------|---------------|---------------|---------------|
| **Reference** | hers | hers | Checks the pipeline: WWF-SIPA's own parameters, run through our pipeline at 90m | Reproduces WWF-SIPA's 30m run closely (r=0.92 for quickflow and baseflow). Used as the reference for every comparison below, so differences reflect only the CN/Kc choice |
| **Ours** | ours | ours | Our global-data parameters as they stand | Baseflow 1.03x the reference overall (r=0.66). Most classes close; forest about 1.3x; annual crop 2.0x |
| **Her CN + our Kc** | hers | ours | Isolates the effect of CN: only Kc differs from the reference | Annual crop drops to 1.25x. So most of the annual crop gap comes from CN |
| **Our CN + her Kc** | ours | hers | Isolates the effect of Kc: only CN differs from the reference | Annual crop stays high (1.9x). Each class lines up on its own straight band, offset by the CN difference |

In short: our crop water use (Kc) is now close to WWF-SIPA's; our crop runoff (CN) is not, and that is the main remaining gap.

## The figures

**Interactive map** (in the report). Quickflow and baseflow for the reference and for our run, on a shared color scale, plus land cover and elevation as background layers. Clicking a point shows the land-cover class, elevation, and both runs' values. Switching between the land cover layer and the baseflow layers is the quickest way to see that the differences follow cropland. Two test layers come from the tree-crop Kc check on perennial crop: that run's baseflow, and the change from our current run (red: less baseflow, blue: more). The average barely moves, but the change layer shows local shifts, mostly more baseflow in the coconut belt (Bicol, Samar, Leyte, northern Mindanao) and less in southeastern Mindanao.

**Baseflow scatter plots** (`swy_ph_cn_kc_scatter_b.png`). One panel per run above (except the reference), each pixel's value plotted against the reference, colored by land-cover class. Points on the dashed line match the reference. The "our CN + her Kc" panel shows clean straight bands because WWF-SIPA's Kc is a single value per class.

**Quickflow scatter plot** (`swy_ph_cn_kc_scatter_qf.png`). One panel only: quickflow depends on CN alone, so every run with WWF-SIPA's CN matches the reference exactly, and every run with our CN is identical to the others.

**Global cropland scoping** (`global_scoping_*.png`). Not Philippines results: these look ahead to a global run. They overlay global crop area (MapSPAM 2020) on climate zones (Köppen-Geiger) to show how many climate × crop combinations a global cropland table would need. Nine combinations cover 80% of the world's crop area. Details in `docs/swy/global_cropland_parameterization_plan.md`.

## Run codes, for the code and data

| Label above | Run script | Workspace (`data/swy/philippines/`) |
|------------------------|------------------------|------------------------|
| Reference | `09h` | `workspace_becky_inputs_90m_herCN_herKc` |
| Ours | `09n` | `workspace_becky_inputs_90m_paddy_kc` |
| Her CN + our Kc | `09m` | `workspace_becky_inputs_90m_herCN_ndvi_perpixel_kc` |
| Our CN + her Kc | `09f` | `workspace_becky_inputs_90m_ourCN_herKc` |
| Ours with FAO-56 tree-crop Kc on perennial crop (check, not in figures) | `09o` | `workspace_becky_inputs_90m_tree_crops_kc` |

Figures come from `Python_scripts/swy_philippines_run/20_cn_kc_comparison.py` (scatter plots), `10c`, `10e` and `11` (map), and `Python_scripts/swy_global_scoping/` (global scoping). Per-class numbers: `data/swy/philippines/comparison_maps/cn_kc_summary.csv` and `cn_kc_by_class.csv`. Earlier intermediate runs (`09e`-`09l`) are superseded; their history is in `docs/swy/research_notes.md`.