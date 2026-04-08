# Global NCP: Data Structure & Dictionary

This document outlines the directory structure and defines the core datasets used and produced by the Global NCP Time-Series Analysis (1992–2020).

**Local Data Root:** `/home/jeronimo/data/global_ncp/`

---

## Directory Tree Overview

```text
global_ncp/
├── raw/                    # Original, unmodified source data (rasters)
├── vector_basedata/        # Canonical grids and administrative boundaries
├── interim/                # Intermediate spatial extractions (Python outputs)
└── processed/              # Final analysis-ready datasets (R outputs)
    ├── hotspots/           # Extracted ES hotspot geometries and indices
    ├── tables/             # Summary statistics and KS test results
    └── ...                 # Canonical change calculation GPKGs
```

---

## 1. `raw/` (Source Data)
Contains the fundamental, unmodified raster and tabular datasets downloaded from external sources.
*   **Ecosystem Services (ES):** InVEST model outputs for 1992 and 2020 (Nitrogen Export, Sediment Export, Pollination, Coastal Risk, USLE).
*   **Land Cover:** ESA CCI 300m historical land cover data (reclassified for granular driver models).
*   **Socioeconomic:** Gridded population density, GDP, Human Development Index (HDI), and Gini coefficient data.

## 2. `vector_basedata/` (Spatial Foundations)
Contains the canonical geometries used for spatial extraction and aggregation.
*   `AOOGrid_10x10km_land_4326_clean.gpkg`: The primary IUCN 10x10km equal-area grid (land-only) used as the backbone of the analysis.
*   `cartographic_ee_r264_correspondence.gpkg`: Administrative boundaries (World Bank Regions, UN Regions, Income Groups, Countries) projected to Equal Earth (EPSG:8857).
*   `Biome.gpkg`: WWF Terrestrial Ecoregions and Biomes classifications.

## 3. `interim/` (Intermediate Processing)
Contains the outputs of the Python `exactextract` and `zonal_stats_toolkit` geosharding pipeline. These files represent raw zonal statistics prior to time-series calculations.
*   `10k_grid_synth_serv_<timestamp>.gpkg`: Extracted mean/sum statistics for Ecosystem Services.
*   `10k_grid_synth_benef_<timestamp>.gpkg`: Extracted mean/sum statistics for Socioeconomic beneficiaries.

## 4. `processed/` (Analysis-Ready Data)
This directory contains the final datasets utilized by the R/Quarto analysis notebooks. **These are the core files recommended for public data sharing (e.g., Zenodo/Dryad).**

### Core Grids and Tidy Data
*   `10k_change_calc.gpkg`: **The Single Source of Truth.** This file merges the interim geometries, computes the absolute and Symmetric Percentage Change (SPC) between 1992 and 2020, and attaches all regional/biome attribute tags.
*   `plt_long.rds` (and/or `.csv`): A computationally efficient, tidy (long-format) version of `10k_change_calc.gpkg`. It reshapes the data to be strictly row-based (`fid`, `service`, `abs_chg`, `pct_chg`) for fast visualization and KS testing.
*   `10k_lcc_granular_metrics.gpkg`: Land Cover Change metrics computed via the `diffeR` methodology. Contains columns tracking the gross loss of forest (`Forest_Loss`) and the gross gain of anthropogenic footprints (`Crop_Exp`, `Urban_Exp`).

### `processed/hotspots/`
Contains the geometries and indices of Ecosystem Service decline hotspots (the worst 5% of change globally).
*   `_hotspots_index.csv`: A lightweight lookup table acting as a directory for all generated hotspot files. Details the scope, grouping, metric, and filepath for each generated `.gpkg`.
*   `pct/global/hotspots_global_pct.gpkg`: The definitive global geometries of relative ES hotspots.
*   `pct/<grouping>/hotspots_<grouping>_<value>_pct.gpkg`: Subregional hotspot geometries (e.g., hotspots specific to Sub-Saharan Africa or Low-Income countries).
*   `drivers_by_group/`: Land Cover Change (driver) hotspots representing the top 5% of physical landscape conversion.

### `processed/tables/`
Contains tabular summary statistics extracted for the manuscript and presentations.
*   `hotspot_area_stats.csv`: Quantifies the **Intensity** (percentage of a region's landmass flagged as a hotspot) and **Enrichment** (Observed share vs. Expected share).
*   `hotspot_multiservice_stats.csv`: Quantifies **Hotness**, showing the overlap of multiple service declines within single grid cells.
*   `ks_results_hot_vs_non.csv`: Results of the Kolmogorov-Smirnov statistical tests, comparing the socioeconomic profiles of hotspots vs. non-hotspot "business-as-usual" median baselines.
*   `lcc_es_hotspot_overlap.csv`: The **"Attribution Gap"** metrics. Details the exact percentage of ES hotspots that spatially overlap with Land Cover Conversion hotspots.

---

## 5. Ecosystem Service Naming Conventions

The analytical pipeline normalizes raw model outputs into a set of 8 "Canonical Services" to guarantee consistent plotting and rule application. Note the directionality: for some metrics, an increase is a "Gain" in service provision, while for others, an increase represents a "Loss" (or increase in risk/damage).

*   `N_export`: Nitrogen Export (InVEST NDR). *Direction:* **Loss** (Higher values = worse water quality / more pollution).
*   `N_Ret_Ratio`: Nitrogen Retention Ratio. The proportion of the total nitrogen load retained by the landscape. *Direction:* **Gain** (Higher values = better service).
*   `Sed_export`: Sediment Export (InVEST SDR). *Direction:* **Loss** (Higher values = worse water quality / more erosion).
*   `Sed_Ret_Ratio`: Sediment Retention Ratio. The proportion of soil retained. *Direction:* **Gain** (Higher values = better service).
*   `Pollination`: People fed by pollinator-dependent crops (InVEST Pollination). *Direction:* **Gain** (Higher values = better service).
*   `Nature_Access`: Access to nature / open space. *Direction:* **Gain** (Higher values = better service).
*   `C_Risk`: Coastal Vulnerability Index (InVEST CV). *Direction:* **Loss** (Higher values = greater risk to coastal populations).
*   `C_Risk_Red_Ratio`: Coastal Risk Reduction Ratio. The proportion of coastal risk mitigated by the presence of natural habitats. *Direction:* **Gain** (Higher values = better service).

*(Note: When identifying "Hotspots" of decline, the pipeline automatically looks for the top 5% highest values for Loss services, and the bottom 5% lowest values for Gain services).*