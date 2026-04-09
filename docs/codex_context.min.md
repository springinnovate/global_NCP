# Global NCP – AI Context (v1.3.1)
*Attach this file to all coding prompts to ensure correct paths, schemas, and architectural awareness.*

## 1. Architecture & Pipeline
*   **Python Pre-Processing:** Uses `taskgraph` & `exactextract` to generate zonal stats (1992/2020) over a 10km grid.
*   **R Analysis Runbook:**
    1.  `process_data.qmd`: Consolidates GPKGs, calculates change.
    2.  `LC_change_preparation.qmd` & `LC_change_granular.qmd`: Land cover metrics (`diffeR`).
    3.  `hotspot_extraction.qmd`: Flags 5% extreme tails, attributes LCC drivers, generates maps/plots.
    4.  `KS_tests_hotspots.qmd`: Socioeconomic profiling (KS tests, Cliff's Delta).

## 2. Core Methodological Quirks
*   **Symmetric Percentage Change (SPC):** We use SPC bounded at `[-200%, 200%]` instead of absolute change to handle zero-baselines and accurately measure ecosystem shocks relative to local conditions.
*   **The Fragment Bug (Patch v1.3.1):** Upstream Python `gdf.explode()` fractured 1.5M grid cells into 1.67M fragments. `process_data.qmd` patches this using `sf::st_intersects` and mathematical re-aggregation (`group_by %>% summarise`) to securely snap fragments back into the pristine 10km grid.
*   **V2 Refactor (TODO):** Python now exports `orig_fid` prior to exploding. Soon, we will remove the `st_intersects` patch and revert to tabular `left_join(by = "orig_fid")`.
*   **Balanced Sampling (KS Tests):** We compare the extreme 5% hotspots against the **Median 5%** (47.5th-52.5th percentiles) of the background landscape to prevent class imbalance.

## 3. Data Dictionary
*   **Canonical 8 Services:**
    *   **Loss Services** (Bad when going DOWN): `Nature_Access`, `Pollination`, `N_Ret_Ratio`, `Sed_Ret_Ratio`, `C_Risk_Red_Ratio`.
    *   **Gain Services** (Bad when going UP): `Sed_export`, `N_export`, `C_Risk`.
*   **Grouping Variables:** `income_grp`, `region_wb`, `WWF_biome`, `nev_name` (Country).
*   **Key Files:**
    *   Master Grid: `/home/jeronimo/data/global_ncp/vector_basedata/AOOGrid_10x10km_land.gpkg`
    *   Clean Output: `processed/10k_change_calc.gpkg`
    *   Pivot Table: `processed/plt_long.rds` (Used by Hotspot/KS pipelines)

## 4. Coding Conventions
*   **Paths:** Never hardcode paths. Use `data_dir()`, `out_plots()`, and `here::here()`.
*   **Roxygen:** Maintain documentation for functions in `R/`.
*   **Dependencies:** `tidyverse`, `sf`, `terra`, `exactextractr`. Use `sf::sf_use_s2(TRUE)`.
*   **Performance:** Drop heavy geometries when aggregating (`st_drop_geometry()`). Clean memory with `rm(); gc()`.

## 5. Hotspot Configuration (`HOTS_CFG`)
Hotspot thresholds are defined centrally in `hotspot_extraction.qmd`.
```R
HOTS_CFG <- list(
  pct_cutoff      = 0.05,            # Extreme 5% tail
  loss            = c("Nature_Access","Pollination","N_Ret_Ratio","Sed_Ret_Ratio","C_Risk_Red_Ratio"),
  gain            = c("Sed_export","N_export","C_Risk"),
  combos          = list(...)        # Overlapping combinations
)
```
*Do not hardcode threshold logic; always rely on `extract_hotspots()`.*