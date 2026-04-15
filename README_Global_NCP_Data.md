# Global NCP Time-Series Analysis Data (1992–2020)

This archive contains the analysis-ready datasets, spatial layers, and visual outputs for the Global NCP Time-Series project, identifying epicenters of Ecosystem Service (ES) decline.

## Directory Guide

### 1. `processed/` (The Core Analysis Data)
This is the most important folder for QGIS exploration.
*   **`10k_change_calc.gpkg`**: The master 10km grid dataset. Contains all absolute and percentage change values for every service, plus regional/biome attribute tags.
*   **`tables/`**: Contains the summary CSVs including `hotspot_area_stats.csv` (Intensity/Enrichment), `hotspot_multiservice_stats.csv` (Compound Risk/Hotness), and the Attribution Gap summaries.
*   **`hotspots/`**: Contains isolated GPKG vector layers showing *only* the grid cells that fall into the top/bottom 5% extreme change (Hotspots).

### 2. `outputs/` (Visualizations)
Contains all the generated PNG figures ready for presentations or manuscript insertion.
*   `plots/signed_bars/`: Directional bar charts showing regional mean change.
*   `plots/intensity/` & `plots/hotness/`: Bar charts highlighting Compound Risk and Disproportionate Burden (Enrichment).
*   `plots/drivers/`: Overlap charts showing the Attribution Gap (ES Hotspots vs. Land Cover drivers).

### 3. `vector_basedata/`
Contains the canonical 10km IUCN Equal-Area reference grid.

---

## Important Methodological Notes

*   **What is a Hotspot?** A "hotspot" is defined as a grid cell falling in the most extreme **5% of relative change** globally for a specific service. It is a relative ranking (an epicenter of shock), not an absolute physical threshold.
*   **Symmetric Percentage Change (SPC):** Relative change is calculated using SPC, bounding distributions between -200% (Total Loss) and +200% (New Emergence). This successfully normalizes for the baseline size of the ecosystem and prevents infinite value artifacts.
*   **Loss vs. Gain Services:** For services like Nature Access or Retention Ratios, hotspots represent steep *declines*. For services like Nitrogen/Sediment Export or Coastal Risk, hotspots represent steep *increases* in damage.