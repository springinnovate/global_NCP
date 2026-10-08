# Population exposure outputs from the June beneficiary run (archived 2026-10-07)

Tables and figures built from Rich's June beneficiary rasters, which used the earlier hotspot set with
8 variables (5 services + 3 ratios; 225,113 cells): `exposure_comparison*.csv`,
`multiplier_summary_*.csv` (also copied into `outputs/data_package/`), the dumbbell figures
(`downstream_exposure_dumbbell_compound.png` was paper Figure "multiplier compound" until 10-07),
`multiplier_effect.png`, and the `pop_exposure_*.png` bin charts (from the June
`hotspot_pop_exposure.csv`). Producer: `archive/scripts/plot_multiplier_effect.R`.

Old headline numbers here: 3,065 M local residents, 7,584 M connected (96.5%), 2.5x.

Replaced by (current hotspots, 2026-07-29 beneficiary run, LandScan 2023):
`outputs/tables/exposure_by_overlap.csv` + `outputs/plots/exposure_by_overlap_bars.png`
(`scripts/mapping/make_exposure_bars.R`) and `outputs/tables/exposure_by_income.csv`
(`scripts/exposure_by_income.R`): 2,367 M local, 7,383 M connected (92.5%), ~3.1x.

Still pointing here (to update): book chapters 02 and 06, `scripts/mapping/make_annex_figures.R`,
`scripts/audit_claims.R`, `Python_scripts/extract_book_data_fills.py`.
