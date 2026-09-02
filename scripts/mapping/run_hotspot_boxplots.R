# ==============================================================================
# Standalone entry point for make_hotspot_boxplots.R -- fast iteration on the boxplot
# figures without a full hotspot_extraction.qmd render (reads its cached plt_long.rds instead).
#
# Usage: Rscript scripts/mapping/run_hotspot_boxplots.R
# ==============================================================================

library(here)

source(here("scripts", "mapping", "make_hotspot_boxplots.R"))

plt_long <- readRDS(here("data", "processed", "plt_long.rds"))

# Ratios dropped 2026-09-02: never part of the hotspot definition (amounts only), so showing
# them in "Hotspot Magnitude by Biome" was inconsistent with that decision.
generate_hotspot_boxplots(
  plt_long,
  groupings     = c("income_grp", "region_wb", "WWF_biome", "nev_name"),
  pct_cutoff    = 0.05,
  loss_services = hotspot_direction_lists(looking_for = "decline")$loss_services,
  all_services  = service_names()
)
