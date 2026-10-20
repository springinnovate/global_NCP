# Geography of change for the paper's "Global Patterns of Change": how each service changed in a few
# landscapes that dominate the change maps, and whether cells where cropland expanded change the
# same way in South America, Africa and Asia.
#
# Landscapes are approximated from country, WWF biome and cell latitude/longitude (the grid carries
# no ecoregion field), e.g. the Cerrado as Brazil's tropical savanna biome. Cropland expansion is the
# share of the 10 km cell that became cropland between 1992 and 2020 (ESA CCI,
# 10k_lcc_granular_metrics.gpkg via the LC crosswalk, as in make_deck_pollination_cropland.R).
# Change is SPC on the 10 km grid; "strong" = 20% or more; positive "adverse" = an increase for the
# exports, a decrease for pollination and nature access. Co-occurrence, not attribution.
#
# Output: outputs/tables/change_map_stats/{landscapes,frontier_by_region}.csv
# Run from the repo root (PowerShell): Rscript analysis/change_geography.R

suppressPackageStartupMessages({library(sf); library(dplyr); library(tidyr); library(readr); library(here)})
source(here("R", "landscapes.R"))

out_dir <- here("outputs", "tables", "change_map_stats")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

d <- readRDS(here("data", "processed", "dashboard", "grid_dashboard.rds")) |>
  mutate(lon = (xmin + xmax) / 2, lat = (ymin + ymax) / 2)

lcc <- st_read(here("data", "processed", "10k_lcc_granular_metrics.gpkg"),
               query = paste('SELECT grid_fid, Expansion_Gain_Cropland_2020_1992 AS crop_gain',
                             'FROM "10k_lcc_granular_metrics"'), quiet = TRUE) |> st_drop_geometry()
cw <- read.csv(here("data", "processed", "lc_grid_fid_to_master_fid_crosswalk.csv"))
cw <- cw[cw$valid_match, ]
lcc$master_fid <- cw$master_fid[match(lcc$grid_fid, cw$lc_grid_fid)]
lcc <- lcc[!is.na(lcc$master_fid) & !duplicated(lcc$master_fid), c("master_fid", "crop_gain")]
d <- left_join(d, lcc, by = c("grid_fid" = "master_fid"))

d$landscape <- assign_landscape(d)

long <- d |>
  select(grid_fid, region_wb, landscape, crop_gain,
         n_export = n_export_pct_chg, sed_export = sed_export_pct_chg,
         pollination = pollination_pct_chg, nature_access = nature_access_pct_chg) |>
  pivot_longer(n_export:nature_access, names_to = "service", values_to = "spc") |>
  filter(!is.na(spc)) |>
  mutate(spc = pmax(pmin(spc, 200), -200),
         adv = spc * if_else(service %in% c("n_export", "sed_export"), 1, -1))

profile <- function(x) summarise(x,
  cells = n(),
  pct_new_cropland_ge10 = 100 * mean(crop_gain >= 10, na.rm = TRUE),
  median_spc = median(spc),
  pct_strong_adverse = 100 * mean(adv >= 20),
  pct_strong_favourable = 100 * mean(adv <= -20),
  .groups = "drop")

landscapes <- long |> filter(!is.na(landscape)) |> group_by(landscape, service) |> profile() |>
  bind_rows(long |> group_by(service) |> profile() |> mutate(landscape = "World"))
frontier <- long |>
  filter(region_wb %in% c("Latin America & Caribbean", "Sub-Saharan Africa", "East Asia & Pacific", "South Asia"),
         !is.na(crop_gain)) |>
  mutate(cropland = if_else(crop_gain >= 10, "new cropland >= 10% of cell", "less or none")) |>
  group_by(region_wb, cropland, service) |> profile()

write_csv(landscapes, file.path(out_dir, "landscapes.csv"))
write_csv(frontier, file.path(out_dir, "frontier_by_region.csv"))
print(landscapes |> mutate(across(where(is.numeric), ~ round(.x, 1))), n = Inf, width = 200)
print(frontier |> mutate(across(where(is.numeric), ~ round(.x, 1))), n = Inf, width = 200)
