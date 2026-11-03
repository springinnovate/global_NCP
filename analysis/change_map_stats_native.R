# Numbers behind the change maps and the "Global Patterns of Change" text, from native pixels.
#
# Reads the native-SPC aggregates (analysis/native_spc.py: per 0.05 deg block, or ~6 km for nature
# access, the shares of native pixels with strong adverse / favourable change, changed by 1% or more,
# unchanged, and the summed 1992 and 2020 values) and links each block to its 10 km cell
# (analysis/native_grid_lookup.R). Only blocks in the evaluated cell universe are used. Shares are of
# land AREA: each block is weighted by its number of valid pixels times pixel area (cos(latitude) on
# the geographic grids; nature access is already equal-area). Service values are per-hectare densities,
# so totals (pollination gains) multiply by pixel area as well.
#
# Nitrogen and sediment export are modelled between 56 S and 60 N only; their statistics cover that
# modelled land (no pixels exist further north).
#
# Output: outputs/tables/change_map_stats_native/{services,regions,landscapes,frontier_by_region}.csv
# Run from the repo root (PowerShell): Rscript analysis/change_map_stats_native.R

suppressPackageStartupMessages({library(terra); library(sf); library(dplyr); library(tidyr); library(readr); library(here)})
source(here("R", "landscapes.R"))

nat <- here("data", "processed", "native_spc")
out_dir <- here("outputs", "tables", "change_map_stats_native")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- cell attributes (evaluated universe) ---------------------------------------------------------
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
attrs <- d |> select(grid_fid, region_wb, landscape, crop_gain)

# ---- blocks -----------------------------------------------------------------------------------------
services <- list(
  n_export      = list(file = "n_export_agg_f18.tif",      lookup = "grid_fid_005deg.tif", geo = TRUE),
  sed_export    = list(file = "sed_export_agg_f18.tif",    lookup = "grid_fid_005deg.tif", geo = TRUE),
  pollination   = list(file = "pollination_agg_f18.tif",   lookup = "grid_fid_005deg.tif", geo = TRUE),
  nature_access = list(file = "nature_access_agg_f3.tif",  lookup = "grid_fid_natacc_6km.tif", geo = FALSE))
adverse_up <- c("n_export", "sed_export")

blocks <- function(svc) {
  cfg <- services[[svc]]
  a <- rast(file.path(nat, cfg$file))
  lk <- rast(file.path(nat, cfg$lookup))
  if (cfg$geo) lk <- crop(lk, ext(a), snap = "near")   # rasters starting at 60 N: same columns, fewer rows
  stopifnot(nrow(lk) == nrow(a), ncol(lk) == ncol(a))
  ext(lk) <- ext(a)   # origins differ by 0.0003 deg (60.000417 vs a whole number of blocks below 90.000718)
  v <- as.data.frame(c(lk, a), xy = TRUE, na.rm = FALSE)
  names(v) <- c("x", "y", "grid_fid", "mean_spc", "spc_sums", "adv", "fav", "n", "chg1", "zero", "s1992", "s2020")
  v <- v |> filter(!is.na(grid_fid), n > 0, grid_fid %in% attrs$grid_fid)
  v$area <- if (cfg$geo) v$n * cos(v$y * pi / 180) else v$n
  v$service <- svc
  v |> left_join(attrs, by = "grid_fid")
}
b <- bind_rows(lapply(names(services), blocks))

wshare <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)])

# ---- services ---------------------------------------------------------------------------------------
svc_tab <- b |> group_by(service) |>
  summarise(blocks = n(),
            pct_changed_1 = 100 * wshare(chg1, area), pct_unchanged = 100 * wshare(zero, area),
            pct_adverse_strong = 100 * wshare(adv, area), pct_favourable_strong = 100 * wshare(fav, area),
            # favourable total change held by the most-improving 1% of the land area
            top1pct_area_share_of_favourable = {
              sign_adv <- if (first(service) %in% adverse_up) 1 else -1
              gain <- pmax(-(s2020 - s1992) * sign_adv, 0) * (area / n)   # per-ha density x pixel area
              o <- order(gain / area, decreasing = TRUE)
              cum_area <- cumsum(area[o]) / sum(area)
              100 * sum(gain[o][cum_area <= 0.01]) / sum(gain)
            }, .groups = "drop")

reg_tab <- b |> filter(!is.na(region_wb), region_wb != "Antarctica") |>
  group_by(service, region_wb) |>
  summarise(area = sum(area), adv_area = sum(adv * area), fav_area = sum(fav * area), .groups = "drop") |>
  group_by(service) |>
  mutate(pct_of_area = 100 * area / sum(area),
         pct_of_strong_adverse = 100 * adv_area / sum(adv_area),
         pct_of_strong_favourable = 100 * fav_area / sum(fav_area)) |>
  ungroup() |> select(service, region_wb, pct_of_area, pct_of_strong_adverse, pct_of_strong_favourable)

profile <- function(x) summarise(x,
  pct_area_new_cropland_ge10 = 100 * wshare(as.numeric(crop_gain >= 10), area),
  pct_strong_adverse = 100 * wshare(adv, area),
  pct_strong_favourable = 100 * wshare(fav, area), .groups = "drop")

land_tab <- bind_rows(
  b |> filter(!is.na(landscape)) |> group_by(landscape, service) |> profile(),
  b |> group_by(service) |> profile() |> mutate(landscape = "World"))

front_tab <- b |>
  filter(region_wb %in% c("Latin America & Caribbean", "Sub-Saharan Africa", "East Asia & Pacific", "South Asia"),
         !is.na(crop_gain)) |>
  mutate(cropland = if_else(crop_gain >= 10, "new cropland >= 10% of cell", "less or none")) |>
  group_by(region_wb, cropland, service) |> profile()

write_csv(svc_tab, file.path(out_dir, "services.csv"))
write_csv(reg_tab, file.path(out_dir, "regions.csv"))
write_csv(land_tab, file.path(out_dir, "landscapes.csv"))
write_csv(front_tab, file.path(out_dir, "frontier_by_region.csv"))
r1 <- function(x) mutate(x, across(where(is.numeric), ~ round(.x, 1)))
print(r1(svc_tab), width = 200); print(r1(reg_tab), n = Inf); print(r1(land_tab), n = Inf, width = 200)
print(r1(front_tab), n = Inf, width = 200)
