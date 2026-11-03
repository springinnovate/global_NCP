# North-south profile of change across West and Central Africa (the band south of the Sahara that
# stands out in the change maps).
#
# For 0.5-degree latitude bands between 0 and 25 N, 17 W to 40 E, area-weighted shares of land with
# strong adverse / favourable change per service (native-pixel aggregates, as in
# analysis/change_map_stats_native.R), next to land cover change in the same bands (10 km grid,
# ESA CCI): share of the cell that switched between natural and transformed (cropland or urban) cover,
# and that became cropland. The natural/transformed switch misses change WITHIN natural cover, so a
# second table gives, per 1-degree band, grassland gain and loss, forest loss and gain and cropland gain
# from the granular metrics (10k_lcc_granular_metrics.gpkg): the 15-18 N band is a grassland gain.
#
# Output: outputs/tables/change_map_stats_native/sahel_profile.csv, sahel_landcover_by_band.csv
# Run from the repo root (PowerShell): Rscript analysis/sahel_profile.R

suppressPackageStartupMessages({library(terra); library(sf); library(dplyr); library(tidyr); library(readr); library(here)})

nat <- here("data", "processed", "native_spc")
d <- readRDS(here("data", "processed", "dashboard", "grid_dashboard.rds")) |> select(grid_fid)
lcc <- st_read(here("data", "processed", "10k_lcc_granular_metrics.gpkg"),
               query = paste('SELECT grid_fid, Expansion_Gain_Cropland_2020_1992 AS crop_gain',
                             'FROM "10k_lcc_granular_metrics"'), quiet = TRUE) |> st_drop_geometry()
lco <- st_read(here("data", "processed", "10k_lcc_metrics.gpkg"),
               query = 'SELECT grid_fid, Gain_Overall_2020_1992 AS lc_gain, Loss_Overall_2020_1992 AS lc_loss FROM "10k_lcc_metrics"',
               quiet = TRUE) |> st_drop_geometry()
lcc <- full_join(lcc, lco, by = "grid_fid")
cw <- read.csv(here("data", "processed", "lc_grid_fid_to_master_fid_crosswalk.csv")); cw <- cw[cw$valid_match, ]
lcc$master_fid <- cw$master_fid[match(lcc$grid_fid, cw$lc_grid_fid)]
lcc <- lcc[!is.na(lcc$master_fid) & !duplicated(lcc$master_fid), ] |> select(-grid_fid)
attrs <- d |> left_join(lcc, by = c("grid_fid" = "master_fid"))

files <- c(n_export = "n_export_agg_f18.tif", sed_export = "sed_export_agg_f18.tif",
           pollination = "pollination_agg_f18.tif")
lk_all <- rast(file.path(nat, "grid_fid_005deg.tif"))
prof <- bind_rows(lapply(names(files), function(svc) {
  a <- rast(file.path(nat, files[[svc]]))
  lk <- crop(lk_all, ext(a), snap = "near"); ext(lk) <- ext(a)
  e <- ext(-17, 40, 0, 25)
  v <- as.data.frame(c(crop(lk, e), crop(a[[c(3, 4, 5)]], e)), xy = TRUE, na.rm = FALSE)
  names(v) <- c("x", "y", "grid_fid", "adv", "fav", "n")
  v |> filter(!is.na(grid_fid), n > 0, grid_fid %in% attrs$grid_fid) |>
    left_join(attrs, by = "grid_fid") |>
    mutate(service = svc, band = floor(y * 2) / 2, w = n * cos(y * pi / 180))
}))
out <- prof |> group_by(service, band) |>
  summarise(pct_strong_adverse = 100 * sum(adv * w) / sum(w),
            pct_strong_favourable = 100 * sum(fav * w) / sum(w),
            lc_changed_pct = weighted.mean(pmax(lc_gain, lc_loss, na.rm = TRUE), w, na.rm = TRUE),
            crop_gain_pct = weighted.mean(crop_gain, w, na.rm = TRUE), .groups = "drop")
write_csv(out, here("outputs", "tables", "change_map_stats_native", "sahel_profile.csv"))
print(out |> filter(service == "n_export") |> select(band, lc_changed_pct, crop_gain_pct) |>
        mutate(across(everything(), ~ round(.x, 2))), n = Inf)
print(out |> select(service, band, pct_strong_adverse, pct_strong_favourable) |>
        pivot_wider(names_from = service, values_from = c(pct_strong_adverse, pct_strong_favourable)) |>
        mutate(across(everything(), ~ round(.x, 1))), n = Inf, width = 200)

# ---- land cover change per 1-degree band, granular metrics (change within natural cover included) ----
g_cols <- c("GrasslandLoss_Gain_Grassland_2020_1992", "GrasslandLoss_Loss_Grassland_2020_1992",
            "ForestLoss_Loss_Forest_2020_1992", "ForestLoss_Gain_Forest_2020_1992",
            "Expansion_Gain_Cropland_2020_1992")
gr <- st_read(here("data", "processed", "10k_lcc_granular_metrics.gpkg"),
              query = sprintf('SELECT grid_fid, %s FROM "10k_lcc_granular_metrics"', paste(g_cols, collapse = ", ")),
              quiet = TRUE) |> st_drop_geometry()
gr$master_fid <- cw$master_fid[match(gr$grid_fid, cw$lc_grid_fid)]
gr <- gr[!is.na(gr$master_fid) & !duplicated(gr$master_fid), ] |> select(-grid_fid)
bands <- readRDS(here("data", "processed", "dashboard", "grid_dashboard.rds")) |>
  mutate(lon = (xmin + xmax) / 2, lat = (ymin + ymax) / 2) |>
  filter(lon > -17, lon < 40, lat > 4, lat < 22) |>
  inner_join(gr, by = c("grid_fid" = "master_fid")) |>
  group_by(band = floor(lat)) |>
  summarise(cells = n(),
            grassland_gain_pct = mean(GrasslandLoss_Gain_Grassland_2020_1992, na.rm = TRUE),
            grassland_loss_pct = mean(GrasslandLoss_Loss_Grassland_2020_1992, na.rm = TRUE),
            forest_loss_pct = mean(ForestLoss_Loss_Forest_2020_1992, na.rm = TRUE),
            forest_gain_pct = mean(ForestLoss_Gain_Forest_2020_1992, na.rm = TRUE),
            cropland_gain_pct = mean(Expansion_Gain_Cropland_2020_1992, na.rm = TRUE), .groups = "drop")
write_csv(bands, here("outputs", "tables", "change_map_stats_native", "sahel_landcover_by_band.csv"))
print(mutate(bands, across(where(is.double), ~ round(.x, 2))), n = Inf)
