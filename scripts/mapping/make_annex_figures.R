# Annex figures in the paper's current style (full names, WWF orange, no in-figure titles):
#   - ES hotspot x land cover conversion driver overlap heatmap (production hotspot set, same
#     driver definition as analysis/attribution_null_model.R)
#   - KS statistic heatmap, labelled with Cliff's Delta (replaces the separate KS and directionality
#     figures; the Cliff's Delta values are also the Results table)
#   - serviceshed multiplier dumbbells by biome and income group
#
# Usage: Rscript scripts/mapping/make_annex_figures.R [out_root]   (default outputs/plots)

suppressMessages({library(sf); library(dplyr); library(tidyr); library(readr); library(ggplot2); library(here)})
setwd(here())
source("R/paths.R")
sf_use_s2(FALSE)

args <- commandArgs(trailingOnly = TRUE)
out_root <- if (length(args)) args[1] else "outputs/plots"

orange <- "#F07D00"
services <- c(N_export = "Nitrogen export", Sed_export = "Sediment export", C_Risk = "Coastal risk",
              Pollination = "Pollination", Nature_Access = "Nature access")
base_theme <- theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(), axis.text = element_text(size = 9))

# --- driver overlap -------------------------------------------------------------------------------
drv <- c(Forest_Loss = "Forest loss", Crop_Exp = "Cropland expansion", Urban_Exp = "Urban expansion",
         Grassland_Loss = "Grassland loss", Grassland_Gain = "Grassland gain")
lc <- st_read(file.path(data_dir(), "processed", "10k_lcc_granular_metrics.gpkg"), quiet = TRUE,
              query = paste0('SELECT grid_fid, ForestLoss_Loss_Forest_2020_1992 AS Forest_Loss, ',
                             'Expansion_Gain_Cropland_2020_1992 AS Crop_Exp, Expansion_Gain_Urban_2020_1992 AS Urban_Exp, ',
                             'GrasslandLoss_Loss_Grassland_2020_1992 AS Grassland_Loss, ',
                             'GrasslandLoss_Gain_Grassland_2020_1992 AS Grassland_Gain FROM "10k_lcc_granular_metrics"')) |>
  st_drop_geometry() |>
  inner_join(read.csv(file.path(data_dir(), "processed", "lc_grid_fid_to_master_fid_crosswalk.csv")) |>
               filter(valid_match) |> select(lc_grid_fid, master_fid, match_dist_m),
             by = c("grid_fid" = "lc_grid_fid")) |>
  select(-grid_fid) |> rename(grid_fid = master_fid) |>
  group_by(grid_fid) |> slice_min(match_dist_m, n = 1, with_ties = FALSE) |> ungroup()
for (d in names(drv)) lc[[d]] <- !is.na(lc[[d]]) & lc[[d]] > quantile(lc[[d]], 0.95, na.rm = TRUE)
es <- st_read(file.path(data_dir(), "processed", "hotspots", "pct", "global", "hotspots_global_pct.gpkg"),
              query = sprintf('SELECT grid_fid, %s FROM "hotspots_global_pct"', paste(names(services), collapse = ", ")),
              quiet = TRUE) |> st_drop_geometry()
ov <- expand_grid(service = names(services), driver = names(drv)) |>
  rowwise() |>
  mutate(pct = { ids <- es$grid_fid[es[[service]] %in% 1]
                 m <- lc[lc$grid_fid %in% ids, ]
                 100 * sum(m[[driver]]) / length(ids) }) |>
  ungroup() |>
  mutate(service = factor(services[service], levels = rev(services)), driver = factor(drv[driver], levels = drv))
print(ov |> pivot_wider(names_from = driver, values_from = pct), width = Inf)
p <- ggplot(ov, aes(driver, service, fill = pct)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), size = 3.2) +
  scale_fill_gradient(low = "#FFF4E8", high = "#C25E00", name = "Hotspot cells\nalso in a conversion\nhotspot (%)") +
  scale_x_discrete(position = "top", labels = scales::label_wrap(12)) +
  labs(x = NULL, y = NULL) + base_theme
dir.create(file.path(out_root, "drivers"), recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(out_root, "drivers", "heatmap_driver_overlap_pct.png"), p, width = 8, height = 3.4, dpi = 300, bg = "white")

# --- KS heatmap labelled with Cliff's Delta -------------------------------------------------------
vars <- c(GHS_POP_E2020_GLOBE_sum = "Population", rast_gdpTot_1990_2020_30arcsec_2020_sum = "GDP (total)",
          hdi_raster_predictions_2020_mean = "HDI", rast_adm1_gini_disp_2020_mean = "Income inequality (GINI)",
          fields_mehrabi_2017_mean = "Field size")
ks <- read_csv(file.path(data_dir(), "processed", "tables", "ks_results_hot_vs_non.csv"), show_col_types = FALSE) |>
  filter(service %in% names(services), var %in% names(vars)) |>
  mutate(service = factor(services[service], levels = services), var = factor(vars[var], levels = rev(vars)),
         label = sprintf("%+.2f%s", cliffs_delta, ifelse(p_adj < 0.05, "", " (ns)")))
p <- ggplot(ks, aes(service, var, fill = D)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = label), size = 3.2) +
  scale_fill_gradient(low = "#FFF4E8", high = "#C25E00", name = "KS statistic (D)") +
  scale_x_discrete(position = "top") +
  labs(x = NULL, y = NULL) + base_theme
dir.create(file.path(out_root, "ks"), recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(out_root, "ks", "ks_heatmap.png"), p, width = 8, height = 3.2, dpi = 300, bg = "white")

# --- multiplier dumbbells ---------------------------------------------------------------------------
drop <- c("Rock & Ice", "Lakes", "2. High income: nonOECD")
for (g in c("WWF_biome", "income_grp")) {
  m <- read_csv(file.path("outputs", "tables", paste0("multiplier_summary_", g, ".csv")), show_col_types = FALSE) |>
    rename(group = 1) |> filter(!group %in% drop, !is.na(group)) |>
    mutate(group = sub("^[0-9]\\. ", "", group),
           group = reorder(group, `Connected Beneficiaries`)) |>
    pivot_longer(c(`Local Residents`, `Connected Beneficiaries`), names_to = "who", values_to = "people") |>
    mutate(who = factor(who, levels = c("Local Residents", "Connected Beneficiaries"),
                        labels = c("Local residents (inside hotspot cells)", "Connected beneficiaries")))
  p <- ggplot(m, aes(people / 1e6, group)) +
    geom_line(aes(group = group), colour = "grey75", linewidth = 1.2) +
    geom_point(aes(colour = who), size = 2.6) +
    scale_colour_manual(values = c("grey30", orange), name = NULL) +
    scale_x_log10(labels = scales::label_comma(suffix = " M"), expand = expansion(mult = c(0.05, 0.08))) +
    labs(x = "People (log scale)", y = NULL) +
    theme_minimal(base_size = 10) + theme(legend.position = "bottom", panel.grid.minor = element_blank())
  ggsave(file.path(out_root, paste0("exposure_multiplier_dumbbell_", g, ".png")), p,
         width = 7.5, height = 1.2 + 0.25 * length(unique(m$group)), dpi = 300, bg = "white")
}
message("done")
