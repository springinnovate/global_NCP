# Robustness of the WHO-section socioeconomic tests (analysis/KS_tests_hotspots.qmd) to geography.
#
# Rebuilds the notebook's groups (5% SPC hotspots per service vs the median 5% band of change,
# same extract_hotspots() call and config) and reports Cliff's delta for Gini, HDI and GDP per
# capita three ways: pooled (as in the paper), within World Bank region, and within country
# (within-stratum deltas combined with weights n_hot * n_background). A pooled signal that
# vanishes within countries comes from which countries the hotspots are in, not from where they
# sit inside those countries. Also reruns the pooled Gini delta with the Gini layer's impossible
# values removed (Suriname and French Guiana carry Gini = 1.39; Guyana ~0.78).
#
# Output: outputs/tables/ks_hotspots_stratified.csv
# Run from the repo root (PowerShell): Rscript analysis/ks_hotspots_stratified.R

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
})
source("R/paths.R")
devtools::load_all(quiet = TRUE)

plt_long <- readRDS(file.path(data_dir(), "processed", "plt_long.rds")) |>
  filter(!continent %in% c("Antarctica", "Seven seas (Open Ocean)"),
         !WWF_biome %in% c("Lakes", "Rock & Ice"),
         service %in% service_names())

hs <- extract_hotspots(
  df = plt_long, value_col = "pct_chg", pct_cutoff = 0.05,
  threshold_mode = "percent", rule_mode = "vectors",
  loss_services = hotspot_direction_lists(looking_for = "decline")$loss_services,
  gain_services = hotspot_direction_lists(looking_for = "decline")$gain_services,
  combos = list(deg_combo = service_names(), rec_combo = service_names()),
  id_cols = c("fid")
)

# median 5% band of change per service, as in the notebook
background <- hs$non_hotspots_df |>
  group_by(service) |>
  filter(pct_chg >= quantile(pct_chg, 0.475, na.rm = TRUE),
         pct_chg <= quantile(pct_chg, 0.525, na.rm = TRUE)) |>
  ungroup()

d <- bind_rows(mutate(hs$hotspots_df, hot = TRUE), mutate(background, hot = FALSE)) |>
  mutate(service = as.character(service),
         gini = rast_adm1_gini_disp_2020_mean,
         gini_clean = if_else(gini > 0.75 & nev_name %in% c("Suriname", "France", "Guyana"), NA_real_, gini),
         hdi = hdi_raster_predictions_2020_mean,
         pop = GHS_POP_E2020_GLOBE_sum,
         gdp_pc = if_else(pop >= 1, rast_gdpTot_1990_2020_30arcsec_2020_sum / pop, NA_real_))

# Cliff's delta (ties count 0), O(n log n)
cdelta <- function(x, y) {
  x <- x[is.finite(x)]; y <- y[is.finite(y)]
  if (length(x) < 2 || length(y) < 2) return(NA_real_)
  ys <- sort(y)
  lt <- findInterval(x, ys, left.open = TRUE); le <- findInterval(x, ys)
  mean(lt - (length(ys) - le)) / length(ys)
}
strat <- function(val, hot, s) {
  per <- tibble(val, hot, s) |> filter(is.finite(val), !is.na(s)) |>
    group_by(s) |>
    summarise(nh = sum(hot), nb = sum(!hot),
              dl = if (nh >= 2 && nb >= 2) cdelta(val[hot], val[!hot]) else NA_real_, .groups = "drop") |>
    filter(!is.na(dl))
  w <- as.numeric(per$nh) * per$nb
  sum(per$dl * w) / sum(w)
}

out <- d |>
  group_by(service) |>
  group_modify(function(g, ...) {
    map_dfr(c("gini", "gini_clean", "hdi", "gdp_pc"), function(v) {
      tibble(var = v, n_hot = sum(g$hot & is.finite(g[[v]])), n_bg = sum(!g$hot & is.finite(g[[v]])),
             pooled = cdelta(g[[v]][g$hot], g[[v]][!g$hot]),
             within_region = strat(g[[v]], g$hot, g$region_wb),
             within_country = strat(g[[v]], g$hot, g$nev_name),
             pct_hot_in_lac = 100 * mean(g$region_wb[g$hot] == "Latin America & Caribbean", na.rm = TRUE),
             pct_bg_in_lac = 100 * mean(g$region_wb[!g$hot] == "Latin America & Caribbean", na.rm = TRUE))
    })
  }) |>
  ungroup()

write_csv(out, file.path("outputs", "tables", "ks_hotspots_stratified.csv"))
print(out, n = Inf, width = 200)
