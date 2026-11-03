# Socioeconomic profile of the beneficiary (serviceshed) masks -- phase 4, audited and extended.
#
# Reads the 10 km grid (10k_change_calc.gpkg, attributes only) and the mask coverage fractions
# zonal-extracted from the 2026-07-29 beneficiary run (Python_scripts/zonal_extract_beneficiary_masks.py),
# flags a cell as inside a mask when >= 50% of its area is covered, and writes:
#
#   cells.csv       cell-level KS D + Cliff's delta, inside vs outside, every mask x variable
#                   (reproduces analysis/KS_tests_beneficiary_masks.qmd for its three categories)
#   people.csv      population-weighted profile: the people inside vs outside each mask
#                   (GHS-POP 2020 weights), with weighted medians, means and a weighted Cliff's delta
#   strata.csv      Gini / HDI / GDP per capita deltas pooled vs stratified by WB region and by country,
#                   plus an admin-unit check (one value per sub-national Gini unit)
#   by_region.csv   cell-level delta within each WB region
#   checks.csv      data-quality checks (HDI gaps, Gini range, reproduction of the published table)
#
# Output folder: outputs/tables/beneficiary_masks/. Read by
# docs/reports/phase4_beneficiary/phase4_beneficiary_disproportionality_report.qmd.
#
# Run from the repo root (PowerShell; sf segfaults under Git Bash):
#   Rscript analysis/beneficiary_mask_profile.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
})

source("R/paths.R")

out_dir <- file.path("outputs", "tables", "beneficiary_masks")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

coverage_threshold <- 0.5

# ---- grid attributes ---------------------------------------------------------------------------

gpkg <- file.path(data_dir(), "processed", "10k_change_calc.gpkg")
cols <- c("grid_fid", "continent", "nev_name", "region_wb", "income_grp", "WWF_biome",
          "GHS_POP_E2020_GLOBE_sum", "rast_gdpTot_1990_2020_30arcsec_2020_sum",
          "hdi_raster_predictions_2020_mean", "rast_adm1_gini_disp_2020_mean")
grid <- sf::st_read(gpkg, quiet = TRUE,
                    query = sprintf('SELECT %s FROM "10k_change_calc"',
                                    paste0('"', cols, '"', collapse = ", "))) |>
  as_tibble() |>
  rename(pop = GHS_POP_E2020_GLOBE_sum, gdp = rast_gdpTot_1990_2020_30arcsec_2020_sum,
         hdi = hdi_raster_predictions_2020_mean, gini = rast_adm1_gini_disp_2020_mean)
stopifnot(!anyDuplicated(grid$grid_fid))

# same exclusions as the cell universe (docs/cell_universe.md)
grid <- grid |>
  filter(!continent %in% c("Antarctica", "Seven seas (Open Ocean)"),
         !WWF_biome %in% c("Lakes", "Rock & Ice"))
stopifnot(nrow(grid) == 1372621L)

# The Gini layer carries impossible values in the Guiana Shield (Suriname and French Guiana = 1.39,
# Guyana ~0.78, blended into neighbouring Brazilian cells): drop them.
gini_bad <- with(grid, is.finite(gini) & gini > 0.75 &
                   nev_name %in% c("Suriname", "France", "Guyana", "Brazil"))
n_gini_bad <- sum(gini_bad)
grid$gini_raw <- grid$gini
grid$gini[gini_bad] <- NA_real_

# GDP per capita (2020): cell GDP / cell GHS population, only where people live
grid <- grid |> mutate(gdp_pc = if_else(pop >= 1, gdp / pop, NA_real_))

vars <- c(gdp = "GDP (total)", pop = "Population", gdp_pc = "GDP per capita",
          hdi = "HDI", gini = "Gini", gini_raw = "Gini (as published, before cleanup)")

# ---- masks ---------------------------------------------------------------------------------------

tables_dir <- file.path(data_dir(), "processed", "tables")
cov_union <- read_csv(file.path(tables_dir, "beneficiary_mask_coverage_10km.csv"),
                      show_col_types = FALSE)
cov_inter <- read_csv(file.path(tables_dir, "intersection_mask_coverage_10km.csv"),
                      show_col_types = FALSE) |>
  mutate(category = paste0(category, "_inter"))

masks <- bind_rows(cov_union, cov_inter) |>
  filter(category != "tier_5plus") |>                      # ~0.03% of land, too small to profile
  mutate(inside = coverage_frac >= coverage_threshold) |>
  select(grid_fid, category, inside) |>
  pivot_wider(names_from = category, values_from = inside)

mask_ids <- setdiff(names(masks), "grid_fid")
grid <- grid |> left_join(masks, by = "grid_fid")
stopifnot(!anyNA(grid[mask_ids]))

# ---- helpers -------------------------------------------------------------------------------------

# Weighted Cliff's delta: P(X > Y) - P(X < Y) for X drawn from x with weights wx, Y from y with wy.
# With unit weights it equals the rank-based delta (2 * AUC - 1) used elsewhere in the project.
wdelta <- function(x, y, wx = rep(1, length(x)), wy = rep(1, length(y))) {
  ok_x <- is.finite(x) & is.finite(wx) & wx > 0; ok_y <- is.finite(y) & is.finite(wy) & wy > 0
  x <- x[ok_x]; wx <- wx[ok_x]; y <- y[ok_y]; wy <- wy[ok_y]
  if (length(x) < 2 || length(y) < 2) return(NA_real_)
  agg <- rowsum(wy, y, reorder = TRUE)            # sorted unique y with summed weights
  ys <- as.numeric(rownames(agg)); cw <- c(0, cumsum(agg[, 1]))
  Wy <- cw[length(cw)]
  w_lt <- cw[findInterval(x, ys, left.open = TRUE) + 1]   # weight of y strictly below x
  w_le <- cw[findInterval(x, ys) + 1]                     # weight of y at or below x
  sum(wx * (w_lt - (Wy - w_le))) / (sum(wx) * Wy)
}

wquantile <- function(x, w, p) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  o <- order(x); x <- x[o]; cw <- cumsum(w[o]) / sum(w)
  vapply(p, function(pp) x[which(cw >= pp)[1]], numeric(1))
}

# ---- 1. cell level: reproduce and extend ----------------------------------------------------------

cells <- map_dfr(mask_ids, function(m) {
  map_dfr(names(vars), function(v) {
    x <- grid[[v]][grid[[m]]]; y <- grid[[v]][!grid[[m]]]
    x <- x[is.finite(x)]; y <- y[is.finite(y)]
    tibble(mask = m, var = v,
           n_in = length(x), n_out = length(y),
           median_in = median(x), median_out = median(y),
           mean_in = mean(x), mean_out = mean(y),
           q10_in = quantile(x, .1, names = FALSE), q90_in = quantile(x, .9, names = FALSE),
           q10_out = quantile(y, .1, names = FALSE), q90_out = quantile(y, .9, names = FALSE),
           ks_D = unname(suppressWarnings(ks.test(x, y, exact = FALSE))$statistic),
           ks_p = suppressWarnings(ks.test(x, y, exact = FALSE))$p.value,
           delta = wdelta(x, y))
  })
}) |>
  group_by(var) |> mutate(ks_p_adj = p.adjust(ks_p, "BH")) |> ungroup()
write_csv(cells, file.path(out_dir, "cells.csv"))

# ---- 2. people level -------------------------------------------------------------------------------

pop_total <- sum(grid$pop, na.rm = TRUE)
people <- map_dfr(mask_ids, function(m) {
  inside <- grid[[m]]
  map_dfr(c("gdp_pc", "hdi", "gini"), function(v) {
    d <- grid |> transmute(val = .data[[v]], w = pop, inside = inside) |>
      filter(is.finite(val), is.finite(w), w > 0)
    di <- filter(d, inside); do <- filter(d, !inside)
    tibble(mask = m, var = v,
           pop_in = sum(grid$pop[inside], na.rm = TRUE),
           pct_pop_in = 100 * sum(grid$pop[inside], na.rm = TRUE) / pop_total,
           pct_cells_in = 100 * mean(inside),
           pop_covered_by_var = sum(d$w) / pop_total,
           wmedian_in = wquantile(di$val, di$w, .5), wmedian_out = wquantile(do$val, do$w, .5),
           wq25_in = wquantile(di$val, di$w, .25), wq75_in = wquantile(di$val, di$w, .75),
           wq25_out = wquantile(do$val, do$w, .25), wq75_out = wquantile(do$val, do$w, .75),
           wmean_in = weighted.mean(di$val, di$w), wmean_out = weighted.mean(do$val, do$w),
           delta_people = wdelta(di$val, do$val, di$w, do$w))
  })
})
# GDP per capita of the group as a whole = group GDP / group population
people <- people |>
  left_join(map_dfr(mask_ids, function(m) {
    d <- filter(grid, pop >= 1, is.finite(gdp))
    tibble(mask = m, var = "gdp_pc",
           pooled_in = sum(d$gdp[d[[m]]]) / sum(d$pop[d[[m]]]),
           pooled_out = sum(d$gdp[!d[[m]]]) / sum(d$pop[!d[[m]]]))
  }), by = c("mask", "var"))
write_csv(people, file.path(out_dir, "people.csv"))

# ---- 3. composition checks: is the Gini signal a between-region effect? --------------------------

# Stratified delta: within-stratum deltas combined with weights n_in * n_out (the number of
# inside/outside pairs that stratum contributes). Equal to the pooled delta restricted to pairs
# that share a stratum.
stratified_delta <- function(val, inside, stratum, weight = NULL) {
  d <- tibble(val, inside, stratum, w = if (is.null(weight)) 1 else weight) |>
    filter(is.finite(val), !is.na(stratum), w > 0)
  per <- d |> group_by(stratum) |>
    summarise(n_in = sum(inside), n_out = sum(!inside),
              W_in = sum(w[inside]), W_out = sum(w[!inside]),
              delta = if (n_in >= 2 && n_out >= 2)
                wdelta(val[inside], val[!inside], w[inside], w[!inside]) else NA_real_,
              .groups = "drop") |>
    filter(!is.na(delta))
  sum(per$delta * per$W_in * per$W_out) / sum(per$W_in * per$W_out)
}

# Sub-national Gini units are not in the grid. The source values have three decimals, so a cell
# lying inside one unit carries a three-decimal value and a cell straddling a boundary a blend;
# a unit is approximated by (country, three-decimal value) over the interior cells.
grid <- grid |> mutate(gini_unit = if_else(is.finite(gini) & abs(gini * 1000 - round(gini * 1000)) < 0.01,
                                           paste(nev_name, round(gini, 3)), NA_character_))

strata_masks <- c("tier_1plus", "water", "access", "combined_cross", "tier_3plus", "tier_4plus")
strata <- map_dfr(strata_masks, function(m) {
  inside <- grid[[m]]
  map_dfr(c("gini", "hdi", "gdp_pc"), function(v) {
    val <- grid[[v]]
    # admin-unit check (Gini only): one row per unit, inside if most of its cells are inside
    unit_delta <- NA_real_; n_units <- NA_integer_
    if (v == "gini") {
      u <- grid |> filter(!is.na(gini_unit)) |>
        group_by(gini_unit) |> summarise(gini = first(gini), share_in = mean(.data[[m]]), .groups = "drop")
      unit_delta <- wdelta(u$gini[u$share_in >= .5], u$gini[u$share_in < .5])
      n_units <- nrow(u)
    }
    no_lac <- grid$region_wb != "Latin America & Caribbean"
    tibble(mask = m, var = v,
           pooled_cells = wdelta(val[inside], val[!inside]),
           within_region_cells = stratified_delta(val, inside, grid$region_wb),
           within_country_cells = stratified_delta(val, inside, grid$nev_name),
           pooled_cells_no_lac = wdelta(val[inside & no_lac], val[!inside & no_lac]),
           pooled_people = wdelta(val[inside], val[!inside], grid$pop[inside], grid$pop[!inside]),
           within_region_people = stratified_delta(val, inside, grid$region_wb, grid$pop),
           within_country_people = stratified_delta(val, inside, grid$nev_name, grid$pop),
           admin_units_delta = unit_delta, n_admin_units = n_units)
  })
})
write_csv(strata, file.path(out_dir, "strata.csv"))

by_region <- map_dfr(strata_masks, function(m) {
  grid |> filter(!is.na(region_wb)) |>
    group_by(region_wb) |>
    summarise(mask = m,
              n_cells = n(), pct_in = 100 * mean(.data[[m]]),
              gini_median = median(gini, na.rm = TRUE),
              delta_gini = wdelta(gini[.data[[m]]], gini[!.data[[m]]]),
              delta_hdi = wdelta(hdi[.data[[m]]], hdi[!.data[[m]]]),
              delta_gdp_pc = wdelta(gdp_pc[.data[[m]]], gdp_pc[!.data[[m]]]),
              .groups = "drop")
})
write_csv(by_region, file.path(out_dir, "by_region.csv"))

# ---- 4. checks -------------------------------------------------------------------------------------

old <- read_csv(file.path(tables_dir, "ks_results_beneficiary_masks.csv"), show_col_types = FALSE) |>
  mutate(var = recode(var, GHS_POP_E2020_GLOBE_sum = "pop",
                      rast_gdpTot_1990_2020_30arcsec_2020_sum = "gdp",
                      hdi_raster_predictions_2020_mean = "hdi",
                      rast_adm1_gini_disp_2020_mean = "gini_raw")) |>
  select(mask = service, var, delta_published = cliffs_delta, D_published = D,
         n_in_published = n_hot, n_out_published = n_non)
repro <- old |> left_join(cells |> select(mask, var, delta, ks_D, n_in, n_out), by = c("mask", "var")) |>
  mutate(abs_diff_delta = abs(delta - delta_published))

checks <- tibble(
  check = c("max |delta - published| (12 published values)",
            "cells with Gini > 0.75 in the Guiana Shield (set to NA)", "Gini range min", "Gini range max",
            "cells without HDI (of 1,372,621)", "cells without HDI that have >= 1 person",
            "people in cells without HDI (% of GHS-POP)",
            "people in cells without Gini (% of GHS-POP)",
            "world GDP per capita (sum GDP / sum GHS-POP)"),
  value = c(max(repro$abs_diff_delta),
            n_gini_bad, min(grid$gini, na.rm = TRUE), max(grid$gini, na.rm = TRUE),
            sum(!is.finite(grid$hdi)), sum(!is.finite(grid$hdi) & grid$pop >= 1, na.rm = TRUE),
            100 * sum(grid$pop[!is.finite(grid$hdi)], na.rm = TRUE) / pop_total,
            100 * sum(grid$pop[!is.finite(grid$gini)], na.rm = TRUE) / pop_total,
            sum(grid$gdp[grid$pop >= 1], na.rm = TRUE) / sum(grid$pop[grid$pop >= 1], na.rm = TRUE)))
write_csv(checks, file.path(out_dir, "checks.csv"))
write_csv(repro, file.path(out_dir, "reproduction.csv"))

print(checks, n = Inf)
print(repro, n = Inf)
