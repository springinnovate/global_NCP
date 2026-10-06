# Rebuild hotspot_area_stats.csv from the production hotspot set (the 189,932 SPC hotspot cells in
# data/processed/hotspots/pct/global/hotspots_global_pct.gpkg that the paper reports), with each
# service's denominator restricted to the cells where that service has a value. Coastal risk exists
# only on coastal cells; computing its expected share against all land made every coastal biome
# look over-represented (Mangroves 15x) and every inland one under-represented.
#
# Same columns and grouping rules as hotspot_synthesis.qmd. Writes to a separate file unless
# --replace is given, so the old table can be compared first.
#
# Usage: Rscript analysis/rebuild_hotspot_area_stats.R [--replace]

suppressMessages({library(sf); library(dplyr); library(tidyr); library(readr); library(here)})
setwd(here())

services <- c(N_export = "n_export_pct_chg", Sed_export = "sed_export_pct_chg", C_Risk = "c_risk_pct_chg",
              Pollination = "pollination_pct_chg", Nature_Access = "nature_access_pct_chg")
groupings <- c("income_grp", "region_wb", "WWF_biome", "nev_name")
exclude_groups <- c("Seven seas (Open Ocean)", "Seven seas (open ocean)", "Antarctica", "Lakes", "Rock & Ice",
                    "2. High income: nonOECD")

cells <- st_read("data/processed/10k_change_calc.gpkg",
                 query = sprintf('SELECT grid_fid, %s, %s FROM "10k_change_calc"',
                                 paste(groupings, collapse = ", "), paste(services, collapse = ", ")),
                 quiet = TRUE) |> st_drop_geometry()
hot <- st_read("data/processed/hotspots/pct/global/hotspots_global_pct.gpkg",
               query = sprintf('SELECT grid_fid, %s FROM "hotspots_global_pct"', paste(names(services), collapse = ", ")),
               quiet = TRUE) |> st_drop_geometry()

long <- lapply(names(services), function(s) {
  tibble(grid_fid = cells$grid_fid, service = s, value = cells[[services[[s]]]]) |>
    bind_cols(cells[groupings]) |>
    filter(!is.na(value)) |>
    mutate(is_hot = grid_fid %in% hot$grid_fid[hot[[s]] %in% 1])
}) |> bind_rows()

global_n_hot <- long |> group_by(service) |> summarise(global_n_hot = sum(is_hot))

stats <- lapply(groupings, function(g) {
  long |> filter(!is.na(.data[[g]]), !.data[[g]] %in% exclude_groups) |>
    group_by(service, group = .data[[g]]) |>
    summarise(n_total = n(), n_hot = sum(is_hot), .groups = "drop") |>
    mutate(pct_area = 100 * n_hot / n_total) |>
    group_by(service) |>
    # shares among cells that have a group: about 45% of coastal risk hotspots sit in coastal cells
    # with no biome or income group, which would otherwise pull every coastal ratio below 1
    mutate(global_n_hot = sum(n_hot),
           pct_share = 100 * n_hot / global_n_hot,
           grouping_var = g,
           expected_share = 100 * n_total / sum(n_total),
           relative_intensity = ifelse(expected_share > 0, pct_share / expected_share, NA)) |>
    ungroup() |> arrange(desc(pct_area))
}) |> bind_rows() |>
  select(service, group, n_total, n_hot, pct_area, global_n_hot, pct_share, grouping_var, expected_share, relative_intensity)

print(global_n_hot)
out <- if ("--replace" %in% commandArgs(trailingOnly = TRUE)) "data/processed/tables/hotspot_area_stats.csv" else
  "data/processed/tables/hotspot_area_stats_rebuilt.csv"
write_csv(stats, out)
message("wrote ", out)
