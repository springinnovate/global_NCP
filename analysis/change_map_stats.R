# Numbers behind the description of the global change maps (paper, "Global Patterns of Change").
#
# Per land service on the 10 km grid (SPC, cleaned as in plt_long: Inf -> +-200, SPC -> 0 where
# |absolute change| < 1e-6): share of cells that changed, share with strong change (20% or more)
# in the adverse and the favourable direction, and where the strong changes are (by World Bank
# region and by country).
#
# Output: outputs/tables/change_map_stats/{services,regions,countries}.csv
# Run from the repo root (PowerShell): Rscript analysis/change_map_stats.R

suppressPackageStartupMessages({library(dplyr); library(readr)})
source("R/paths.R")

out_dir <- file.path("outputs", "tables", "change_map_stats")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
strong <- 20

pl <- readRDS(file.path(data_dir(), "processed", "plt_long.rds")) |>
  filter(!continent %in% c("Antarctica", "Seven seas (Open Ocean)"), !WWF_biome %in% c("Lakes", "Rock & Ice"),
         service %in% c("N_export", "Sed_export", "Pollination", "Nature_Access")) |>
  mutate(service = as.character(service),
         spc = if_else(is.infinite(pct_chg), sign(pct_chg) * 200, pct_chg),
         spc = if_else(!is.na(abs_chg) & abs(abs_chg) < 1e-6, 0, spc),
         # positive = adverse: an increase for the exports, a decrease for pollination and nature access
         adv = spc * if_else(service %in% c("N_export", "Sed_export"), 1, -1),
         adv_abs = abs_chg * if_else(service %in% c("N_export", "Sed_export"), 1, -1)) |>
  filter(!is.na(spc))

services <- pl |> group_by(service) |>
  summarise(cells = n(),
            pct_unchanged = 100 * mean(spc == 0),
            pct_changed_1 = 100 * mean(abs(spc) >= 1),
            pct_adverse_1 = 100 * mean(adv >= 1), pct_favourable_1 = 100 * mean(adv <= -1),
            pct_adverse_strong = 100 * mean(adv >= strong), pct_favourable_strong = 100 * mean(adv <= -strong),
            # concentration: share of the total favourable ABSOLUTE change held by the top 1% of cells
            top1pct_share_of_favourable_abs = {
              f <- sort(pmax(-adv_abs, 0, na.rm = TRUE), decreasing = TRUE)
              100 * sum(f[seq_len(ceiling(0.01 * length(f)))]) / sum(f)
            })

regions <- pl |> group_by(service, region_wb) |>
  summarise(n_cells = n(), n_adverse = sum(adv >= strong), n_favourable = sum(adv <= -strong), .groups = "drop") |>
  group_by(service) |>
  mutate(pct_of_cells = 100 * n_cells / sum(n_cells),
         pct_of_strong_adverse = 100 * n_adverse / sum(n_adverse),
         pct_of_strong_favourable = 100 * n_favourable / sum(n_favourable)) |>
  ungroup()

countries <- pl |> filter(!is.na(nev_name)) |> group_by(service, nev_name) |>
  summarise(n_cells = n(), n_adverse = sum(adv >= strong), n_favourable = sum(adv <= -strong), .groups = "drop") |>
  group_by(service) |>
  mutate(pct_country_cells_adverse = 100 * n_adverse / n_cells,
         pct_country_cells_favourable = 100 * n_favourable / n_cells,
         pct_of_strong_adverse = 100 * n_adverse / sum(n_adverse),
         pct_of_strong_favourable = 100 * n_favourable / sum(n_favourable)) |>
  ungroup() |>
  arrange(service, desc(n_adverse))

write_csv(services, file.path(out_dir, "services.csv"))
write_csv(regions, file.path(out_dir, "regions.csv"))
write_csv(countries, file.path(out_dir, "countries.csv"))
print(services, width = 200)
