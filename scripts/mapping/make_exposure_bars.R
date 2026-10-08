# Population exposure by number of overlapping hotspots: local residents and connected
# beneficiaries as two bar panels on a linear scale (replaces the log-scale dumbbell,
# downstream_exposure_dumbbell_compound.png, per PI comment BC12).
#
# Both from the same population layer (LandScan 2023) and the same hotspot set:
# Local residents: LandScan 2023 summed over the production hotspot cells with at least k
#   overlapping hotspots (hotspots_global_pct.gpkg, hotspot_count).
# Connected beneficiaries: union of downstream (50 km) and travel-time (1 h) reach from the
#   2026-07-29 beneficiary run on the 5-service hotspots (data/processed/beneficiaries/
#   run_2026-07-29_5service/, LandScan 2023). Its input hotspot set matches the production one (189,918 of
#   189,932 cells shared; the rest are pollination ties). The June run in
#   data/archive/superseded/2026-06_beneficiaries_8var/ (outputs/tables/exposure_comparison*.csv) was built
#   on the earlier 8-variable hotspots and is superseded.
#
# Usage: Rscript scripts/mapping/make_exposure_bars.R
# Writes: outputs/plots/exposure_by_overlap_bars.png, outputs/tables/exposure_by_overlap.csv

suppressMessages({library(sf); library(dplyr); library(readr); library(tidyr); library(ggplot2); library(here)})
setwd(here())

landscan <- "C:/Users/JerónimoRodríguezEsc/data/global_ncp/Raw/Beneficiaries/Landscan/landscan-global-2023-assets/landscan-global-2023.tif"
suppressMessages({library(terra); library(exactextractr)})
ls_r <- rast(landscan)
hot_geom <- st_read("data/processed/hotspots/pct/global/hotspots_global_pct.gpkg", quiet = TRUE)[, c("grid_fid", "hotspot_count")]
hot <- st_drop_geometry(hot_geom) |>
  mutate(pop = exact_extract(ls_r, st_transform(hot_geom, crs(ls_r)), "sum", progress = FALSE))
world_pop <- global(ls_r, "sum", na.rm = TRUE)[1, 1]

levels_k <- c(`1+` = 1, `2+` = 2, `3+` = 3, `4+` = 4)
local <- tibble(level = names(levels_k),
                local = vapply(levels_k, function(k) sum(hot$pop[hot$hotspot_count >= k], na.rm = TRUE), numeric(1)),
                cells = vapply(levels_k, function(k) sum(hot$hotspot_count >= k), numeric(1)))

run_dir <- "data/processed/beneficiaries/run_2026-07-29_5service"
conn <- lapply(1:4, function(k) {
  f <- Sys.glob(file.path(run_dir, sprintf("*hotspot_count_%dplus_beneficiaries", k), "*.csv"))
  stopifnot(length(f) == 1)
  read_csv(f, show_col_types = FALSE) |>
    transmute(level = paste0(k, "+"), connected = union_population, downstream = downstream_50k_population,
              travel = within_travel_time_population)
}) |> bind_rows()

tab <- inner_join(local, conn, by = "level") |>
  mutate(ratio = connected / local, pct_world = 100 * connected / world_pop,
         level = factor(level, levels = names(levels_k)))
stopifnot(nrow(tab) == 4)
write_csv(tab, "outputs/tables/exposure_by_overlap.csv")
print(tab |> mutate(across(c(local, connected, downstream, travel), ~ round(.x / 1e6))))
cat(sprintf("LandScan 2023 world population: %.0f M\n", world_pop / 1e6))

long <- tab |>
  select(level, `Local residents (live in hotspot cells)` = local,
         `Connected beneficiaries (downstream or within 1 h travel)` = connected) |>
  pivot_longer(-level, names_to = "group", values_to = "people") |>
  mutate(group = factor(group, levels = unique(group)))

p <- ggplot(long, aes(level, people / 1e9)) +
  geom_col(fill = "#F07D00", width = 0.65) +
  geom_text(aes(label = ifelse(people >= 1e9, sprintf("%.1f B", people / 1e9), sprintf("%.0f M", people / 1e6))), vjust = -0.4, size = 3.4) +
  facet_wrap(~ group, nrow = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Number of overlapping hotspots in the cell", y = "People (billions)") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold", size = 10),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())

ggsave("outputs/plots/exposure_by_overlap_bars.png", p, width = 9, height = 3.8, dpi = 300, bg = "white")
message("wrote outputs/plots/exposure_by_overlap_bars.png and outputs/tables/exposure_by_overlap.csv")
