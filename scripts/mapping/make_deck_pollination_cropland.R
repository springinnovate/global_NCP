# Pollination and nature access change by cropland expansion, per 10 km cell (lab deck, 2026-10-09).
#
# Supports the deck's "A 'gain' that signals conversion" slide: realized pollination on agriculture
# can rise because pollinator-dependent cropland expands next to remaining habitat, while access to
# nature falls in the same places. Cells are binned by the share of the cell that became cropland
# between 1992 and 2020 (ESA CCI, 10k_lcc_granular_metrics.gpkg, joined to the master grid through
# the LC crosswalk); change values are the 10 km grid's (Path B). Co-occurrence, not attribution.
#
# Usage: Rscript scripts/mapping/make_deck_pollination_cropland.R
# Writes: docs/presentations/images/pollination_vs_cropland.png and a CSV of the binned values

suppressPackageStartupMessages({ library(sf); library(dplyr); library(ggplot2); library(here) })

d <- readRDS(here("data", "processed", "dashboard", "grid_dashboard.rds"))
lcc <- st_read(here("data", "processed", "10k_lcc_granular_metrics.gpkg"),
               query = paste('SELECT grid_fid, Expansion_Gain_Cropland_2020_1992 AS crop_gain',
                             'FROM "10k_lcc_granular_metrics"'), quiet = TRUE)
cw <- read.csv(here("data", "processed", "lc_grid_fid_to_master_fid_crosswalk.csv"))
cw <- cw[cw$valid_match, ]
lcc$master_fid <- cw$master_fid[match(lcc$grid_fid, cw$lc_grid_fid)]
# several LC cells can map to one master cell: keep one per master cell
lcc <- lcc[!is.na(lcc$master_fid) & !duplicated(lcc$master_fid), ]

x <- d |>
  select(grid_fid, pollination_abs_chg, nature_access_pct_chg) |>
  inner_join(lcc |> select(master_fid, crop_gain), by = c("grid_fid" = "master_fid")) |>
  filter(!is.na(crop_gain), !is.na(pollination_abs_chg))
message("cells with both land cover change and pollination change: ", nrow(x))

x$bin <- cut(x$crop_gain, c(-Inf, 0, 2, 5, 10, 20, Inf),
             labels = c("None", "0-2", "2-5", "5-10", "10-20", ">20"))
s <- x |>
  group_by(bin) |>
  summarise(cells = n(),
            pollination_mean_abs = mean(pollination_abs_chg),
            nature_access_median_spc = median(nature_access_pct_chg, na.rm = TRUE),
            .groups = "drop")
print(s)
write.csv(s, here("outputs", "tables", "pollination_nature_access_by_cropland_gain.csv"), row.names = FALSE)

long <- bind_rows(
  s |> transmute(bin, v = pollination_mean_abs,
                 panel = "Pollination: mean change per cell\n(people fed equiv./ha)"),
  s |> transmute(bin, v = nature_access_median_spc,
                 panel = "Nature access: median change per cell\n(SPC, %)"))
long$panel <- factor(long$panel, levels = unique(long$panel))

p <- ggplot(long, aes(bin, v, fill = panel)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, colour = "grey40") +
  facet_wrap(~ panel, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("#009191", "#F07D00"), guide = "none") +
  labs(x = "Share of the 10 km cell that became cropland, 1992-2020 (%)", y = NULL) +
  theme_minimal(base_size = 15) +
  theme(strip.text = element_text(face = "bold", hjust = 0, size = 14),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())

out <- here("docs", "presentations", "images", "pollination_vs_cropland.png")
ggsave(out, p, width = 7, height = 6.2, dpi = 200, bg = "white")
message("wrote ", out)
