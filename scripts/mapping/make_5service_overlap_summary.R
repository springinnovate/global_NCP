# Summary stats + faceted bar chart for the 5-service hotspot overlap categories
# (water, access, combined cross-category, and the two access pairwise splits),
# companion to make_5service_overlap_maps.R -- same tier definitions, so the
# numbers here match the map legends exactly.

library(sf)
library(dplyr)
library(ggplot2)
library(here)
library(tidyr)

source(here("R", "paths.R"))

out_dir_tables <- here("outputs", "tables")
out_dir_plots <- here("outputs", "plots")
dir.create(out_dir_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir_plots, recursive = TRUE, showWarnings = FALSE)

gpkg_path <- file.path(data_dir(), "processed", "hotspots_5service", "pct", "global", "hotspots_global_5service_pct.gpkg")
d <- st_read(gpkg_path, quiet = TRUE) |> st_drop_geometry()

n_hotspots <- nrow(d)

# Total valid land grid cells (denominator for "share of land area"), same
# Antarctica/Lakes/Rock & Ice exclusion as extract_hotspots.R.
master_path <- file.path(data_dir(), "processed", "10k_change_calc.gpkg")
geom_check <- st_read(master_path, quiet = TRUE) |> st_drop_geometry()
if ("continent" %in% names(geom_check)) geom_check <- filter(geom_check, !continent %in% c("Antarctica", "Seven seas (Open Ocean)"))
if ("WWF_biome" %in% names(geom_check)) geom_check <- filter(geom_check, !WWF_biome %in% c("Lakes", "Rock & Ice"))
n_grid_total <- nrow(geom_check)

message(sprintf("Total 5-service hotspot cells: %d | Total valid land grid cells: %d", n_hotspots, n_grid_total))

mk_row <- function(category, tier, n) {
  tibble::tibble(
    category = category, tier = tier, n_cells = n,
    pct_of_hotspots = 100 * n / n_hotspots,
    pct_of_land_area = 100 * n / n_grid_total
  )
}

summary_df <- bind_rows(
  mk_row("Total 5-service hotspots", "all", n_hotspots),

  mk_row("Water overlap", "1 (N or Sed export)", sum(d$count_water == 1)),
  mk_row("Water overlap", "2 (both)", sum(d$count_water == 2)),

  mk_row("Access overlap", "1", sum(d$count_access == 1)),
  mk_row("Access overlap", "2", sum(d$count_access == 2)),
  mk_row("Access overlap", "3 (all three)", sum(d$count_access == 3)),

  mk_row("Combined cross-category", "2 (minimum: 1 water + 1 access)", sum(d$combined_cross == 1 & d$hotspot_count <= 2)),
  mk_row("Combined cross-category", "3+ (deeper overlap)", sum(d$combined_cross == 1 & d$hotspot_count >= 3)),

  mk_row("Access + Coastal Risk pair", "1 (either)", sum((d$Nature_Access + d$C_Risk) == 1)),
  mk_row("Access + Coastal Risk pair", "2 (both)", sum((d$Nature_Access + d$C_Risk) == 2)),

  mk_row("Access + Pollination pair", "1 (either)", sum((d$Nature_Access + d$Pollination) == 1)),
  mk_row("Access + Pollination pair", "2 (both)", sum((d$Nature_Access + d$Pollination) == 2))
)

out_csv <- file.path(out_dir_tables, "hotspots_5service_overlap_summary.csv")
write.csv(summary_df, out_csv, row.names = FALSE)
message("Wrote: ", out_csv)
print(summary_df, n = 20)

# ------------------------------------------------------------------------------
# Faceted bar chart -- one panel per category, bars = tiers, matching the map
# legends' tier definitions and colors exactly.
# ------------------------------------------------------------------------------

plot_df <- summary_df |> filter(category != "Total 5-service hotspots")

# Preserve a sensible category order (roughly matching the map delivery order)
plot_df$category <- factor(plot_df$category, levels = c(
  "Water overlap", "Access overlap", "Combined cross-category",
  "Access + Coastal Risk pair", "Access + Pollination pair"
))

p <- ggplot(plot_df, aes(x = tier, y = n_cells, fill = tier)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::comma(n_cells)), vjust = -0.4, size = 3.2) +
  facet_wrap(~category, scales = "free", ncol = 3) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "5-Service Hotspot Overlap Categories -- Cell Counts by Tier",
    subtitle = sprintf("Percentage-change metric, global scope. %s total 5-service hotspot cells (%.2f%% of %s valid land grid cells).",
                        scales::comma(n_hotspots), 100 * n_hotspots / n_grid_total, scales::comma(n_grid_total)),
    x = NULL, y = "Number of 10km cells"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    axis.text.x = element_text(angle = 20, hjust = 1),
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank()
  )

out_png <- file.path(out_dir_plots, "hotspots_5service_overlap_summary_faceted.png")
ggsave(out_png, p, width = 14, height = 8, dpi = 300, bg = "white")
message("Wrote: ", out_png)
