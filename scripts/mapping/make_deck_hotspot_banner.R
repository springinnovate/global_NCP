# Wide, legend-free world map of multi-service hotspots of decline for the lab deck's
# "Three Questions" slide (2026-10-08). Same hotspot definition as the paper (each service's
# 5% most adverse SPC change over the evaluated cells) and the palette of the paper's hotspot
# count map; drawn from the explorer's prepared data (scripts/dashboard/prepare_dashboard_data.R).
#
# Usage: Rscript scripts/mapping/make_deck_hotspot_banner.R
# Writes: docs/presentations/images/hotspot_banner.png

suppressPackageStartupMessages({ library(ggplot2); library(sf); library(here) })

d <- readRDS(here("data", "processed", "dashboard", "grid_dashboard.rds"))
layers <- readRDS(here("data", "processed", "dashboard", "change_map_layers.rds"))

services <- c("n_export", "sed_export", "c_risk", "pollination", "nature_access")
adverse_up <- c("n_export", "sed_export", "c_risk")
n_hot <- integer(nrow(d))
for (s in services) {
  v <- d[[paste0(s, "_pct_chg")]]
  up <- s %in% adverse_up
  cut <- quantile(v, if (up) 0.95 else 0.05, na.rm = TRUE, names = FALSE)
  n_hot <- n_hot + (!is.na(v) & (if (up) v >= cut else v <= cut))
}
message("cells in at least one hotspot: ", sum(n_hot > 0))

px <- layers$pix$world
px$n <- pmin(n_hot[px$row], 4)
px <- px[px$n > 0, ]
px$n <- factor(px$n, levels = 1:4)

# drop Antarctica from the frame: the map is a band under the slide text
base <- layers$base
ylim <- c(-6.3e6, 8.4e6)

p <- ggplot() +
  geom_sf(data = base, fill = "gray93", color = "gray82", linewidth = 0.08) +
  geom_raster(data = px, aes(x, y, fill = n)) +
  scale_fill_manual(values = c("1" = "#FDD44C", "2" = "#FB8C00", "3" = "#E53935", "4" = "#800026"),
                    guide = "none") +
  coord_sf(crs = "EPSG:8857", xlim = c(-1.25e7, 1.45e7), ylim = ylim, expand = FALSE, datum = NA) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = NA))

out <- here("docs", "presentations", "images", "hotspot_banner.png")
dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
suppressWarnings(ggsave(out, p, width = 16, height = 16 * diff(ylim) / 2.7e7, dpi = 200, bg = "white"))
message("wrote ", out)

# same map with its legend inside the frame (South Pacific), for the WHERE slide: the paper's PNG has
# wide margins and a stray line across the Arctic
p_leg <- p +
  scale_fill_manual(values = c("1" = "#FDD44C", "2" = "#FB8C00", "3" = "#E53935", "4" = "#800026"),
                    labels = c("1", "2", "3", "4+"), name = "Overlapping\nhotspots") +
  guides(fill = guide_legend(ncol = 1)) +
  theme(legend.position = "inside", legend.position.inside = c(0.01, 0.12), legend.justification = c(0, 0),
        legend.title = element_text(size = 22), legend.text = element_text(size = 22),
        legend.key.size = unit(0.9, "cm"))
out2 <- here("docs", "presentations", "images", "hotspot_map_legend.png")
suppressWarnings(ggsave(out2, p_leg, width = 16, height = 16 * diff(ylim) / 2.7e7, dpi = 200, bg = "white"))
message("wrote ", out2)
