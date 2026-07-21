# ==============================================================================
# Global-extent thumbnail maps (IDB-WWF workshop deck, intro slide)
# ==============================================================================
# Small global-extent versions of both LAC maps, same color ramps/styling as
# make_lac_hotspot_map.R and make_lac_critical_assets_map.R, to show side by
# side on the intro slide ("these are available globally") before zooming to
# LAC on the following slides. Per Becky's request 2026-07-10.

library(sf)
library(terra)
library(ggplot2)
library(dplyr)
library(here)
library(rnaturalearth)

source(here("R", "paths.R"))

out_dir <- here("outputs", "plots", "maps")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

world <- ne_countries(scale = "medium", returnclass = "sf")

# ---- 1. Global ES hotspot thumbnail (orange ramp, same as LAC hotspot map) ----

hotspots <- st_read(here("data", "processed", "hotspots", "pct", "global", "hotspots_global_pct.gpkg"), quiet = TRUE)
hotspots_capped <- hotspots %>%
  mutate(
    hotspots_capped = pmin(as.numeric(hotspot_count), 4),
    hotspot_label = factor(hotspots_capped, levels = 1:4, labels = c("1", "2", "3", "4+"))
  )
ramp_orange <- colorRampPalette(c("#FFE0B2", "#F07D00", "#B85C00", "#5C2E00"))(4)
names(ramp_orange) <- c("1", "2", "3", "4+")

p1 <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "white", linewidth = 0.1) +
  geom_sf(data = hotspots_capped, aes(fill = hotspot_label), color = NA) +
  scale_fill_manual(values = ramp_orange, na.value = "gray90", drop = FALSE, guide = "none") +
  coord_sf(expand = FALSE) +
  labs(title = "ES Decline Hotspots", subtitle = "Ecosystem services in simultaneous decline, 1992–2020") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#E8F4F8", color = NA),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 2)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30", margin = margin(b = 4))
  )
ggsave(file.path(out_dir, "global_hotspot_thumbnail.png"), p1, width = 8, height = 4.8, dpi = 200, bg = "white")
message("Saved global_hotspot_thumbnail.png")

# ---- 2. Global critical natural assets thumbnail (green ramp) ----

raster_path <- file.path(data_dir(), "external", "critical_natural_assets", "local_NCP_all_targets",
                          "local_NCP_land_all_targets_md5_7ccece.tif")
r <- rast(raster_path)
r[r <= 2] <- NA
# Aggregate before plotting -- full native 2km global raster is too large for
# a quick thumbnail; factor 8 brings it to ~18km, plenty for a small overview.
# Use mean, not max: max-aggregation lets a single high-priority pixel drag its
# whole 8x8 block to the ceiling, artificially inflating and over-darkening
# the thumbnail relative to the true underlying distribution.
r_agg <- aggregate(r, fact = 8, fun = "mean", na.rm = TRUE)
r_df <- as.data.frame(r_agg, xy = TRUE, na.rm = TRUE)
names(r_df)[3] <- "value"

p2 <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "white", linewidth = 0.1) +
  geom_raster(data = r_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(
    colours = c("#E8F5E9", "#66BB6A", "#007930", "#00331A"),
    values = scales::rescale(c(3, 11.5, 16.5, 20), from = c(3, 20)),
    limits = c(3, 20), guide = "none"
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "Critical Natural Assets", subtitle = "Land required to sustain 90%+ of nature's contributions to people") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#E8F4F8", color = NA),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 2)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30", margin = margin(b = 4))
  )
ggsave(file.path(out_dir, "global_critical_assets_thumbnail.png"), p2, width = 8, height = 4.8, dpi = 200, bg = "white")
message("Saved global_critical_assets_thumbnail.png")
