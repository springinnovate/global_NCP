# ==============================================================================
# LAC Compound Hotspot Map (IDB-WWF workshop deck)
# Regenerates outputs/plots/maps/lac_hotspot_map.png with a title that
# disambiguates "hotspot" (intensity of decline, relative to local baseline)
# from "highest provision" — flagged during deck review 2026-07-09.
# ==============================================================================

library(sf)
library(ggplot2)
library(dplyr)
library(here)
library(rnaturalearth)

source(here("R", "paths.R"))

out_dir <- here("outputs", "plots", "maps")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

gpkg_path <- file.path(data_dir(), "processed", "hotspots", "pct", "region_wb",
                        "hotspots_region_wb_Latin_America_Caribbean_pct.gpkg")

hotspots <- st_read(gpkg_path, quiet = TRUE)

# LAC bounding box (matches the extent shown in the original map: Mexico south to
# Tierra del Fuego, including the Caribbean)
lac_bbox <- st_bbox(c(xmin = -120, xmax = -30, ymin = -58, ymax = 33), crs = st_crs(4326))

world <- ne_countries(scale = "medium", returnclass = "sf")
world_lac <- suppressWarnings(st_crop(world, lac_bbox))

hotspots_capped <- hotspots %>%
  mutate(
    hotspots_capped = pmin(as.numeric(hotspot_count), 4),
    hotspot_label = factor(hotspots_capped, levels = 1:4, labels = c("1", "2", "3", "4+"))
  )

# Single-hue orange ramp anchored on the WWF-brand orange (#F07D00, same as
# make_faceted_maps.R's "damage/decline" color) — colorblind-safer than a
# multi-hue ramp since it varies only in lightness.
ramp <- colorRampPalette(c("#FFE0B2", "#F07D00", "#B85C00", "#5C2E00"))(4)
names(ramp) <- c("1", "2", "3", "4+")

p <- ggplot() +
  geom_sf(data = world_lac, fill = "gray90", color = "white", linewidth = 0.2) +
  geom_sf(data = hotspots_capped, aes(fill = hotspot_label), color = NA) +
  scale_fill_manual(name = "Overlapping\nES hotspots", values = ramp, na.value = "gray90", drop = FALSE) +
  coord_sf(xlim = c(lac_bbox["xmin"], lac_bbox["xmax"]), ylim = c(lac_bbox["ymin"], lac_bbox["ymax"]), expand = FALSE) +
  labs(
    title = "Ecosystem Service Decline Hotspots — Latin America & the Caribbean",
    subtitle = "Ecosystem services in simultaneous decline, 1992–2020"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#E8F4F8", color = NA),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray30", margin = margin(b = 10)),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 11)
  )

out_file <- file.path(out_dir, "lac_hotspot_map.png")
ggsave(out_file, p, width = 10, height = 12, dpi = 300, bg = "white")
message("Saved: ", out_file)
