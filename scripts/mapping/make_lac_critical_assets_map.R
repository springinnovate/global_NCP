# ==============================================================================
# LAC Critical Natural Assets Map (IDB-WWF workshop deck, "opportunities" slide)
# ==============================================================================
# Source data: Chaplin-Kramer et al. 2022, "Mapping the planet's critical
# natural assets," Nature Ecology & Evolution. https://doi.org/10.1038/s41559-022-01934-5
# Data: https://osf.io/whrq6/ -- local_NCP_all_targets.zip, land raster
# (12 local-scale NCPs, national-scale optimization; matches the paper's Fig. 1).
#
# Value meaning (per the OSF project wiki): each cell's value (0-20) is the
# minimum cumulative land-area target (in 5% steps, 20 = the most restrictive
# 5% target, 1 = only needed at the 100% target) at which the optimization
# selects that cell for at least one of the 12 local NCPs. The paper defines
# "critical natural assets" at the 90% target, i.e. value > 2.
#
# Deliberately mirrors make_lac_hotspot_map.R's structure/style (same basemap,
# extent, title layout) with a WWF-green ramp instead of orange, and a
# continuous colorbar legend (not discrete bins) since the underlying variable
# is an 18-level ordinal rank, not a small-integer overlap count like
# hotspot_count -- forcing that into 4 discrete bins would misrepresent it.

library(sf)
library(terra)
library(ggplot2)
library(here)
library(rnaturalearth)

source(here("R", "paths.R"))

out_dir <- here("outputs", "plots", "maps")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

raster_path <- file.path(data_dir(), "external", "critical_natural_assets", "local_NCP_all_targets",
                          "local_NCP_land_all_targets_md5_7ccece.tif")
stopifnot(file.exists(raster_path))

lac_bbox <- c(xmin = -120, xmax = -30, ymin = -58, ymax = 33)

r <- rast(raster_path)
r_lac <- crop(r, ext(lac_bbox["xmin"], lac_bbox["xmax"], lac_bbox["ymin"], lac_bbox["ymax"]))
r_lac[r_lac <= 2] <- NA  # keep only critical natural assets (value > 2)

# Convert to a plain data frame for geom_raster -- avoids needing tidyterra
# (which requires a newer dplyr than is safely upgradable in this environment
# without risking a locked/in-use R session).
r_df <- as.data.frame(r_lac, xy = TRUE, na.rm = TRUE)
names(r_df)[3] <- "value"

world <- ne_countries(scale = "medium", returnclass = "sf")
world_lac <- suppressWarnings(st_crop(world, st_bbox(lac_bbox)))

# WWF-green sequential ramp, same construction as the hotspot map's orange ramp
# (light tint -> WWF green #007930 -> a dark anchor), continuous this time.

p <- ggplot() +
  geom_sf(data = world_lac, fill = "gray90", color = "white", linewidth = 0.2) +
  geom_raster(data = r_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(
    name = "Priority rank\n(critical natural\nassets, >90% target)",
    colours = c("#E8F5E9", "#66BB6A", "#007930", "#00331A"),
    # Anchors pushed toward the upper tail: the darkest green only kicks in
    # for the top ~20% of values. Data is skewed high (LAC median value = 8,
    # 75th pct = 12, out of the 3-20 critical range) so a linear 0/.33/.67/1
    # placement over-saturated most of the map to a uniform dark green.
    values = scales::rescale(c(3, 11.5, 16.5, 20), from = c(3, 20)),
    na.value = NA,
    limits = c(3, 20)
  ) +
  coord_sf(xlim = c(lac_bbox["xmin"], lac_bbox["xmax"]), ylim = c(lac_bbox["ymin"], lac_bbox["ymax"]), expand = FALSE) +
  labs(
    title = "Critical Natural Assets — Latin America & the Caribbean",
    subtitle = "Land required to sustain 90%+ of nature's contributions to people"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#E8F4F8", color = NA),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray30", margin = margin(b = 10)),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.8, "cm")
  )

out_file <- file.path(out_dir, "lac_critical_assets_map.png")
ggsave(out_file, p, width = 10, height = 12, dpi = 300, bg = "white")
message("Saved: ", out_file)
