# ==============================================================================
# Colombia Critical Natural Assets Map — "opportunity" layer for the CLEC/Sandra
# report, paired with the hotspots-of-change ("risk") maps.
# ==============================================================================
# Same source/logic as make_lac_critical_assets_map.R (Chaplin-Kramer et al.
# 2022 12-NCP national-scale optimization), cropped to Colombia specifically
# instead of the full LAC extent. Deliberately placed BEFORE the hotspot-of-
# change section in the report/deck -- "opportunity leads, risk follows," same
# ordering used in the IADB workshop deck.
# ==============================================================================

library(sf)
library(terra)
library(ggplot2)
library(dplyr)
library(here)

source(here("R", "paths.R"))

out_dir <- here("outputs", "plots", "colombia_report")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

wwf_dark_green <- "#004D1E"

raster_path <- file.path(data_dir(), "external", "critical_natural_assets", "local_NCP_all_targets",
                          "local_NCP_land_all_targets_md5_7ccece.tif")
stopifnot(file.exists(raster_path))

# Colombia bbox from the change grid (same source used for the hotspot/change maps)
grid_col <- st_read(here("data", "processed", "10k_change_calc.gpkg"), quiet = TRUE) %>%
  filter(nev_name == "Colombia")
colombia_boundary <- st_union(grid_col)
col_bbox <- st_bbox(grid_col)

r <- rast(raster_path)
r_col <- crop(r, ext(col_bbox["xmin"], col_bbox["xmax"], col_bbox["ymin"], col_bbox["ymax"]))
r_col[r_col <= 2] <- NA  # keep only critical natural assets (value > 2, the paper's 90% threshold)

r_df <- as.data.frame(r_col, xy = TRUE, na.rm = TRUE)
names(r_df)[3] <- "value"

p <- ggplot() +
  geom_sf(data = colombia_boundary, fill = "gray97", color = "gray70", linewidth = 0.3) +
  geom_raster(data = r_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(
    name = "Rango de prioridad\n(activo natural crítico,\numbral >90%)",
    colours = c("#E8F5E9", "#66BB6A", "#007930", "#00331A"),
    values = scales::rescale(c(3, 11.5, 16.5, 20), from = c(3, 20)),
    na.value = NA,
    limits = c(3, 20)
  ) +
  coord_sf(xlim = c(col_bbox["xmin"], col_bbox["xmax"]), ylim = c(col_bbox["ymin"], col_bbox["ymax"]), expand = FALSE) +
  labs(
    title = "Colombia — Activos Naturales Críticos",
    subtitle = "Área de tierra requerida para sostener el 90%+ de las contribuciones de la naturaleza a las personas\n(Chaplin-Kramer et al. 2022)"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5, color = wwf_dark_green),
    plot.subtitle = element_text(size = 9.5, hjust = 0.5, color = "gray30"),
    legend.position = "right",
    legend.title = element_text(size = 9.5, face = "bold"),
    legend.text = element_text(size = 8)
  )

ggsave(file.path(out_dir, "colombia_critical_assets_map.png"), p, width = 9.5, height = 10, dpi = 200, bg = "white")
message("Saved: ", file.path(out_dir, "colombia_critical_assets_map.png"))
message("Done.")
