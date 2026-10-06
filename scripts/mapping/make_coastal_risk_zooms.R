# Coastal risk change, 1992-2020: global locator map plus six regional zooms (BC9), drawn from the
# InVEST coastal vulnerability shoreline points (the 10 km grid values are the mean of these points
# per cell). Coastal risk exists only along the shoreline, invisible at global scale in the main
# change maps. Regions: coastlines with the most change (|SPC| >= 1%) per 5-degree box, restricted to densely populated coasts: the four 5-degree boxes with the most people (GHS-POP 2020)
# in cells where coastal risk rose by 1% or more, one per continent (A-D), plus the two populated
# coasts with the most people in cells where it fell (E, F) (chosen 2026-10-05).
#
# Usage: Rscript scripts/mapping/make_coastal_risk_zooms.R [out_png]
#   default: outputs/plots/maps/coastal_risk_change_zooms.png

suppressMessages({library(sf); library(dplyr); library(ggplot2); library(patchwork); library(here)})
source(here("R", "paths.R"))
sf_use_s2(FALSE)

args <- commandArgs(trailingOnly = TRUE)
out_png <- if (length(args)) args[1] else here("outputs", "plots", "maps", "coastal_risk_change_zooms.png")
dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)

orange <- "#F07D00"; teal <- "#009191"
lim <- 14                      # SPC range of coastal risk change is about -14% to +14%
strong <- 1                    # |SPC| >= 1% counts as strong change

zooms <- tibble::tribble(
  ~id, ~name,                              ~xmin, ~xmax, ~ymin, ~ymax,
  "A", "New York and New Jersey, USA",       -76,   -70,  38.5,    42,
  "B", "Nile delta, Egypt",                 28.5,  34.5,    29,  32.5,
  "C", "Istanbul and Sea of Marmara, Turkey", 25.5,  31.5,  39.2,  42.2,
  "D", "Manila Bay, Philippines",          119.5,  122.5,    13,  15.5,
  "E", "Pearl River delta, China",          110.5,  116.5,    20,    24,
  "F", "Gulf of Kutch, India",              67.8,  71.2,    21,  23.8
)

# coastal_risk_tnc_esa1992_2020_ch.gpkg is NOT usable: its 2020 column is out of row order
# relative to 1992 (only ~10% of rows match this file), which makes almost every point "change".
pts_all <- st_read(here("data", "coastal_risk_tnc_esa1992_2020_20251224_013825.gpkg"),
                   query = 'SELECT Rt_1992, Rt_2020, geom FROM "coastal_risk_tnc_esa1992_2020_20251224_013825" WHERE Rt_1992 != Rt_2020',
                   quiet = TRUE) |>
  mutate(spc = (Rt_2020 - Rt_1992) / ((abs(Rt_2020) + abs(Rt_1992)) / 2) * 100,
         spc = pmax(pmin(spc, lim), -lim)) |>
  filter(abs(spc) >= strong) |>
  arrange(abs(spc))
base <- st_read(file.path(data_dir(), "vector_basedata", "cartographic_ee_r264_correspondence.gpkg"), quiet = TRUE) |>
  st_transform(4326)

col_scale <- scale_colour_gradient2(low = teal, mid = "grey80", high = orange, midpoint = 0,
                                    limits = c(-lim, lim), name = "Coastal risk change (SPC, %)")

# --- global locator: strong-change shoreline points, zoom boxes outlined ------------------
boxes <- zooms |> rowwise() |>
  mutate(geometry = st_as_sfc(st_bbox(c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), crs = 4326))) |>
  ungroup() |> st_as_sf()
ee <- "+proj=eqearth"
# labels above each box, except B (Nile delta), placed below so it does not collide with C
labs_sf <- zooms |> mutate(lon = (xmin + xmax) / 2, lat = ifelse(id == "B", ymin - 4, ymax + 4)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |> st_transform(ee)
p_global <- ggplot() +
  geom_sf(data = st_transform(st_crop(base, c(xmin = -180, xmax = 180, ymin = -58, ymax = 84)), ee),
          fill = "grey93", colour = "grey80", linewidth = 0.1) +
  geom_sf(data = st_transform(pts_all, ee), aes(colour = spc), size = 0.25) +
  geom_sf(data = st_transform(boxes, ee), fill = NA, colour = "grey15", linewidth = 0.5) +
  geom_sf_text(data = labs_sf, aes(label = id), fontface = "bold", size = 4) +
  col_scale +
  guides(colour = "none") +
  labs(title = "Change in coastal risk, 1992 to 2020") +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# --- zooms ------------------------------------------------------------------------------------
zoom_panel <- function(z) {
  bb <- c(xmin = z$xmin, xmax = z$xmax, ymin = z$ymin, ymax = z$ymax)
  cz <- st_crop(pts_all, bb)
  n_up <- sum(cz$spc >= strong); n_dn <- sum(cz$spc <= -strong)
  message(sprintf("%s %s: %d points up >= 1%%, %d down", z$id, z$name, n_up, n_dn))
  ggplot() +
    geom_sf(data = st_crop(base, bb), fill = "grey93", colour = "grey60", linewidth = 0.15) +
    geom_sf(data = cz, aes(colour = spc), size = 1.1) +
    col_scale +
    coord_sf(xlim = c(z$xmin, z$xmax), ylim = c(z$ymin, z$ymax), expand = FALSE) +
    labs(title = sprintf("%s  %s", z$id, z$name),
         subtitle = sprintf("%d points rose, %d fell", n_up, n_dn)) +
    theme_minimal(base_size = 9) +
    theme(plot.title = element_text(face = "bold"), axis.text = element_text(size = 7),
          panel.grid = element_line(colour = "grey90", linewidth = 0.2),
          panel.border = element_rect(fill = NA, colour = "grey60"))
}
zp <- lapply(seq_len(nrow(zooms)), function(i) zoom_panel(zooms[i, ]))

fig <- (p_global | ((zp[[1]] | zp[[2]]) / (zp[[3]] | zp[[4]]) / (zp[[5]] | zp[[6]]))) +
  plot_layout(widths = c(1.45, 1), guides = "collect") &
  theme(legend.position = "bottom", legend.key.width = unit(1.5, "cm"))

ggsave(out_png, fig, width = 16, height = 9.5, dpi = 300, bg = "white")
message("wrote ", out_png)
