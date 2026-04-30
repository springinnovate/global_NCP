# generate_overview_map.R
# Run this locally in your R console to generate the "First Look" transition maps

library(sf)
library(ggplot2)
library(rnaturalearth)

# Define local paths based on your downloaded archive structure
gpkg_pct <- "home/jeronimo/data/global_ncp/processed/hotspots/pct/global/hotspots_global_pct.gpkg"
gpkg_abs <- "home/jeronimo/data/global_ncp/processed/hotspots/abs/global/hotspots_global_abs.gpkg"

out_dir <- "outputs/plots"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Load world background
world <- ne_countries(scale = "medium", returnclass = "sf")

create_first_look_map <- function(gpkg_path, out_file) {
  message("Reading ", gpkg_path, "...")
  hotspots <- st_read(gpkg_path, quiet = TRUE)
  
  p <- ggplot() +
    geom_sf(data = world, fill = "gray90", color = "white", linewidth = 0.2) +
    geom_sf(data = hotspots, fill = "#E83737", color = NA) + # Solid canonical red, no heatmap
    coord_sf(crs = "+proj=eqearth") + # Equal Earth projection
    theme_void() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  ggsave(out_file, p, width = 16, height = 9, dpi = 300, bg = "white")
  message("Saved full-slide map to ", out_file)
}

create_first_look_map(gpkg_pct, file.path(out_dir, "first_look_map_pct.png"))
create_first_look_map(gpkg_abs, file.path(out_dir, "first_look_map_abs.png"))