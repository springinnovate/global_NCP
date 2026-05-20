# R/visualize.R

# Visualize ecosystem service raster
visualize_es <- function(raster, title = "Ecosystem Service Map") {
  ggplot2::ggplot() +
    geom_raster(data = as.data.frame(raster, xy = TRUE), aes(x = x, y = y, fill = value)) +
    scale_fill_viridis_c() +
    labs(title = title, fill = "Value") +
    theme_minimal()
}
