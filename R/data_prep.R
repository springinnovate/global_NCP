# data_prep.R
library(terra)  # Raster data handling
library(sf)     # Vector data handling
library(dplyr)  # Data manipulation
library(yaml)   # Configuration handling

prepare_data <- function(config_path) {
  config <- yaml::read_yaml(config_path)
  intervention <- sf::st_read(config$intervention_area)
  rasters <- lapply(config$services, terra::rast)
  list(intervention = intervention, rasters = rasters, config = config)
}
