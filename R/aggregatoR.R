# R/model.R

# Normalize raster data
normalize_raster <- function(raster) {
  (raster - terra::minValue(raster)) / (terra::maxValue(raster) - terra::minValue(raster))
}

# Aggregate ecosystem services
aggregate_services <- function(rasters, intervention) {
  masked <- lapply(rasters, terra::mask, mask = intervention)
  normalized <- lapply(masked, normalize_raster)
  aggregated <- Reduce("+", normalized)
  terra::mask(aggregated, intervention)
}

