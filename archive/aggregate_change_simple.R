#' Normalize raster data
#'
#' Linearly scales raster values to a 0-1 range based on its minimum and maximum values.
#'
#' @param raster A `SpatRaster` object to be normalized.
#'
#' @return A normalized `SpatRaster` object with values between 0 and 1.
#' @export
normalize_raster <- function(raster) {
  (raster - terra::minValue(raster)) / (terra::maxValue(raster) - terra::minValue(raster))
}

#' Aggregate ecosystem services
#'
#' Masks a list of service rasters to an intervention area, normalizes them,
#' and then sums them to produce an aggregated index.
#'
#' @param rasters A list of `SpatRaster` objects representing different ecosystem services.
#' @param intervention A `SpatVector` or `SpatRaster` used as a mask for the intervention area.
#'
#' @return A `SpatRaster` representing the sum of normalized, masked services.
#' @export
aggregate_services <- function(rasters, intervention) {
  masked <- lapply(rasters, terra::mask, mask = intervention)
  normalized <- lapply(masked, normalize_raster)
  aggregated <- Reduce("+", normalized)
  terra::mask(aggregated, intervention)
}
