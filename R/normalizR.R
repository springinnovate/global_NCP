#' Normalize a Raster
#'
#' This function normalizes a raster by rescaling its values to the range [0, 1].
#'
#' @param raster A `SpatRaster` object to be normalized.
#' @return A normalized `SpatRaster` object.
#' @examples
#' r <- terra::rast(matrix(1:100, ncol = 10))
#' normalize_raster(r)
normalize_raster <- function(raster) {
  (raster - terra::minValue(raster)) / (terra::maxValue(raster) - terra::minValue(raster))
}