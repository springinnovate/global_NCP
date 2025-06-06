library(terra)

#' Exaggerate raster for display by aggregating pixel size
#'
#' @param r A SpatRaster object
#' @param factor Aggregation factor (e.g., 10 means 10x larger pixels)
#' @param fun Aggregation function: "mean", "sum", or "max"
#'
#' @return Aggregated SpatRaster for display
resamp_disp<- function(r, factor = 10, fun = "mean") {
  if (!inherits(r, "SpatRaster")) stop("Input must be a SpatRaster")
  r_agg <- aggregate(r, fact = factor, fun = match.fun(fun), na.rm = TRUE)
  return(r_agg)
}
