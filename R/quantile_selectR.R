#' Select Top Percentile of Raster Values
#'
#' This function selects the top percentile of raster values, retaining only pixels 
#' with values greater than or equal to the given percentile threshold. It works for 
#' both single-band and multi-band `SpatRaster` objects.
#'
#' @param raster A `SpatRaster` object from the `terra` package.
#' @param percentile A numeric value between 0 and 1 indicating the percentile threshold. 
#'        For example, `0.9` retains the top 10% of values.
#' @return A `SpatRaster` object with the same structure as the input raster. For each band,
#'         only the pixels above the specified percentile threshold are retained; others are set to NA.
#' @import terra
#' @examples
#' library(terra)
#' r <- rast(ncol=10, nrow=10)
#' values(r) <- runif(ncell(r), min=0, max=100)
#' top_10 <- select_top_percentile(r, percentile=0.9)
#'
#' # For multi-band rasters
#' multi_r <- c(r, r * 2)
#' top_multi <- select_top_percentile(multi_r, percentile=0.8)
#' 
#' @export
select_top_percentile <- function(raster, percentile = 0.9) {
  # Ensure the input is a SpatRaster
  if (!inherits(raster, "SpatRaster")) {
    stop("Input must be a SpatRaster object.")
  }
  
  # Check if the percentile value is valid
  if (!is.numeric(percentile) || percentile <= 0 || percentile > 1) {
    stop("Percentile must be a numeric value between 0 and 1.")
  }
  
  # Apply the threshold calculation band by band
  result <- app(raster, function(band) {
    threshold <- quantile(band, probs = percentile, na.rm = TRUE)  # Get the threshold
    ifelse(band >= threshold, band, NA)  # Retain only top values
  })
  
  return(result)
}