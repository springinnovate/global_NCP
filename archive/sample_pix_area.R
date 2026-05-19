#' Sample Pixels from a Raster with Constraints
#'
#' This function samples a specified number of pixels from a raster while ensuring
#' constraints like a minimum distance from raster edges or masked areas.
#'
#' @param raster A \code{SpatRaster} object. The raster from which pixels will be sampled.
#' @param target_area_ha Numeric. The target area in hectares to be sampled, converted to pixel count.
#' @param mask A \code{SpatRaster} object (optional). A mask defining valid sampling regions (1 for valid, NA for invalid).
#' @param min_distance Numeric (optional). Minimum distance (in pixels) from raster edges or NA areas.
#' @return A \code{SpatRaster} object with the sampled pixels.
#' @examples
#' # Example usage:
#' library(terra)
#' r <- rast(nrows=100, ncols=100, res=30, vals=runif(10000, 0, 1))
#' mask <- r
#' values(mask) <- ifelse(runif(ncell(mask)) > 0.5, 1, NA)
#' sample <- sample_pixels(raster = r, target_area_ha = 50, mask = mask, min_distance = 5)
#' plot(sample)
#' @export
sample_pixels <- function(raster, target_area_ha, mask = NULL, min_distance = NULL) {
  res <- res(raster)
  pixel_area <- res[1] * res[2] / 10000
  target_pixel_count <- round(target_area_ha / pixel_area)
  
  # Apply mask if provided
  valid_cells <- which(!is.na(values(raster)))
  if (!is.null(mask)) {
    valid_cells <- intersect(valid_cells, which(!is.na(values(mask))))
  }
  
  # Exclude pixels near edges or NA areas
  if (!is.null(min_distance)) {
    edge_buffer <- buffer(raster, width = -min_distance * max(res))
    valid_cells <- intersect(valid_cells, which(!is.na(values(edge_buffer))))
  }
  
  # Ensure sampling doesn't exceed available pixels
  if (target_pixel_count > length(valid_cells)) {
    warning("Target area exceeds available valid pixels. Sampling all available pixels.")
    target_pixel_count <- length(valid_cells)
  }
  
  # Sample pixels
  sampled_cells <- sample(valid_cells, target_pixel_count, replace = FALSE)
  
  # Return sampled raster
  sampled_raster <- raster
  values(sampled_raster) <- NA
  values(sampled_raster)[sampled_cells] <- values(raster)[sampled_cells]
  
  return(sampled_raster)
}


