#' Align a List of SpatRaster Objects to a Template Raster
#'
#' This function iterates over a list of `SpatRaster` objects, checks if each raster
#' is aligned with a given template raster (in terms of extent, resolution, projection),
#' and if not, resamples it to match the template.
#'
#' @param raster_list A list of `SpatRaster` objects to be checked and aligned.
#' @param template A `SpatRaster` object used as the reference for alignment.
#' @param resample_method Character. The resampling method to use if alignment is needed.
#'        Options are `"bilinear"` (default), `"near"` (nearest neighbor), `"cubic"`, `"cubicspline"`, etc.
#'
#' @return A list of `SpatRaster` objects, where each raster is either unchanged (if already aligned)
#'         or resampled to match the template.
#'
#' @details The function checks alignment using `terra::compareGeom()`, which verifies if the
#'          extent, resolution, and coordinate reference system (CRS) match between the raster
#'          and the template. If differences are detected, `terra::resample()` is used to align the raster.
#'
#' @import terra
#' @importFrom terra compareGeom resample
#'
#' @examples
#' library(terra)
#' r1 <- rast(ncol=10, nrow=10)
#' r2 <- rast(ncol=12, nrow=12)  # Different resolution
#' template <- rast(ncol=10, nrow=10)
#'
#' rasters <- list(r1, r2)
#' aligned_rasters <- align_rasters(rasters, template, resample_method = "bilinear")
#'
#' @export
align_rasters <- function(raster_list, template, resample_method = "bilinear") {
  if (!inherits(template, "SpatRaster")) {
    stop("The template must be a SpatRaster object.")
  }
  
  if (!all(sapply(raster_list, inherits, "SpatRaster"))) {
    stop("All elements in raster_list must be SpatRaster objects.")
  }
  
  # Iterate and align rasters if needed
  aligned_list <- lapply(raster_list, function(r) {
    if (!compareGeom(r, template, stopOnError = FALSE)) {
      message("Aligning raster: ", names(r))
      return(resample(r, template, method = resample_method))
    } else {
      message("Raster already aligned: ", names(r))
      return(r)
    }
  })
  
  return(aligned_list)
}
