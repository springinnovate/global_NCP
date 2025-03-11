#' Create a Mask for No-Change or Change Across Multiple spatRaster Layers or Objects
#'
#' This function takes a multiband spatRaster or multiple spatRaster objects 
#' and produces a mask indicating pixels that remained the same or changed 
#' across all layers/objects.
#'
#' @param ... Multiple spatRaster objects or a single multiband spatRaster.
#' @param type A character string, either "nochange" to create a no-change mask
#'             or "change" to create a change mask. Default is "nochange".
#' @return A spatRaster object with 1 indicating no-change or change pixels
#'         and NA for the rest.
#' @export
#' @examples
#' # Given r1, r2, and r3 are spatRaster objects
#' # Create a no-change mask:
#' mask <- no_chmsk(r1, r2, r3, type = "nochange")
#' # Create a change mask:
#' mask <- no_chmsk(r1, r2, r3, type = "change")
#'
#' # Given multi_band_raster is a multiband spatRaster
#' # Create a no-change mask:
#' mask <- no_chmsk(multi_band_raster, type = "nochange")
no_chmsk <- function(..., type = "nochange") {
  inputs <- list(...)
  
  # If a single multiband spatRaster is provided
  if (length(inputs) == 1 && terra::nlyr(inputs[[1]]) > 1) {
    rasters <- lapply(1:terra::nlyr(inputs[[1]]), function(i) terra::subset(inputs[[1]], i))
  } else {
    rasters <- inputs
  }
  
  # Check if all objects are of type spatRaster
  # if(!all(sapply(rasters, function(x) class(x)[1] == "spatRaster"))) {
  #   stop("All input objects must be of type spatRaster")
  # }
  
  # Calculate mask
  if (type == "nochange") {
    mask <- Reduce(`==`, rasters)
  } else if (type == "change") {
    mask <- !Reduce(`==`, rasters)
  } else {
    stop('Invalid type. Choose either "nochange" or "change".')
  }
  
  # Convert logical mask to spatRaster with values 1 and NA
  result_rast <- rasters[[1]] * 0
  result_rast[mask] <- 1
  result_rast[!mask] <- NA
  
  return(result_rast)
}

