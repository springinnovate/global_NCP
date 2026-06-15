#' Create a Mask for No-Change or Change Across Multiple SpatRaster Layers
#'
#' This function identifies pixels that remain the same (or change) across all 
#' provided SpatRaster layers. It is optimized to use terra's internal algebra 
#' (min/max) for speed.
#'
#' @param ... SpatRaster objects. Can be provided as individual arguments 
#'            (e.g., `r1, r2`), as a list of SpatRasters, or as a single 
#'            multiband SpatRaster.
#' @param type Character. "nochange" (default) to mask pixels that have the 
#'             same value across all layers. "change" to mask pixels that 
#'             have at least one different value across layers.
#' @return A SpatRaster with value 1 for matching pixels and NA otherwise.
#' @export
#' @examples
#' # mask <- no_chmsk(r1, r2, type = "nochange")
no_chmsk <- function(..., type = "nochange") {
  inputs <- list(...)
  
  # Combine inputs into a single SpatRaster stack
  # terra::rast handles lists of rasters or single multiband rasters efficiently
  r_stack <- tryCatch(terra::rast(inputs), error = function(e) {
    stop("Could not create a raster stack from inputs. Ensure all inputs are SpatRaster objects.")
  })
  
  # Optimization: Use range (min/max) to detect change across layers
  # If min == max, values are identical across all layers.
  r_min <- terra::min(r_stack)
  r_max <- terra::max(r_stack)
  
  if (type == "nochange") {
    mask_logic <- (r_min == r_max)
  } else if (type == "change") {
    mask_logic <- (r_min != r_max)
  } else {
    stop('Invalid type. Choose either "nochange" or "change".')
  }
  
  # Convert logical (TRUE/FALSE) to 1/NA
  # We multiply by 1 to get 0/1, then set 0 to NA
  result_rast <- mask_logic * 1
  result_rast[result_rast == 0] <- NA
  
  return(result_rast)
}
