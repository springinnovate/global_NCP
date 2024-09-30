extract_summary <- function(raster, polygon, polygon_attribute, fun = fun) {
  # Extract the raster values within the polygon
  values_within_polygon <- extract(raster, polygon, fun = mean, na.rm = TRUE) # Example: using mean
  # Add the attribute from the polygon and the raster name
  result <- data.frame(
    polygon_attribute = polygon[[polygon_attribute]],  # Get the specific attribute of the polygon
    raster_name = names(raster),  # Get the name of the SpatRaster
    mean_value = values_within_polygon[, 2]  # Extracted mean value
  )
  return(result)
}
