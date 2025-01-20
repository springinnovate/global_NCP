select_top_10_percent <- function(raster) {
  # Get all values from the raster, ignoring NA
  raster_values <- values(raster, na.rm = TRUE)
  
  # Calculate the threshold for the top 10%
  threshold <- quantile(raster_values, probs = 0.9, na.rm = TRUE)
  
  # Retain only the top 10% pixels
  result <- ifelse(values(raster) >= threshold, values(raster), NA)
  
  # Return the reclassified raster
  raster_out <- raster
  values(raster_out) <- result
  return(raster_out)
}

# Apply the function to each layer