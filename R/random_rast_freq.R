#' Generate Probability Raster and Summary Statistics
#'
#' This function generates a probability raster and summary statistics for a single or multiband
#' SpatRaster object based on random sampling of target areas.
#'
#' @param raster SpatRaster. Single or multiband raster with ecosystem service values in physical units.
#' @param target_area_ha Numeric. Target area for restoration in hectares.
#' @param iterations Integer. Number of iterations for random sampling.
#' @param conf_level Numeric. Confidence level for calculating confidence intervals (default: 0.95).
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{probability_raster}}{SpatRaster. Probability raster showing the likelihood of each pixel being sampled.}
#'   \item{\code{summary_table}}{Tibble. Summary statistics for each band, including mean, standard deviation, and confidence intervals.}
#' }
#' @examples
#' \dontrun{
#' library(terra)
#' library(dplyr)
#' 
#' # Create example raster
#' r <- rast(nrow = 100, ncol = 100, nlyrs = 3, vals = runif(30000, min = 0, max = 10))
#' names(r) <- c("Nitrogen", "Sediment", "Pollination")
#' 
#' # Generate probability raster and statistics
#' result <- generate_probability_raster(r, target_area_ha = 500, iterations = 30)
#' 
#' # View probability raster
#' plot(result$probability_raster)
#' 
#' # View summary table
#' print(result$summary_table)
#' }
#' @import terra dplyr
#' @export
generate_probability_raster <- function(raster, target_area_ha, iterations, conf_level = 0.95) {
  # Initialize probability raster
  prob_raster <- raster
  values(prob_raster) <- 0
  
  # Store estimates for each layer
  estimates_list <- vector("list", nlyr(raster))
  names(estimates_list) <- names(raster)
  
  for (i in seq_len(iterations)) {
    # Sample pixels for each band
    sampled_raster <- sample_pixels(raster, target_area_ha)
    
    # Update probability raster
    prob_raster <- prob_raster + (!is.na(values(sampled_raster)))
    
    # Calculate total ecosystem service values for each band
    layer_totals <- app(sampled_raster, sum, na.rm = TRUE)
    for (lyr in seq_len(nlyr(layer_totals))) {
      estimates_list[[lyr]] <- c(estimates_list[[lyr]], layer_totals[[lyr]])
    }
  }
  
  # Normalize probability raster
  prob_raster <- prob_raster / iterations
  
  # Calculate summary statistics
  summary_table <- bind_rows(lapply(seq_len(nlyr(raster)), function(lyr) {
    values <- estimates_list[[lyr]]
    mean_val <- mean(values, na.rm = TRUE)
    sd_val <- sd(values, na.rm = TRUE)
    se_val <- sd_val / sqrt(iterations)
    t_critical <- qt(1 - (1 - conf_level) / 2, df = iterations - 1)
    ci_lower <- mean_val - t_critical * se_val
    ci_upper <- mean_val + t_critical * se_val
    tibble(
      Layer = names(raster)[lyr],
      Mean = mean_val,
      SD = sd_val,
      SE = se_val,
      CI_Lower = ci_lower,
      CI_Upper = ci_upper
    )
  }))
  
  # Return results
  return(list(
    probability_raster = prob_raster,
    summary_table = summary_table
  ))
}



