#' Perform Random Sampling and Compute Summary Statistics on a SpatRaster
#'
#' This function performs a stratified random sampling on a `SpatRaster` object, extracts pixel values, 
#' computes summary statistics (mean, confidence intervals, sum), and handles cases where insufficient 
#' non-NA samples are available.
#'
#' @param raster A `SpatRaster` object from the `terra` package. The raster to sample.
#' @param pixels_needed Integer. The number of pixels to sample.
#' @param confidence Numeric. The confidence level for margin of error calculation (default is 0.975 for 95% CI).
#'
#' @return A data frame with summary statistics per raster band, including:
#'   - Mean (`mean`)
#'   - Lower confidence interval (`lower_ci`)
#'   - Upper confidence interval (`upper_ci`)
#'   - Sum of pixel values (`sum`)
#'
#' @import terra
#' @importFrom stats qt mean sd
#' @examples
#' library(terra)
#' r <- rast(ncol=100, nrow=100, nlyrs=3)
#' values(r) <- runif(ncell(r) * nlyr(r), min=0, max=100)
#' sample_and_calculate(raster = r, pixels_needed = 500)
#'
#' @export
sample_and_calculate <- function(raster, pixels_needed, confidence = 0.975) {
  
  # Ensure the input is a SpatRaster
  if (!inherits(raster, "SpatRaster")) {
    stop("Input must be a SpatRaster object.")
  }
  
  # Sample pixels randomly, keeping NA values
  sample_values <- spatSample(
    raster, size = pixels_needed, 
    method = "random", 
    na.rm = FALSE,  # Keep NAs for now to handle resampling
    as.points = FALSE, 
    xy = FALSE, 
    values = TRUE
  )
  
  # Remove rows where all values are NA
  sample_values <- sample_values[rowSums(is.na(sample_values)) != ncol(sample_values), ]
  
  # If not enough samples after removing NAs, resample until reaching the needed size
  if (nrow(sample_values) < pixels_needed) {
    additional_samples <- spatSample(
      raster, size = pixels_needed - nrow(sample_values),
      method = "random", na.rm = TRUE,  # Now enforce NA removal
      as.points = FALSE, xy = FALSE, values = TRUE
    )
    sample_values <- rbind(sample_values, additional_samples)
  }
  
  # Compute summary statistics for each raster band
  band_stats <- apply(sample_values, 2, function(x) {
    mean_val <- mean(x, na.rm = TRUE)
    sd_val <- sd(x, na.rm = TRUE)
    n_val <- sum(!is.na(x))  # Number of non-NA values
    
    # Standard error and margin of error
    se_val <- sd_val / sqrt(n_val)
    margin_error <- qt(confidence, df = n_val - 1) * se_val
    lower_ci <- mean_val - margin_error
    upper_ci <- mean_val + margin_error
    
    return(c(mean = mean_val, lower_ci = lower_ci, upper_ci = upper_ci))
  })
  
  # Convert results into a data frame
  band_stats_df <- as.data.frame(t(band_stats))
  
  # Compute sum of pixel values per band
  band_stats_df$sum <- colSums(sample_values, na.rm = TRUE)
  
  return(band_stats_df)
}
