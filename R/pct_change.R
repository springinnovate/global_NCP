#' Calculate Symmetric Percentage Change
#'
#' Calculates the percentage change between two values using the symmetric method.
#' This method is robust to zero baselines and is bounded between -200% and +200%.
#'
#' Formula: (V2 - V1) / (0.5 * (V1 + V2)) * 100
#'
#' @param v1 Numeric vector (baseline/start)
#' @param v2 Numeric vector (target/end)
#' @return Numeric vector of percentage change
#' @export
calc_symmetric_pct_change <- function(v1, v2) {
  diff <- v2 - v1
  # Use average of magnitudes for denominator
  denom <- 0.5 * (abs(v1) + abs(v2))

  # Handle cases where both are zero (change is 0)
  pct <- ifelse(denom == 0, 0, (diff / denom) * 100)

  return(pct)
}