#' Filter Observations by Percentile Range within Groups
#'
#' Filters a data frame to retain only the top, bottom, or both extremes of a numeric variable
#' within groups (e.g., by ecosystem service). This is useful for identifying spatial units
#' or observations that show extreme values of change or impact across services.
#'
#' @param df A `data.frame` or `tibble` containing the data.
#' @param var A character string specifying the name of the numeric column to filter on (e.g., `"pct_ch"`).
#' @param group_var A character string specifying the grouping variable (e.g., `"service"`).
#' @param filter_type A character string indicating whether to filter the `"top"`, `"bottom"`, or `"both"` ends.
#' @param percentile A numeric value between 0 and 1 indicating the proportion of extreme values to keep
#'        (e.g., `0.10` keeps the top and/or bottom 10%). Default is `0.10`.
#'
#' @return A filtered `tibble` containing only the observations within the specified percentile range.
#' @export
#'
#' @examples
#' # Filter top and bottom 10% of pct_ch within each service
#' df_top_bottom <- filter_by_percentile(df, var = "pct_ch", group_var = "service",
#'                                       filter_type = "both", percentile = 0.10)
#'
#' # Filter only the top 5% of pct_ch
#' df_top <- filter_by_percentile(df, filter_type = "top", percentile = 0.05)
#'
#' # Filter only the bottom 1% of pct_ch
#' df_bottom <- filter_by_percentile(df, filter_type = "bottom", percentile = 0.01)
filter_by_percentile <- function(df, var = "pct_ch", group_var = "service",
                                 filter_type = c("both", "top", "bottom"),
                                 percentile = 0.10) {
  
  filter_type <- match.arg(filter_type)
  
  df_filtered <- df %>%
    group_by(.data[[group_var]]) %>%
    filter({
      v <- .data[[var]]
      q_low <- quantile(v, percentile, na.rm = TRUE)
      q_high <- quantile(v, 1 - percentile, na.rm = TRUE)
      
      if (filter_type == "both") {
        v <= q_low | v >= q_high
      } else if (filter_type == "top") {
        v >= q_high
      } else {
        v <= q_low
      }
    }) %>%
    ungroup()
  
  return(df_filtered)
}
