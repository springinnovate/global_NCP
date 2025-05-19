#' Filter and prepare ecosystem service-beneficiary data for plotting
#'
#' @param df A data frame with ecosystem service and beneficiary data.
#' @param area_col Name of the column with basin area (e.g., "SUB_AREA").
#' @param filter_type Type of filtering: "quantile", "percent", or "absolute".
#' @param filter_cutoff Numeric threshold used for filtering.
#' @param filter_direction Which rows to keep: "top", "bottom", "both", "tail", "middle", or "none".
#' @param service_col Name of the service column (default: "service").
#' @param service_levels Optional factor levels to enforce for service ordering.
#' @param metric_labels A named character vector or tibble with columns `lc_metrics`, `names`.
#'
#' @return A list with filtered `df`, label tibble `nam`, and string `filter_note`.
#' @export
filter_and_prepare_df <- function(df,
                                  area_col = "SUB_AREA",
                                  filter_type = "percent",
                                  filter_cutoff = 0.05,
                                  filter_direction = "tail",
                                  service_col = "service",
                                  service_levels = NULL,
                                  metric_labels = NULL) {
  stopifnot(area_col %in% names(df))
  df_raw <- df
  area_vals <- df[[area_col]]
  
  # Compute filtering threshold
  if (filter_type == "quantile") {
    q_val <- quantile(area_vals, probs = c(filter_cutoff, 1 - filter_cutoff), na.rm = TRUE)
  } else if (filter_type == "percent") {
    sorted_areas <- sort(area_vals)
    n_total <- length(sorted_areas)
    n_cut <- ceiling(n_total * filter_cutoff)
    q_val <- c(sorted_areas[n_cut], sorted_areas[n_total - n_cut])
  } else if (filter_type == "absolute") {
    q_val <- c(filter_cutoff, max(area_vals, na.rm = TRUE))
  } else {
    stop("Invalid filter_type: must be 'quantile', 'percent', or 'absolute'")
  }
  
  # Filter
  if (filter_direction == "top") {
    df <- df %>% filter(.data[[area_col]] >= q_val[2])
    filter_note <- paste0("Top ", round(100 * filter_cutoff), "% largest basins")
  } else if (filter_direction == "bottom") {
    df <- df %>% filter(.data[[area_col]] <= q_val[1])
    filter_note <- paste0("Bottom ", round(100 * filter_cutoff), "% smallest basins")
  } else if (filter_direction == "both" || filter_direction == "tail") {
    df <- df %>% filter(.data[[area_col]] <= q_val[1] | .data[[area_col]] >= q_val[2])
    filter_note <- paste0("Extreme tails (", round(100 * filter_cutoff), "%)")
  } else if (filter_direction == "middle") {
    df <- df %>% filter(.data[[area_col]] > q_val[1] & .data[[area_col]] < q_val[2])
    filter_note <- paste0("Middle ", round(100 * (1 - 2 * filter_cutoff)), "% of basins")
  } else {
    filter_note <- "No basin size filtering applied"
  }
  
  # Order services
  if (!is.null(service_levels)) {
    df[[service_col]] <- factor(df[[service_col]], levels = service_levels)
  }
  
  # Return list with filtered df, labels, and subtitle tag
  result <- list(
    df = df,
    nam = metric_labels,
    filter_note = filter_note
  )
  return(result)
}
