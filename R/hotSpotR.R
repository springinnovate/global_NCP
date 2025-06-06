
#' Extract Hotspots of Change for Ecosystem Services
#'
#' This function identifies spatial units (e.g., basins) with the most extreme changes 
#' in ecosystem service provision based on percentiles or absolute counts. It returns both a 
#' long-format dataframe with hotspot flags per service and a summary showing for each unit 
#' how many services it is a hotspot for.
#'
#' @param df A data.frame or tibble. Must include columns: `service`, `HYBAS_ID`, and `pct_ch`.
#' @param threshold Numeric. Either a percentile (if `use_percentile = TRUE`) or an integer count of top/bottom values.
#' @param use_percentile Logical. If TRUE (default), `threshold` is treated as a percentile (e.g., 0.1 for 10%).
#' @param direction Character. One of "both" (default), "top", or "bottom".
#'
#' @return A list with two tibbles:
#'   - `df_hotspots`: long-format tibble with an added column `hotspot_flag` per service and record.
#'   - `hotspot_summary`: summarised tibble with a `hotspot_binary` column and a cleaned list of hotspot services per unit.
#'
#' @examples
#' result <- extract_hotspots(df, threshold = 0.1)
#' result$df_hotspots
#' result$hotspot_summary
#'
#' @export
extract_hotspots <- function(df, threshold = 0.1, use_percentile = TRUE, direction = "both") {
  stopifnot("service" %in% names(df), "pct_ch" %in% names(df), "HYBAS_ID" %in% names(df))
  
  # Helper to assign flag by service
  flag_service <- function(df_s) {
    if (use_percentile) {
      if (direction == "both") {
        df_s <- df_s %>%
          mutate(hotspot_flag = case_when(
            pct_ch >= quantile(pct_ch, 1 - threshold, na.rm = TRUE) ~ "top",
            pct_ch <= quantile(pct_ch, threshold, na.rm = TRUE) ~ "bottom",
            TRUE ~ NA_character_
          ))
      } else if (direction == "top") {
        df_s <- df_s %>%
          mutate(hotspot_flag = ifelse(pct_ch >= quantile(pct_ch, 1 - threshold, na.rm = TRUE), "top", NA))
      } else if (direction == "bottom") {
        df_s <- df_s %>%
          mutate(hotspot_flag = ifelse(pct_ch <= quantile(pct_ch, threshold, na.rm = TRUE), "bottom", NA))
      }
    } else {
      n <- threshold
      if (direction == "both") {
        df_s <- bind_rows(
          df_s %>% slice_max(pct_ch, n = n, with_ties = FALSE) %>% mutate(hotspot_flag = "top"),
          df_s %>% slice_min(pct_ch, n = n, with_ties = FALSE) %>% mutate(hotspot_flag = "bottom")
        )
      } else if (direction == "top") {
        df_s <- df_s %>% slice_max(pct_ch, n = n, with_ties = FALSE) %>% mutate(hotspot_flag = "top")
      } else if (direction == "bottom") {
        df_s <- df_s %>% slice_min(pct_ch, n = n, with_ties = FALSE) %>% mutate(hotspot_flag = "bottom")
      }
    }
    return(df_s)
  }
  
  df_hotspots <- df %>%
    group_by(service) %>%
    group_modify(~ flag_service(.x)) %>%
    ungroup() %>%
    mutate(hotspot_binary = ifelse(!is.na(hotspot_flag), 1, 0))
  
  hotspot_summary <- df_hotspots %>%
    filter(hotspot_binary == 1) %>%
    group_by(HYBAS_ID) %>%
    summarise(
      hotspot_count = n(),
      hotspot_services = list(unique(service)),
      .groups = "drop"
    ) %>%
    mutate(
      hotspot_services = lapply(hotspot_services, \(x) trimws(as.character(x))),
      hotspot_services = sapply(hotspot_services, \(x) paste(x, collapse = ", "))
    )
  
  return(list(
    df_hotspots = df_hotspots,
    hotspot_summary = hotspot_summary
  ))
}
