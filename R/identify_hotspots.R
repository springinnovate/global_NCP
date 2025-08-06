#' Identify and summarize spatial hotspots based on service change thresholds
#'
#' @param df A long-format dataframe with one row per service per spatial unit (e.g., from `plt_long`)
#' @param fid_col Column name for feature ID (default: "fid")
#' @param c_fid_col Column name for cluster or region ID (default: "c_fid")
#' @param service_col Column name for the service name (default: "service")
#' @param value_col Column name for the value to rank on (default: "pct_chg")
#' @param combos A named list of character vectors, each specifying a group of services to tally per combo
#' @param loss_services Character vector of services where low values (losses) are undesirable
#' @param gain_services Character vector of services where high values (gains or damages) are undesirable
#' @param pct_cutoff Proportion cutoff (e.g., 0.05 for top/bottom 5%)
#' @param type Type of hotspot to return: "negative" (default), "positive", or "both"
#' @param sf_data Optional `sf` object with geometries to join (must contain `fid`)
#'
#' @return An `sf` object (if `sf_data` provided) or `data.frame` with:
#'   - hotspot count and services per feature
#'   - binary matrix of services
#'   - combo counts
#'   - hotspot type classification
#'
#' @export
identify_hotspots <- function(df,
                              fid_col = "fid",
                              c_fid_col = "c_fid",
                              service_col = "service",
                              value_col = "pct_chg",
                              combos = list(),
                              loss_services = character(),
                              gain_services = character(),
                              pct_cutoff = 0.05,
                              type = c("negative", "positive", "both"),
                              sf_data = NULL) {
  type <- match.arg(type)
  
  # ---------------------------------------------
  # Step 1: Rank and identify top/bottom hotspots
  # ---------------------------------------------
  df_hotspots <- df %>%
    group_by(.data[[service_col]]) %>%
    mutate(
      n_total = n(),
      n_cut = ceiling(n_total * pct_cutoff),
      rank_high = rank(-.data[[value_col]], ties.method = "first"),
      rank_low = rank(.data[[value_col]], ties.method = "first"),
      hotspot_flag = case_when(
        rank_high <= n_cut ~ "high",
        rank_low <= n_cut ~ "low",
        TRUE ~ NA_character_
      ),
      hotspot_binary = !is.na(hotspot_flag)
    ) %>%
    ungroup()
  
  # ---------------------------------------------
  # Step 2: Filter based on type
  # ---------------------------------------------
  df_hotspots_filtered <- df_hotspots %>%
    filter(
      hotspot_binary,
      (
        (type == "negative" &
           ((.data[[service_col]] %in% loss_services & hotspot_flag == "low") |
              (.data[[service_col]] %in% gain_services & hotspot_flag == "high"))) |
          
          (type == "positive" &
             ((.data[[service_col]] %in% loss_services & hotspot_flag == "high") |
                (.data[[service_col]] %in% gain_services & hotspot_flag == "low"))) |
          
          (type == "both")
      )
    )
  
  # ---------------------------------------------
  # Step 3: Summarize by feature ID
  # ---------------------------------------------
  hotspot_summary <- df_hotspots_filtered %>%
    group_by(.data[[fid_col]]) %>%
    summarise(
      !!c_fid_col := first(.data[[c_fid_col]]),
      hotspot_count = n(),
      hotspot_services_list = list(unique(.data[[service_col]])),
      .groups = "drop"
    ) %>%
    mutate(
      hotspot_services = sapply(hotspot_services_list, \(x) paste(trimws(as.character(x)), collapse = ", ")), 
      
      # Combo counts
      !!!setNames(
        lapply(names(combos), function(combo_name) {
          combo_svcs <- combos[[combo_name]]
          sapply(hotspot_services_list, \(x) sum(x %in% combo_svcs))
        }),
        paste0("count_", names(combos))
      ),
      
      # Classify type
      hotspot_types = sapply(hotspot_services_list, function(svcs) {
        types <- sapply(svcs, function(s) {
          if (s %in% loss_services) "loss"
          else if (s %in% gain_services) "damage"
          else NA_character_
        })
        paste(na.omit(types), collapse = ", ")
      }),
      
      hotspot_services_flagged = hotspot_services
    ) %>%
    select(-hotspot_services_list)  # Drop list column for sf compatibility
  
  # ---------------------------------------------
  # Step 4: Binary matrix
  # ---------------------------------------------
  hotspot_binary_matrix <- hotspot_summary %>%
    select(.data[[fid_col]], hotspot_services_flagged) %>%
    separate_rows(hotspot_services_flagged, sep = ",\\s*") %>%
    mutate(is_hotspot = 1L) %>%
    pivot_wider(names_from = hotspot_services_flagged, values_from = is_hotspot, values_fill = 0)
  
  # ---------------------------------------------
  # Step 5: Join and return
  # ---------------------------------------------
  output <- hotspot_binary_matrix %>%
    inner_join(hotspot_summary, by = fid_col)
  
  if (!is.null(sf_data)) {
    output <- sf_data %>%
      inner_join(output, by = fid_col)
  }
  
  return(output)
}
