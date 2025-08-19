#' Extract hotspots from long-format service-change data
#'
#' Identifies hotspots per service using percentile or fixed-count thresholds.
#' Supports two modes:
#' 1) rule_mode = "vectors": apply service-specific directions where
#'    services in `loss_services` are "good when low" and services in
#'    `gain_services` are "good when high".
#' 2) rule_mode = "explicit": ignore loss/gain vectors and use `side`
#'    to select "high", "low", or "both" extremes for all services.
#'
#' Optionally computes per-feature combo counts, expands to a wide binary
#' matrix of hotspot services, joins to an sf object, and writes outputs.
#'
#' @param df A data.frame (long format) containing at least columns
#'   `service`, `fid`, and a numeric change column (default `pct_chg`).
#' @param value_col Name of the numeric column to rank by (default "pct_chg").
#' @param pct_cutoff Percentile cutoff in (0,1] when `threshold_mode="percent"`.
#' @param n_cut Optional integer when `threshold_mode="count"`; number of rows
#'   per service to keep at each side (top/bottom). Ignored otherwise.
#' @param threshold_mode "percent" (default) or "count".
#' @param rule_mode "vectors" (default) to use `loss_services`/`gain_services`,
#'   or "explicit" to use `side` for all services.
#' @param side When `rule_mode="explicit"`, choose "high", "low", or "both".
#'   Ignored if `rule_mode="vectors"`.
#' @param loss_services Character vector of services where decreases are desirable.
#' @param gain_services Character vector of services where increases are desirable.
#' @param combos Optional named list of character vectors, each listing
#'   services that form a combo (e.g., list(combo_1 = c("A","B"))).
#' @param id_cols Optional character vector of extra ID columns to carry through
#'   (e.g., c("c_fid")). These will be summarised with `dplyr::first()`.
#' @param sf_obj Optional sf object containing a column `fid` to join results.
#' @param write_sf_path Optional file path to write the hotspot sf to disk.
#' @param write_driver GDAL driver name when writing (e.g., "GPKG", "ESRI Shapefile").
#'
#' @return A list with:
#'   - hotspots_df: per-row hotspots (long)
#'   - non_hotspots_df: complement (long)
#'   - summary_df: one row per fid with service list, types, and combo counts
#'   - binary_matrix: wide binary service matrix by fid
#'   - hotspots_sf: sf with binary/service columns (if sf_obj supplied)
#'   - summary_sf: sf with summary columns (if sf_obj supplied)
#'
#' @examples
#' # hotspots <- extract_hotspots(
#' #   df = plt_long,
#' #   value_col = "pct_chg",
#' #   pct_cutoff = 0.05,
#' #   threshold_mode = "percent",
#' #   rule_mode = "vectors",
#' #   loss_services = c("Nature_Access","Pollination","N_Ret_Ratio","Sed_Ret_Ratio","C_Risk_Red_Ratio"),
#' #   gain_services = c("Sed_export","N_export","C_Risk"),
#' #   combos = list(
#' #     combo_1 = c("Nature_Access","Pollination","N_export","Sed_export","C_Risk"),
#' #     combo_2 = c("Nature_Access","Pollination","N_Ret_Ratio","Sed_Ret_Ratio","C_Risk_Red_Ratio")
#' #   ),
#' #   id_cols = c("c_fid"),
#' #   sf_obj = sf_f,
#' #   write_sf_path = NULL
#' # )
#'
#' @importFrom dplyr group_by ungroup mutate summarise first n across any_of arrange
#' @importFrom dplyr select left_join inner_join rename bind_cols filter case_when
#' @importFrom tidyr separate_rows pivot_wider
#' @importFrom tibble as_tibble tibble
#' @importFrom purrr map set_names
#' @importFrom sf st_as_sf
#' @export
extract_hotspots <- function(
    df,
    value_col = "pct_chg",
    pct_cutoff = 0.05,
    n_cut = NULL,
    threshold_mode = c("percent","count"),
    rule_mode = c("vectors","explicit"),
    side = c("high","low","both"),
    loss_services = character(0),
    gain_services = character(0),
    combos = NULL,
    id_cols = NULL,
    sf_obj = NULL,
    write_sf_path = NULL,
    write_driver = "GPKG"
) {
  threshold_mode <- match.arg(threshold_mode)
  rule_mode <- match.arg(rule_mode)
  side <- match.arg(side)
  
  stopifnot(all(c("fid","service", value_col) %in% names(df)))
  
  # Step 1. ranks and flags per service
  df_hotspots <- df %>%
    dplyr::group_by(.data$service) %>%
    dplyr::mutate(
      n_total = dplyr::n(),
      n_cut = dplyr::case_when(
        threshold_mode == "percent" ~ ceiling(n_total * pct_cutoff),
        threshold_mode == "count" ~ ifelse(is.null(n_cut), 1L, as.integer(n_cut))
      ),
      rank_high = rank(-.data[[value_col]], ties.method = "first"),
      rank_low  = rank( .data[[value_col]], ties.method = "first"),
      flag_high = rank_high <= n_cut,
      flag_low  = rank_low  <= n_cut
    ) %>%
    dplyr::ungroup()
  
  # Step 2. choose relevant directions
  if (rule_mode == "vectors") {
    df_hotspots <- df_hotspots %>%
      dplyr::mutate(
        hotspot_binary =
          (.data$service %in% loss_services & .data$flag_low) |
          (.data$service %in% gain_services & .data$flag_high),
        hotspot_flag = dplyr::case_when(
          .data$service %in% loss_services & .data$flag_low  ~ "low",
          .data$service %in% gain_services & .data$flag_high ~ "high",
          TRUE ~ NA_character_
        )
      )
  } else {
    df_hotspots <- df_hotspots %>%
      dplyr::mutate(
        hotspot_binary = dplyr::case_when(
          side == "high" ~ .data$flag_high,
          side == "low"  ~ .data$flag_low,
          side == "both" ~ (.data$flag_high | .data$flag_low)
        ),
        hotspot_flag = dplyr::case_when(
          .data$flag_high & (side %in% c("high","both")) ~ "high",
          .data$flag_low  & (side %in% c("low","both"))  ~ "low",
          TRUE ~ NA_character_
        )
      )
  }
  
  # Subsets
  hotspots_df <- df_hotspots %>% dplyr::filter(.data$hotspot_binary)
  non_hotspots_df <- df_hotspots %>% dplyr::filter(!.data$hotspot_binary)
  
  # Step 3. per-fid summary
  group_cols <- c("fid")
  carry_cols <- intersect(id_cols %||% character(0), names(df_hotspots))
  
  hotspot_summary <- hotspots_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(carry_cols), dplyr::first, .names = "{.col}"),
      hotspot_count = dplyr::n(),
      hotspot_services_list = list(unique(.data$service)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      hotspot_services = vapply(
        hotspot_services_list,
        function(x) paste(trimws(as.character(x)), collapse = ", "),
        FUN.VALUE = character(1)
      )
    )
  
  # Combo counts
  if (!is.null(combos) && length(combos) > 0) {
    combo_counts <- lapply(names(combos), function(combo_name) {
      combo_svcs <- combos[[combo_name]]
      vapply(
        hotspot_summary$hotspot_services_list,
        function(x) sum(x %in% combo_svcs),
        FUN.VALUE = integer(1)
      )
    })
    names(combo_counts) <- paste0("count_", names(combos))
    hotspot_summary <- dplyr::bind_cols(hotspot_summary, tibble::as_tibble(combo_counts))
  }
  
  # Types (loss/damage) for readability
  hotspot_summary <- hotspot_summary %>%
    dplyr::mutate(
      hotspot_types = vapply(
        hotspot_services_list,
        function(svcs) {
          types <- vapply(svcs, function(s) {
            if (s %in% loss_services) "loss"
            else if (s %in% gain_services) "damage"
            else NA_character_
          }, FUN.VALUE = character(1))
          paste(na.omit(types), collapse = ", ")
        },
        FUN.VALUE = character(1)
      )
    )
  
  # Step 4. wide binary matrix
  binary_matrix <- hotspot_summary %>%
    dplyr::select(dplyr::all_of(c("fid","hotspot_services"))) %>%
    tidyr::separate_rows(hotspot_services, sep = ",\\s*") %>%
    dplyr::filter(nzchar(.data$hotspot_services)) %>%
    dplyr::mutate(is_hotspot = 1L) %>%
    tidyr::pivot_wider(
      names_from = hotspot_services,
      values_from = is_hotspot,
      values_fill = 0
    )
  
  # Step 5. optional join to sf
  hotspots_sf <- NULL
  summary_sf <- NULL
  
  if (!is.null(sf_obj)) {
    stopifnot("fid" %in% names(sf_obj))
    hotspots_sf <- sf_obj %>%
      dplyr::inner_join(binary_matrix, by = "fid")
    
    summary_sf <- sf_obj %>%
      dplyr::left_join(
        hotspot_summary %>% dplyr::select(-.data$hotspot_services_list),
        by = "fid"
      )
    
    if (!is.null(write_sf_path)) {
      sf::st_write(hotspots_sf, write_sf_path, delete_dsn = TRUE, driver = write_driver, quiet = TRUE)
    }
  }
  
  list(
    hotspots_df = hotspots_df,
    non_hotspots_df = non_hotspots_df,
    summary_df = hotspot_summary,
    binary_matrix = binary_matrix,
    hotspots_sf = hotspots_sf,
    summary_sf = summary_sf
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
