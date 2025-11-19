# ---- Config you already used (keep) -------------------------------------

loss <- c("Nature_Access","Pollination","N_Ret_Ratio","Sed_Ret_Ratio","C_Risk_Red_Ratio")
gain <- c("Sed_export","N_export","C_Risk")

# Target facet order

facet_core <- c(
  "C_Risk","N_export","Sed_export",
  "C_Risk_Red_Ratio","N_Ret_Ratio","Sed_Ret_Ratio",
  "Pollination","Nature_Access"
)

# Helper: run wrapper and produce ONE mixed-metric violin per grouping

run_mixed_violin_by <- function(group_col,
                                out_stub,
                                upper_cut = 0.999,  # 99.9% trim
                                plot_n   = 300000L   # sampling cap for speed
){
  stopifnot(group_col %in% names(plt_long))
  
  message("==> Mixed-metric violins by: ", group_col)
  
  # 1) Run hotspots for ABS & PCT once
  
  by_abs <- extract_hotspots_by(plt_long, group_col, loss, gain, value_col = "abs_chg")
  by_pct <- extract_hotspots_by(plt_long, group_col, loss, gain, value_col = "pct_chg")
  
  # 2) Tidy value tables (fid × service × group)
  
  abs_vals <- by_abs |>
    dplyr::transmute(
      !!group_col := .data[[group_col]],
      vals = purrr::map(hotspots_df, ~ dplyr::select(.x, fid, service, abs_chg))
    ) |>
    tidyr::unnest(vals)
  
  pct_vals <- by_pct |>
    dplyr::transmute(
      !!group_col := .data[[group_col]],
      vals = purrr::map(hotspots_df, ~ dplyr::select(.x, fid, service, pct_chg))
    ) |>
    tidyr::unnest(vals)
  
  # 3) Join measures so each (fid,service,group) can pick its metric
  
  vals <- dplyr::full_join(
    abs_vals, pct_vals,
    by = c(group_col, "fid", "service")
  )
  
  # 4) Build chosen metric:
  
  # - Nature_Access -> use pct_chg
  
  # - others        -> use abs_chg
  
  vals <- vals |>
    dplyr::mutate(
      metric    = dplyr::if_else(service == "Nature_Access", "pct", "abs"),
      y         = dplyr::if_else(metric == "pct", pct_chg, abs_chg),
      # nice facet label (only rename Access)
      service_lab = dplyr::if_else(service == "Nature_Access", "Access", as.character(service)),
      # keep group factor stable
      !!group_col := as.factor(.data[[group_col]])
    ) |>
    dplyr::filter(!is.na(.data[[group_col]]), !is.na(y), y != 0)
  
  # 5) Trim outliers per-service (upper quantile)
  
  vals <- vals |>
    dplyr::group_by(service) |>
    dplyr::mutate(y_upper = stats::quantile(y, probs = upper_cut, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::filter(y <= y_upper)
  
  # 6) Optional sampling to keep render fast
  
  if (nrow(vals) > plot_n) {
    vals <- dplyr::slice_sample(vals, n = plot_n)
  }
  
  # 7) Facet order: core list first, all others afterward
  
  all_services <- unique(vals$service)
  extra <- setdiff(all_services, facet_core)
  facet_levels <- c(facet_core, extra)
  vals <- vals |>
    dplyr::mutate(
      service = forcats::fct_relevel(service, !!!facet_levels, after = 0L),
      # ensure label vector matches the same order
      service_lab = factor(service_lab,
                           levels = dplyr::recode(facet_levels,
                                                  "Nature_Access" = "Access", .default = facet_levels))
    )
  
  # 8) Plot: violins only (no boxplots), same styling as before
  
  p <- ggplot2::ggplot(vals, ggplot2::aes(x = y, y = .data[[group_col]])) +
    ggplot2::geom_violin(trim = TRUE, scale = "width") +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ service_lab, ncol = 3, scales = "free_x") +
    ggplot2::labs(
      title    = paste0("Hotspot change by ", group_col),
      subtitle = "Metric rule: absolute change for all services except Access (percent change).\nValues trimmed at the 99.9th percentile; zeros and NAs removed.",
      x        = NULL,
      y        = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 8, angle = 45, hjust = 1),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  # 9) Save inside repo
  
  out_dir <- file.path("outputs", "plots")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, paste0(out_stub, "_mixed_metric_violins.png"))
  ggplot2::ggsave(out_path, p, width = 12, height = 8, dpi = 300)
  
  message("   saved: ", out_path)
  invisible(out_path)
}

# ---- Run for each grouping you care about --------------------------------

run_mixed_violin_by("region_wb", "regionwb")
run_mixed_violin_by("BIOME",     "biome")
run_mixed_violin_by("continent", "continent")
run_mixed_violin_by("region_un", "regionun")
run_mixed_violin_by("income_grp","incomegrp")
#' Extract hotspots from long-format service-change data
#'
#' Identifies hotspots per service using percentile or fixed-count thresholds.
#' Two rule modes are supported:
#' - `rule_mode = "vectors"`: services in `loss_services` are "good when low"
#'   and services in `gain_services` are "good when high".
#' - `rule_mode = "explicit"`: ignore loss/gain vectors and use `side`
#'   ("high", "low", or "both") for all services.
#'
#' Returns per-row hotspot flags, the complement (non-hotspots), a per-`fid`
#' summary (count, types, combos, service list), a wide binary matrix (one
#' column per service), and (optionally) an `sf` with **only the hotspot
#' features** that already includes the summary fields **and** the wide
#' binary columns. When writing to disk, the function can **duplicate** `fid`
#' into a safe alias (e.g., `fid_2`) and (optionally) **drop** the original
#' `fid` on disk to avoid GDAL driver conflicts (e.g., Shapefile).
#'
#' @param df A data.frame (long format) with `service`, `fid`, and a numeric
#'   change column (default `pct_chg`). May include additional ID cols.
#' @param value_col Name of numeric column used to rank (default "pct_chg").
#' @param pct_cutoff Percentile cutoff in (0,1] when `threshold_mode="percent"`.
#' @param n_cut Optional integer when `threshold_mode="count"`; number of rows
#'   per service to keep at each side (top/bottom).
#' @param threshold_mode "percent" (default) or "count".
#' @param rule_mode "vectors" (default) or "explicit".
#' @param side When `rule_mode="explicit"`, choose "high", "low", or "both".
#' @param loss_services Character vector of services where decreases are desirable.
#' @param gain_services Character vector of services where increases are desirable.
#' @param combos Optional named list of character vectors, each listing services
#'   that form a combo (e.g., `list(combo_1 = c("A","B"))`). Combo columns are
#'   added to the summary and `hotspots_sf`.
#' @param id_cols Optional character vector of extra ID columns from `df` to carry
#'   through summaries (e.g., `"c_fid"`). These are summarised with `dplyr::first()`.
#'   They are **not** duplicated into `hotspots_sf` if already present in `sf_obj`.
#' @param sf_obj Optional `sf` object containing a `fid` column to join results.
#'   If supplied, an `hotspots_sf` is returned (and optionally written to disk).
#'   `sf_obj` must have unique `fid`; duplicates error.
#' @param write_sf_path Optional file path to write the hotspot `sf` to disk.
#' @param write_driver GDAL driver (e.g., `"GPKG"`, `"ESRI Shapefile"`). Default "GPKG".
#' @param clean_names Logical; if `TRUE`, clean the **new** attribute names
#'   (binary and summary columns) for GIS friendliness (underscored, ASCII).
#' @param fid_alias Character or `NULL`. If non-NULL and `write_sf_path` is set,
#'   duplicate `fid` to this alias **only for the file on disk** (default `"fid_2"`).
#' @param drop_fid_on_write Logical; if `TRUE`, drop the original `fid` from the
#'   on-disk file after aliasing. The returned `hotspots_sf` in R keeps `fid`.
#'
#' @return A list with:
#' \describe{
#'   \item{hotspots_df}{per-row hotspots (long tibble)}
#'   \item{non_hotspots_df}{complement (long tibble)}
#'   \item{summary_df}{one row per fid with counts, types, combos, service list}
#'   \item{binary_matrix}{wide binary service matrix by fid (hotspot fids only)}
#'   \item{hotspots_sf}{`sf` with hotspot features + summary + wide binary columns (if `sf_obj` supplied)}
#' }
#'
#' @examples
#' \dontrun{
#' hs <- extract_hotspots(
#'   df = plt_long,
#'   value_col = "pct_chg",
#'   pct_cutoff = 0.05,
#'   threshold_mode = "percent",
#'   rule_mode = "vectors",
#'   loss_services = c("Nature_Access","Pollination","N_Ret_Ratio","Sed_Ret_Ratio","C_Risk_Red_Ratio"),
#'   gain_services = c("Sed_export","N_export","C_Risk"),
#'   combos = list(
#'     deg_combo = c("Nature_Access","Pollination","N_export","Sed_export","C_Risk"),
#'     rec_combo = c("Nature_Access","Pollination","N_Ret_Ratio","Sed_Ret_Ratio","C_Risk_Red_Ratio")
#'   ),
#'   id_cols = c("c_fid"),
#'   sf_obj = sf_f,
#'   write_sf_path = "output_charts/hotspots.gpkg",
#'   write_driver  = "GPKG",
#'   clean_names   = TRUE,
#'   fid_alias = "fid_2",
#'   drop_fid_on_write = TRUE
#' )
#' }
#'
#' @importFrom dplyr group_by ungroup mutate summarise first n across any_of arrange
#' @importFrom dplyr select left_join inner_join rename bind_cols filter case_when distinct pull
#' @importFrom tidyr separate_rows pivot_wider
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map_lgl
#' @importFrom sf st_as_sf st_drop_geometry st_write st_crs
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
    write_driver = "GPKG",
    clean_names = TRUE,
    fid_alias = "fid_2",
    drop_fid_on_write = TRUE
) {
  threshold_mode <- match.arg(threshold_mode)
  rule_mode      <- match.arg(rule_mode)
  side           <- match.arg(side)
  
  stopifnot(all(c("fid","service", value_col) %in% names(df)))
  
  #--- helpers ------------------------------------------------------------
  `%||%` <- function(x, y) if (is.null(x)) y else x
  clean_nm <- function(x) {
    x <- gsub("[^A-Za-z0-9_]+", "_", x)
    x <- gsub("_+", "_", x)
    x <- gsub("^_+", "", x)
    x <- sub("^([0-9])", "_\\1", x)
    x
  }
  
  #--- 1) rank & flags per service ---------------------------------------
  df_hotspots <- df %>%
    dplyr::group_by(.data$service) %>%
    dplyr::mutate(
      n_total = dplyr::n(),
      n_cut   = dplyr::case_when(
        threshold_mode == "percent" ~ ceiling(n_total * pct_cutoff),
        threshold_mode == "count"   ~ ifelse(is.null(n_cut), 1L, as.integer(n_cut))
      ),
      rank_high = rank(-.data[[value_col]], ties.method = "first"),
      rank_low  = rank( .data[[value_col]], ties.method = "first"),
      flag_high = .data$rank_high <= n_cut,
      flag_low  = .data$rank_low  <= n_cut
    ) %>%
    dplyr::ungroup()
  
  #--- 2) choose directions ----------------------------------------------
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
  
  hotspots_df     <- df_hotspots %>% dplyr::filter(.data$hotspot_binary)
  non_hotspots_df <- df_hotspots %>% dplyr::filter(!.data$hotspot_binary)
  
  #--- 3) per-fid summary -------------------------------------------------
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
      ),
      hotspot_types = vapply(
        hotspot_services_list,
        function(svcs) {
          types <- vapply(svcs, function(s) {
            if (s %in% loss_services) "loss"
            else if (s %in% gain_services) "damage"
            else NA_character_
          }, FUN.VALUE = character(1))
          paste(stats::na.omit(types), collapse = ", ")
        },
        FUN.VALUE = character(1)
      )
    )
  
  #--- 4) optional combo counts ------------------------------------------
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
  
  #--- 5) wide binary matrix (hotspots only) ------------------------------
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
  
  if (clean_names) {
    new_names <- names(binary_matrix)
    new_names[-match("fid", new_names)] <- clean_nm(new_names[-match("fid", new_names)])
    names(binary_matrix) <- new_names
    
    keep_to_clean <- setdiff(names(hotspot_summary), c("fid"))
    names(hotspot_summary)[match(keep_to_clean, names(hotspot_summary))] <-
      clean_nm(keep_to_clean)
  }
  
  #--- 6) hotspot attributes for sf ---------------------------------------
  hotspot_attrs <- hotspot_summary %>%
    dplyr::select(-hotspot_services_list) %>%
    dplyr::left_join(binary_matrix, by = "fid")
  
  #--- 7) optional join to sf (hotspot features only) ---------------------
  hotspots_sf <- NULL
  if (!is.null(sf_obj)) {
    stopifnot("fid" %in% names(sf_obj))
    if (any(duplicated(sf_obj$fid))) {
      stop("`sf_obj` must have unique `fid` per feature; found duplicates.")
    }
    
    # Avoid duplicating ID columns already present in sf_obj (keep sf_obj's)
    drop_from_attrs <- intersect(names(hotspot_attrs), setdiff(names(sf_obj), "fid"))
    drop_from_attrs <- setdiff(drop_from_attrs, c("hotspot_count","hotspot_services","hotspot_types"))
    hotspot_attrs_slim <- hotspot_attrs %>%
      dplyr::select(-dplyr::all_of(drop_from_attrs))
    
    hotspots_sf <- sf_obj %>%
      dplyr::inner_join(hotspot_attrs_slim, by = "fid")
    
    # ---- write if requested, with fid aliasing to avoid GDAL conflicts
    if (!is.null(write_sf_path)) {
      to_write <- hotspots_sf
      
      # Duplicate fid into alias (only for the file), and optionally drop fid
      alias_name <- fid_alias
      if (!is.null(alias_name) && "fid" %in% names(to_write)) {
        # sanitize alias; enforce 10-char max for Shapefile
        alias_name <- gsub("[^A-Za-z0-9_]+", "_", alias_name)
        alias_name <- gsub("_+", "_", alias_name)
        alias_name <- sub("^([0-9])", "_\\1", alias_name)
        if (identical(write_driver, "ESRI Shapefile") && nchar(alias_name) > 10) {
          alias_name <- substr(alias_name, 1, 10)
        }
        # ensure uniqueness
        while (alias_name %in% names(to_write)) {
          alias_name <- paste0(alias_name, "_x")
          if (identical(write_driver, "ESRI Shapefile") && nchar(alias_name) > 10) {
            alias_name <- substr(alias_name, 1, 10)
          }
        }
        to_write[[alias_name]] <- to_write$fid
      }
      
      if (isTRUE(drop_fid_on_write) && "fid" %in% names(to_write)) {
        to_write$fid <- NULL
      }
      
      dir.create(dirname(write_sf_path), recursive = TRUE, showWarnings = FALSE)
      sf::st_write(to_write, write_sf_path, delete_dsn = TRUE,
                   driver = write_driver, quiet = TRUE)
    }
  }
  
  #--- return -------------------------------------------------------------
  list(
    hotspots_df     = hotspots_df,
    non_hotspots_df = non_hotspots_df,
    summary_df      = hotspot_summary,
    binary_matrix   = binary_matrix,
    hotspots_sf     = hotspots_sf
  )
}

