#' Faceted boxplots of hotspot change by grouping column
#'
#' @param df_long Long-form change table (`plt_long` style).
#' @param group_col Column name to facet by (string).
#' @param loss,gain Character vectors of services treated as losses / gains.
#' @param pct_cutoff Numeric cutoff (e.g., 0.05) passed to `extract_hotspots_by()`.
#' @param threshold_mode Threshold interpretation (passed through).
#' @param svc_order Optional service ordering; defaults to factor levels in `df_long`.
#' @param plot_n Max rows sampled for plotting (to keep files manageable).
#' @param keep_only_ordered Keep only services in `svc_order`.
#' @param out_root Directory root for plots (defaults to `out_plots()`).
#'
#' @return List with `abs_box` and `pct_box` invisibly; also writes PNGs.
#' @export
run_hotspot_boxplots_by <- function(
    df_long,
    group_col,
    loss,
    gain,
    pct_cutoff,
    threshold_mode,
    svc_order = NULL,
    plot_n = 300000L,
    keep_only_ordered = TRUE,
    out_root = out_plots(),
    volumetric_services = NULL,
    ratio_services = NULL) {

  stopifnot(group_col %in% names(df_long))

  # Define canonical order for all services (edit as needed)
  canonical_order <- c(
    "N_export", "Sed_export", "Pollination", "Nature_Access",
    "C_Risk", "C_Risk_Red_Ratio", "N_Ret_Ratio", "Sed_Ret_Ratio"
  )
  # Use canonical order for svc_order if not provided
  if (is.null(svc_order)) {
    svc_order <- canonical_order
  }
  svc_order <- as.character(svc_order)

  message("==> Boxplots by: ", group_col)

  # Original data extraction remains the same
  by_abs <- extract_hotspots_by(df_long, group_cols=group_col, loss=loss, gain=gain, value_col="abs_chg", pct_cutoff=pct_cutoff, threshold_mode=threshold_mode)
  by_pct <- extract_hotspots_by(df_long, group_cols=group_col, loss=loss, gain=gain, value_col="pct_chg", pct_cutoff=pct_cutoff, threshold_mode=threshold_mode)

  abs_vals <- by_abs %>% dplyr::transmute(!!group_col := .data[[group_col]], vals=purrr::map(hotspots_df, ~ dplyr::select(.x, fid, service, abs_chg))) %>% tidyr::unnest(vals)
  pct_vals <- by_pct %>% dplyr::transmute(!!group_col := .data[[group_col]], vals=purrr::map(hotspots_df, ~ dplyr::select(.x, fid, service, pct_chg))) %>% tidyr::unnest(vals)

  vals <- dplyr::full_join(abs_vals, pct_vals, by = c(group_col, "fid", "service")) %>%
    { if (isTRUE(keep_only_ordered)) dplyr::filter(., service %in% svc_order) else . } %>%
    dplyr::mutate(service = { extras <- setdiff(unique(service), svc_order); factor(service, levels = c(svc_order, sort(extras))) }) %>%
    dplyr::filter(!is.na(.data[[group_col]])) %>%
    { if (nrow(.) > plot_n) dplyr::slice_sample(., n = plot_n) else . } %>%
    dplyr::mutate(!!group_col := as.factor(.data[[group_col]]))

  if (nrow(vals) == 0) {
    message("No hotspot data found for grouping: ", group_col)
    return(invisible(NULL))
  }

  calc_box_stats <- function(df, var) {
    df %>%
      dplyr::filter(!is.na(.data[[var]])) %>%
      dplyr::group_by(service, .data[[group_col]]) %>%
      dplyr::summarise(
        middle = stats::median(.data[[var]], na.rm = TRUE),
        lower  = stats::quantile(.data[[var]], 0.25, na.rm = TRUE),
        upper  = stats::quantile(.data[[var]], 0.75, na.rm = TRUE),
        iqr    = stats::IQR(.data[[var]], na.rm = TRUE),
        ymin   = max(min(.data[[var]], na.rm = TRUE), lower - 1.5 * iqr),
        ymax   = min(max(.data[[var]], na.rm = TRUE), upper + 1.5 * iqr),
        .groups = "drop"
      )
  }

  # Define which services get both abs and pct plots (volumetric), and which are ratios/indices
  if (is.null(volumetric_services)) {
    volumetric_services <- c("Nature_Access", "Pollination", "Sed_export", "N_export")
  }
  if (is.null(ratio_services)) {
    ratio_services <- c("C_Risk", "C_Risk_Red_Ratio", "N_Ret_Ratio", "Sed_Ret_Ratio")
  }


  # --- PLOT: Volumetric Services ---
  volumetric_order <- canonical_order[canonical_order %in% volumetric_services]
  vals_vol <- dplyr::filter(vals, service %in% volumetric_order) %>%
    dplyr::mutate(service = factor(service, levels = volumetric_order))
  if (nrow(vals_vol) > 0) {
    # Absolute
    stats_abs_vol <- calc_box_stats(vals_vol, "abs_chg")
    p_abs_box_vol <- ggplot2::ggplot(stats_abs_vol, ggplot2::aes(x = .data[[group_col]], ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax)) +
      ggplot2::geom_boxplot(stat = "identity", fill = "gray90") +
      ggplot2::facet_wrap(~ service, scales = "free_y", ncol = 2) +
      ggplot2::labs(title=paste0("Absolute Change (Volumetric Services) by ", group_col), subtitle="Whiskers = 1.5*IQR. Outliers hidden.", x=NULL, y="Absolute change") +
      ggplot2::theme_minimal(base_size = 11) + ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8, angle = 45, hjust = 1))
    dir_abs <- file.path(out_root, "abs", tolower(group_col))
    dir.create(dir_abs, recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(filename=file.path(dir_abs, paste0("boxplots_", tolower(group_col), "_abs_volumetric.png")), plot=p_abs_box_vol, width=12, height=8, dpi=300, bg="white")

    # Percent
    stats_pct_vol <- calc_box_stats(vals_vol, "pct_chg")
    p_pct_box_vol <- ggplot2::ggplot(stats_pct_vol, ggplot2::aes(x = .data[[group_col]], ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax)) +
      ggplot2::geom_boxplot(stat = "identity", fill = "gray90") +
      ggplot2::facet_wrap(~ service, scales = "free_y", ncol = 2) +
      ggplot2::labs(title=paste0("Percentage Change (Volumetric Services) by ", group_col), subtitle="Whiskers = 1.5*IQR. Outliers hidden.", x=NULL, y="Percentage change (%)") +
      ggplot2::theme_minimal(base_size = 11) + ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8, angle = 45, hjust = 1))
    dir_pct <- file.path(out_root, "pct", tolower(group_col))
    dir.create(dir_pct, recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(filename=file.path(dir_pct, paste0("boxplots_", tolower(group_col), "_pct_volumetric.png")), plot=p_pct_box_vol, width=12, height=8, dpi=300, bg="white")
  }

  # --- PLOT: Ratio/Index Services (only one plot needed) ---
  # Only plot ratio_services that are present in the data, with specified facet order/layout
  ratio_present <- c("C_Risk", "C_Risk_Red_Ratio", "N_Ret_Ratio", "Sed_Ret_Ratio")
  ratio_present <- ratio_present[ratio_present %in% unique(vals$service)]
  vals_ratio <- dplyr::filter(vals, service %in% ratio_present) %>%
    dplyr::mutate(service = factor(service, levels = ratio_present))
  if (nrow(vals_ratio) > 0) {
    stats_abs_ratio <- calc_box_stats(vals_ratio, "abs_chg")
    p_abs_box_ratio <- ggplot2::ggplot(stats_abs_ratio, ggplot2::aes(x = .data[[group_col]], ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax)) +
      ggplot2::geom_boxplot(stat = "identity", fill = "gray90") +
      ggplot2::facet_wrap(~ service, scales = "free_y", ncol = 2) +
      ggplot2::labs(title=paste0("Change in Ratio/Index Services by ", group_col), subtitle="Whiskers = 1.5*IQR. Outliers hidden.", x=NULL, y="Change (ratio/index units)") +
      ggplot2::theme_minimal(base_size = 11) + ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8, angle = 45, hjust = 1))
    dir_ratio <- file.path(out_root, "ratios", tolower(group_col))
    dir.create(dir_ratio, recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(filename=file.path(dir_ratio, paste0("boxplots_", tolower(group_col), "_ratios.png")), plot=p_abs_box_ratio, width=12, height=8, dpi=300, bg="white")
  }

  # Return a list of plots invisibly
  invisible(list(
    abs_box_vol = if (exists("p_abs_box_vol")) p_abs_box_vol else NULL,
    pct_box_vol = if (exists("p_pct_box_vol")) p_pct_box_vol else NULL,
    abs_box_ratio = if (exists("p_abs_box_ratio")) p_abs_box_ratio else NULL
  ))
}
