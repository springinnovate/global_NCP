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
    out_root = out_plots()) {

  stopifnot(group_col %in% names(df_long))
  if (is.null(svc_order)) {
    svc_order <- levels(df_long$service)
    if (is.null(svc_order)) svc_order <- unique(df_long$service)
  }
  svc_order <- as.character(svc_order)

  message("==> Boxplots by: ", group_col)

  by_abs <- extract_hotspots_by(
    df_long        = df_long,
    group_cols     = group_col,
    loss           = loss,
    gain           = gain,
    value_col      = "abs_chg",
    pct_cutoff     = pct_cutoff,
    threshold_mode = threshold_mode
  )

  by_pct <- extract_hotspots_by(
    df_long        = df_long,
    group_cols     = group_col,
    loss           = loss,
    gain           = gain,
    value_col      = "pct_chg",
    pct_cutoff     = pct_cutoff,
    threshold_mode = threshold_mode
  )

  abs_vals <- by_abs %>%
    dplyr::transmute(
      !!group_col := .data[[group_col]],
      vals = purrr::map(hotspots_df, ~ dplyr::select(.x, fid, service, abs_chg))
    ) %>%
    tidyr::unnest(vals)

  pct_vals <- by_pct %>%
    dplyr::transmute(
      !!group_col := .data[[group_col]],
      vals = purrr::map(hotspots_df, ~ dplyr::select(.x, fid, service, pct_chg))
    ) %>%
    tidyr::unnest(vals)

  vals <- dplyr::full_join(abs_vals, pct_vals, by = c(group_col, "fid", "service")) %>%
    { if (isTRUE(keep_only_ordered)) dplyr::filter(., service %in% svc_order) else . } %>%
    dplyr::mutate(
      service = {
        extras <- setdiff(unique(service), svc_order)
        factor(service, levels = c(svc_order, sort(extras)))
      }
    ) %>%
    dplyr::filter(!is.na(.data[[group_col]])) %>%
    { if (nrow(.) > plot_n) dplyr::slice_sample(., n = plot_n) else . } %>%
    dplyr::mutate(!!group_col := as.factor(.data[[group_col]]))

  if (nrow(vals) == 0) {
    message("No hotspot data found for grouping: ", group_col)
    return(invisible(NULL))
  }

  # --- Boxplots (Stats on full data, view zoomed to whiskers) ---
  # We calculate stats manually to ensure the box represents the full distribution
  # (not the filtered one), but we plot without outliers to "zoom in" the axis.
  calc_box_stats <- function(df, var) {
    df %>%
      dplyr::filter(!is.na(.data[[var]])) %>%
      dplyr::group_by(service, .data[[group_col]]) %>%
      dplyr::summarise(
        middle = stats::median(.data[[var]], na.rm = TRUE),
        lower  = stats::quantile(.data[[var]], 0.25, na.rm = TRUE),
        upper  = stats::quantile(.data[[var]], 0.75, na.rm = TRUE),
        iqr    = stats::IQR(.data[[var]], na.rm = TRUE),
        # Whiskers: 1.5 IQR from hinge, bounded by data range
        ymin   = max(min(.data[[var]], na.rm = TRUE), lower - 1.5 * iqr),
        ymax   = min(max(.data[[var]], na.rm = TRUE), upper + 1.5 * iqr),
        .groups = "drop"
      )
  }
  stats_abs <- calc_box_stats(vals, "abs_chg")
  stats_pct <- calc_box_stats(vals, "pct_chg")

  p_abs_box <- ggplot2::ggplot(stats_abs, ggplot2::aes(x = .data[[group_col]], ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax)) +
    ggplot2::geom_boxplot(stat = "identity", fill = "gray90") +
    ggplot2::facet_wrap(~ service, scales = "free_y", ncol = 3) +
    ggplot2::labs(
      title    = paste0("Absolute change (Boxplot) by ", group_col),
      subtitle = "Whiskers = 1.5*IQR. Outliers hidden to zoom view.",
      x = NULL, y = "Absolute change (service units)"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8, angle = 45, hjust = 1))

  p_pct_box <- ggplot2::ggplot(stats_pct, ggplot2::aes(x = .data[[group_col]], ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax)) +
    ggplot2::geom_boxplot(stat = "identity", fill = "gray90") +
    ggplot2::facet_wrap(~ service, scales = "free_y", ncol = 3) +
    ggplot2::labs(
      title    = paste0("Percent change (Boxplot) by ", group_col),
      subtitle = "Whiskers = 1.5*IQR. Outliers hidden to zoom view.",
      x = NULL, y = "Percent change (%)"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8, angle = 45, hjust = 1))

  dir_abs <- file.path(out_root, "abs", tolower(group_col))
  dir_pct <- file.path(out_root, "pct", tolower(group_col))
  dir.create(dir_abs, recursive = TRUE, showWarnings = FALSE)
  dir.create(dir_pct, recursive = TRUE, showWarnings = FALSE)

  ggplot2::ggsave(
    filename = file.path(dir_abs, paste0("boxplots_", tolower(group_col), "_abs.png")),
    plot = p_abs_box, width = 12, height = 8, dpi = 300, bg = "white"
  )
  ggplot2::ggsave(
    filename = file.path(dir_pct, paste0("boxplots_", tolower(group_col), "_pct.png")),
    plot = p_pct_box, width = 12, height = 8, dpi = 300, bg = "white"
  )

  invisible(list(abs_box = p_abs_box, pct_box = p_pct_box))
}
