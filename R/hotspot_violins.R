#' Faceted violins of hotspot change by grouping column
#'
#' @param df_long Long-form change table (`plt_long` style).
#' @param group_col Column name to facet by (string).
#' @param loss,gain Character vectors of services treated as losses / gains.
#' @param pct_cutoff Numeric cutoff (e.g., 0.05) passed to `extract_hotspots_by()`.
#' @param threshold_mode Threshold interpretation (passed through).
#' @param svc_order Optional service ordering; defaults to factor levels in `df_long`.
#' @param cut_q Trim quantile for violin plotting.
#' @param plot_n Max rows sampled for plotting (to keep files manageable).
#' @param keep_only_ordered Keep only services in `svc_order`.
#' @param out_root Directory root for plots (defaults to `out_plots()`).
#'
#' @return List with `abs_plot` and `pct_plot` invisibly; also writes PNGs.
#' @export
run_hotspot_violins_by <- function(
    df_long,
    group_col,
    loss,
    gain,
    pct_cutoff,
    threshold_mode,
    svc_order = NULL,
    cut_q = 0.999,
    plot_n = 300000L,
    keep_only_ordered = TRUE,
    out_root = out_plots()) {

  stopifnot(group_col %in% names(df_long))
  if (is.null(svc_order)) {
    svc_order <- levels(df_long$service)
    if (is.null(svc_order)) svc_order <- unique(df_long$service)
  }
  svc_order <- as.character(svc_order)

  message("==> Violins by: ", group_col)

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

  trim_df <- function(df, var) {
    df %>%
      dplyr::filter(!is.na(.data[[var]]), .data[[var]] != 0) %>%
      dplyr::group_by(service) %>%
      dplyr::mutate(
        lo = stats::quantile(.data[[var]], 1 - cut_q, na.rm = TRUE),
        hi = stats::quantile(.data[[var]], cut_q,     na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data[[var]] >= lo, .data[[var]] <= hi)
  }

  abs_trim <- trim_df(vals, "abs_chg")
  p_abs <- ggplot2::ggplot(abs_trim, ggplot2::aes(x = .data[[group_col]], y = abs_chg)) +
    ggplot2::geom_violin(trim = TRUE, scale = "width") +
    ggplot2::facet_wrap(~ service, scales = "free_y", ncol = 3) +
    ggplot2::labs(
      title    = paste0("Absolute change in hotspots by ", group_col),
      subtitle = "Violins only · NA/0 removed · per-service 99.9% trim",
      x = NULL, y = "Absolute change (service units)"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8, angle = 45, hjust = 1))

  pct_trim <- trim_df(vals, "pct_chg")
  p_pct <- ggplot2::ggplot(pct_trim, ggplot2::aes(x = .data[[group_col]], y = pct_chg)) +
    ggplot2::geom_violin(trim = TRUE, scale = "width") +
    ggplot2::facet_wrap(~ service, scales = "free_y", ncol = 3) +
    ggplot2::labs(
      title    = paste0("Percent change in hotspots by ", group_col),
      subtitle = "Violins only · NA/0 removed · per-service 99.9% trim",
      x = NULL, y = "Percent change (%)"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8, angle = 45, hjust = 1))

  dir_abs <- file.path(out_root, "abs", tolower(group_col))
  dir_pct <- file.path(out_root, "pct", tolower(group_col))
  dir.create(dir_abs, recursive = TRUE, showWarnings = FALSE)
  dir.create(dir_pct, recursive = TRUE, showWarnings = FALSE)

  ggplot2::ggsave(
    filename = file.path(dir_abs, paste0("violins_", tolower(group_col), "_abs.png")),
    plot = p_abs, width = 12, height = 8, dpi = 300, bg = "white"
  )
  ggplot2::ggsave(
    filename = file.path(dir_pct, paste0("violins_", tolower(group_col), "_pct.png")),
    plot = p_pct, width = 12, height = 8, dpi = 300, bg = "white"
  )

  invisible(list(abs_plot = p_abs, pct_plot = p_pct))
}
