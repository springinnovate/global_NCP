#' Heatmap of KS D with direction arrows
#' @param ks_aug Output of \code{augment_ks_with_direction()}.
#' @param title Plot title.
#' @param fill_limits Optional numeric length-2 for D scale (e.g., c(0, 0.35)).
#' @param palette Viridis option (A/B/C/D/E) or vector of colors.
#' @param reorder If TRUE, order services by mean D.
#' @export
plot_ks_heatmap <- function(ks_aug, title = "KS effect size (D) by service × variable",
                            fill_limits = c(0, NA), palette = "C", reorder = TRUE) {
  d <- ks_aug
  if (isTRUE(reorder)) {
    ord <- d |>
      dplyr::group_by(.data$service) |>
      dplyr::summarise(D_mean = mean(.data$D, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(.data$D_mean)) |>
      dplyr::pull(.data$service)
    d$service <- factor(d$service, levels = ord)
  }
  if (length(palette) == 1L) {
    fill_scale <- ggplot2::scale_fill_viridis_c(option = palette, limits = fill_limits, oob = scales::squish, name = "KS D")
  } else {
    fill_scale <- ggplot2::scale_fill_gradientn(colors = palette, limits = fill_limits, oob = scales::squish, name = "KS D")
  }
  ggplot2::ggplot(d, ggplot2::aes(service, var, fill = .data$D)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.15) +
    ggplot2::geom_text(ggplot2::aes(label = dplyr::if_else(is.na(.data$median_delta), "",
                                                           dplyr::if_else(.data$median_delta > 0, "↑", "↓"))),
                       size = 3, color = "white") +
    fill_scale +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = title,
                  subtitle = "Arrow = direction of median difference (hotspot vs non-hotspot)",
                  x = "Service", y = "Variable") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   panel.grid = ggplot2::element_blank())
}

#' Top-K services by KS D for a chosen variable
#' @export
plot_ks_bars <- function(ks_aug, var_id, top_k = 10, palette = "C") {
  d <- ks_aug |>
    dplyr::filter(.data$var == var_id) |>
    dplyr::arrange(dplyr::desc(.data$D)) |>
    dplyr::mutate(service = forcats::fct_reorder(.data$service, .data$D))
  p <- ggplot2::ggplot(head(d, top_k), ggplot2::aes(service, .data$D, fill = .data$D)) +
    ggplot2::geom_col() + ggplot2::coord_flip() +
    ggplot2::labs(title = paste0("Top ", top_k, " services by KS D — ", var_id),
                  x = NULL, y = "KS D")
  if (length(palette) == 1L) p + ggplot2::scale_fill_viridis_c(option = palette, guide = "none")
  else p + ggplot2::scale_fill_gradientn(colors = palette, guide = "none")
}

#' ECDF overlays for the top-K services (by D) for one variable
#' @param transform "identity" or "log1p" (applied to x for plotting only).
#' @export
plot_ecdf_grid <- function(hotspots_df, inverse_df, var_id, top_k = 4,
                           transform = c("identity","log1p")[2],
                           line_size = 0.9) {
  transform <- match.arg(transform)
  base <- dplyr::bind_rows(
    dplyr::mutate(hotspots_df, group = "hotspot"),
    dplyr::mutate(inverse_df,  group = "nonhotspot")
  ) |>
    dplyr::select(service, group, val = .data[[var_id]]) |>
    dplyr::filter(is.finite(.data$val))
  
  # pick top-K by D quickly
  find_D <- function(d) {
    x <- d$val[d$group=="hotspot"]; y <- d$val[d$group=="nonhotspot"]
    if (length(x) < 2 || length(y) < 2) return(NA_real_)
    suppressWarnings(as.numeric(stats::ks.test(x, y, exact = FALSE)$statistic))
  }
  top_services <- base |>
    dplyr::group_by(.data$service) |>
    dplyr::summarise(D = find_D(dplyr::cur_data_all()), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$D)) |>
    dplyr::slice_head(n = top_k) |>
    dplyr::pull(.data$service)
  
  dd <- dplyr::filter(base, .data$service %in% top_services)
  if (transform == "log1p") dd <- dplyr::mutate(dd, val = log1p(.data$val))
  
  ggplot2::ggplot(dd, ggplot2::aes(.data$val, color = .data$group)) +
    ggplot2::stat_ecdf(linewidth = line_size) +
    ggplot2::facet_wrap(~ service, scales = "free_x") +
    ggplot2::scale_color_manual(values = c(nonhotspot = "#2D708E", hotspot = "#D43D51"), guide = ggplot2::guide_legend(title = NULL)) +
    ggplot2::labs(title = paste("ECDF overlays —", var_id, if (transform=="log1p") "(log1p)" else ""),
                  x = var_id, y = "F(value)") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "top")
}

#' Mountain plot (CDF difference) for one service × variable
#' @export
plot_ks_mountain <- function(hotspots_df, inverse_df, service_id, var_id,
                             transform = c("identity","log1p")[2], n_grid = 512) {
  transform <- match.arg(transform)
  dd <- dplyr::bind_rows(
    dplyr::mutate(hotspots_df, group = "hotspot"),
    dplyr::mutate(inverse_df,  group = "nonhotspot")
  ) |>
    dplyr::filter(.data$service == service_id) |>
    dplyr::transmute(group, x = as.numeric(.data[[var_id]])) |>
    dplyr::filter(is.finite(.data$x))
  if (transform == "log1p") dd <- dplyr::mutate(dd, x = log1p(.data$x))
  if (nrow(dd) < 4) stop("Too few points for mountain plot")
  
  rng <- range(dd$x); grid <- seq(rng[1], rng[2], length.out = n_grid)
  ecdf_h <- stats::ecdf(dd$x[dd$group=="hotspot"])
  ecdf_n <- stats::ecdf(dd$x[dd$group=="nonhotspot"])
  delta  <- ecdf_h(grid) - ecdf_n(grid)
  D      <- max(abs(delta), na.rm = TRUE)
  
  tib <- tibble::tibble(x = grid, delta = delta)
  ggplot2::ggplot(tib, ggplot2::aes(.data$x, .data$delta)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey60") +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::labs(
      title = paste("Mountain plot:", service_id, "—", var_id),
      subtitle = paste0("Max |Δ| = D = ", signif(D, 3), "  (Δ = F_hot − F_non)"),
      x = if (transform=="log1p") paste0(var_id, " (log1p)") else var_id,
      y = expression(Delta~CDF)
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

