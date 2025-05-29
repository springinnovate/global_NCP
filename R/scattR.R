#' Create Scatterplot(s) Between ES % Change and LC Metric(s)
#'
#' @param df A data frame containing at least `pct_ch`, `service`, and the LC metric columns.
#' @param lc_metrics A character vector of LC metric column names to plot.
#' @param nam A tibble with two columns: `lc_metrics` and `names` (pretty labels for each metric).
#' @param geom_type Geometry type: one of 'hex', 'point', or 'density'.
#' @param bins Number of bins for geom_hex (default = 60).
#' @param service_filter Optional character vector of services to include (default = all).
#' @param export_dir Optional: if provided, plots will be saved as image files.
#' @param image_format Export format, e.g. 'jpg' or 'png'.
#' @param dpi Image resolution.
#' @param width Plot width.
#' @param height Plot height.
#' @param filter_note Optional subtitle to annotate filter used.
#' @param reverse_axes If TRUE, swaps X and Y axes (default = FALSE).
#' @param apply_log Logical. If TRUE, applies a log10 scale to the axis of the LC metric.
#' @param remove_outliers Logical. If TRUE, remove outlier values beyond 99.9 percentile for plotting.
#'
#' @return Plots are printed or saved, depending on `export_dir`.
plot_es_lc_scatter <- function(df, lc_metrics, nam,
                               geom_type = "hex", bins = 60,
                               service_filter = NULL,
                               export_dir = NULL,
                               image_format = "jpg",
                               dpi = 300, width = 10, height = 8,
                               filter_note = NULL,
                               reverse_axes = FALSE,
                               apply_log = FALSE,
                               remove_outliers = TRUE) {
  if (!is.null(service_filter)) {
    df <- df %>% filter(service %in% service_filter)
  }
  
  for (metric in lc_metrics) {
    label_row <- nam %>% filter(lc_metrics == metric)
    if (nrow(label_row) == 0) {
      warning(paste("Label for", metric, "not found in `nam`. Skipping."))
      next
    }
    
    short_label <- gsub("[^a-zA-Z0-9]", "_", label_row$lc_metrics)
    y_label <- label_row$names
    
    plot_data <- df %>% filter(!is.infinite(pct_ch), !is.infinite(.data[[metric]]))
    
    if (remove_outliers) {
      qx <- quantile(plot_data$pct_ch, probs = c(0.001, 0.999), na.rm = TRUE)
      qy <- quantile(plot_data[[metric]], probs = c(0.001, 0.999), na.rm = TRUE)
      plot_data <- plot_data %>% filter(
        pct_ch >= qx[1], pct_ch <= qx[2],
        .data[[metric]] >= qy[1], .data[[metric]] <= qy[2]
      )
    }
    
    aes_base <- if (reverse_axes) {
      aes(x = .data[[metric]], y = pct_ch)
    } else {
      aes(x = pct_ch, y = .data[[metric]])
    }
    
    fill_scale <- scale_fill_viridis_c(
      option = "D",
      direction = 1,
      oob = scales::squish,
      trans = "log10",
      name = "Count"
    )
    
    axis_scale <- if (reverse_axes) {
      scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()))
    } else {
      scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()))
    }
    
    p <- ggplot(plot_data, aes_base) + {
      if (geom_type == "hex") {
        geom_hex(bins = bins)
      } else if (geom_type == "point") {
        geom_point(alpha = 0.4, size = 1)
      } else if (geom_type == "density") {
        geom_density_2d_filled(contour_var = "ndensity")
      } else {
        stop("Invalid geom_type. Use 'hex', 'point', or 'density'.")
      }
    } +
      fill_scale +
      axis_scale +
      facet_wrap(~ service, scales = "free", ncol = 3) +
      labs(
        title = if (reverse_axes) {
          paste("% Change in ES vs.", y_label)
        } else {
          paste(y_label, "vs. % Change in ES")
        },
        subtitle = if (!is.null(filter_note) && filter_note != "No basin size filtering applied") filter_note else NULL,
        x = if (reverse_axes) y_label else "% Change in Ecosystem Service Provision, 1992–2020",
        y = if (reverse_axes) "% Change in Ecosystem Service Provision, 1992–2020" else y_label,
        fill = "Count"
      ) +
      theme(
        strip.text = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 10),
        axis.text = element_text(size = 9)
      )
    
    if (!is.null(export_dir)) {
      suffix <- if (!is.null(filter_note)) paste0("_", gsub("[^a-zA-Z0-9]", "_", filter_note)) else ""
      filename <- paste0(short_label, "_scatterplot", suffix, ".", image_format)
      filepath <- file.path(export_dir, filename)
      ggsave(filepath, plot = p, device = image_format, dpi = dpi,
             width = width, height = height)
    } else {
      print(p)
    }
  }
}

