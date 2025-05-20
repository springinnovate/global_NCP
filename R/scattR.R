#' Create Scatterplot(s) Between ES % Change and LC Metric(s)
#'
#' @param df A data frame containing at least `pct_ch`, `service`, and the LC metric columns.
#' @param lc_metrics A character vector of LC metric column names to plot.
#' @param service_filter Optional: a character vector of service names to include (default is all).
#' @param bins Number of bins for geom_hex (default is 60).
#' @param export_path Optional path to a PDF file to export plots. If NULL, plots are shown interactively.
#'
#' @return Plots are printed to screen or saved to file if `export_path` is provided.
plot_es_lc_scatter <- function(df, lc_metrics, nam,
                               geom_type = "hex", bins = 60,
                               service_filter = NULL,
                               export_dir = here::here("output_charts"),
                               image_format = "jpg",
                               dpi = 300, width = 10, height = 8,
                               filter_note = NULL) {
  # Optional service filter
  if (!is.null(service_filter)) {
    df <- df %>% filter(service %in% service_filter)
  }
  
  for (metric in lc_metrics) {
    # Get y-axis label and short name
    label_row <- nam %>% filter(lc_metrics == metric)
    if (nrow(label_row) == 0) {
      warning(paste("Label for", metric, "not found in `nam`. Skipping."))
      next
    }
    
    short_label <- gsub("[^a-zA-Z0-9]", "_", label_row$lc_metrics)
    y_label <- label_row$names
    
    # Build plot
    p <- ggplot(df, aes(x = pct_ch, y = .data[[metric]])) +
      {
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
      scale_fill_viridis_c(
        option = "D",
        direction = 1,
        oob = scales::squish,
        limits = quantile(df$pct_ch, c(0.02, 0.98), na.rm = TRUE),
        name = "Density"
      ) +
      facet_wrap(~ service, scales = "free", ncol = 3) +
      labs(
        title = paste(y_label, "vs. % Change in ES"),
        subtitle = filter_note,
        x = "% Change in Ecosystem Service Provision, 1992–2020",
        y = y_label,
        fill = "Density"
      ) +
      theme(
        strip.text = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 10),
        axis.text = element_text(size = 9)
      )
    
    # Export one image per plot
    if (!is.null(export_dir)) {
      # Sanitize filter_note for filename if provided
      filter_suffix <- if (!is.null(filter_note)) {
        paste0("_", gsub("[^a-zA-Z0-9]", "_", filter_note))
      } else {
        ""
      }
      
      filename <- paste0(short_label, "_scatterplot", filter_suffix, ".", image_format)
      filepath <- file.path(export_dir, filename)
      
      ggsave(filepath, plot = p, device = image_format, dpi = dpi,
             width = width, height = height)
    } else {
      print(p)
    }
  }
}
