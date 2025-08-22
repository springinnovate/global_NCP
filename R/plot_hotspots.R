# plot_hotspot_density_bin2d.R

#' Faceted hotspot vs non-hotspot density overlays (2D binned heatmaps)
#'
#' Draw per-service, faceted 2D density overlays where non-hotspots and hotspots
#' are two stat_bin2d layers with independent fill scales (via ggnewscale).
#' Supports quantile "stretch" globally (per service) or locally (per facet).
#'
#' @param plt_long Data frame of hotspot rows.
#' @param inverse_df Data frame of non-hotspot rows (complement).
#' @param socio_labels Named character vector mapping socio var names to facet labels.
#' @param drop_services Character vector of services to exclude.
#' @param trim_p Length-2 numeric quantiles for x trim per service (e.g., c(0.01,0.99)).
#' @param bg_bins,hs_bins Integer bin counts for non-hotspot / hotspot heatmaps.
#' @param bg_alpha,hs_alpha Opacity for each layer.
#' @param bg_trans,hs_trans Count transform ("identity","sqrt","log10").
#' @param bg_palette,hs_palette Color vectors for the two fill scales.
#' @param bg_fill_limits,hs_fill_limits Length-2 numeric. If limits_mode="quantile",
#'   values must be in [0,1] and are quantiles of bin counts. If "absolute",
#'   values are absolute count limits. Use NULL to auto.
#' @param limits_mode "quantile" or "absolute".
#' @param per_facet_stretch Logical. If TRUE, stretch per facet; else per service.
#' @param out_dir Output directory.
#' @param filename_suffix Suffix appended to filenames.
#' @param run_id Optional tag stamped into filenames and caption. If NULL, a timestamp is used.
#' @param overwrite Logical. If FALSE and a file exists, a numeric suffix is added.
#' @param width,height,dpi PNG size/resolution.
#' @param use_ragg Logical. Use ragg::agg_png for writing PNGs.
#' @param hide_inline Logical. Hide inline figures when knitting.
#' @param print_interactive Logical. Print plots when interactive and not knitting.
#' @return Invisibly, character vector of written file paths.
#' @export
plot_hotspot_density_bin2d <- function(
    plt_long,
    inverse_df,
    socio_labels,
    drop_services = NULL,
    trim_p = c(0.01, 0.99),
    bg_bins = 80, hs_bins = 50,
    bg_alpha = 0.85, hs_alpha = 0.90,
    bg_trans = "sqrt", hs_trans = "sqrt",
    bg_palette = viridisLite::viridis(9, option = "C"),
    hs_palette = c("#FFE9E9", "#FFA3A3", "#FF5B5B", "#D40000"),
    bg_fill_limits = c(0.02, 0.98),
    hs_fill_limits = c(0.02, 0.98),
    limits_mode = c("quantile", "absolute"),
    per_facet_stretch = FALSE,
    out_dir = "output_charts",
    filename_suffix = "_two_scales_bin2d.png",
    run_id = NULL,
    overwrite = FALSE,
    width = 10, height = 6, dpi = 300,
    use_ragg = TRUE,
    hide_inline = TRUE,
    print_interactive = TRUE
) {
  limits_mode <- match.arg(limits_mode)
  socio_vars <- names(socio_labels)
  
  if (hide_inline && isTRUE(getOption("knitr.in.progress"))) {
    knitr::opts_chunk$set(fig.show = "hide")
  }
  
  if (!is.null(drop_services) && length(drop_services)) {
    inverse_df <- dplyr::filter(inverse_df, !.data$service %in% drop_services)
  }
  
  hot_df  <- dplyr::mutate(plt_long,   hotspot_flag = 1L)
  non_df  <- dplyr::mutate(inverse_df, hotspot_flag = 0L)
  both_df <- dplyr::bind_rows(hot_df, non_df)
  
  both_long <- both_df %>%
    tidyr::pivot_longer(
      cols = tidyselect::all_of(socio_vars),
      names_to = "socio_var",
      values_to = "socio_val"
    ) %>%
    dplyr::mutate(
      pct_chg      = as.numeric(.data$pct_chg),
      socio_val    = as.numeric(.data$socio_val),
      hotspot_flag = factor(.data$hotspot_flag, levels = c(0,1),
                            labels = c("Non-hotspot","Hotspot"))
    ) %>%
    dplyr::filter(is.finite(.data$pct_chg), is.finite(.data$socio_val)) %>%
    dplyr::group_by(.data$service) %>%
    dplyr::mutate(
      pct_low  = stats::quantile(.data$pct_chg, trim_p[1], na.rm = TRUE),
      pct_high = stats::quantile(.data$pct_chg, trim_p[2], na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$pct_chg >= .data$pct_low, .data$pct_chg <= .data$pct_high) %>%
    dplyr::mutate(socio_label = socio_labels[as.character(.data$socio_var)])
  
  drop_flat_facets <- function(df) {
    df %>%
      dplyr::group_by(.data$socio_label) %>%
      dplyr::filter(dplyr::n() > 1,
                    dplyr::n_distinct(.data$pct_chg)   > 1,
                    dplyr::n_distinct(.data$socio_val) > 1) %>%
      dplyr::ungroup()
  }
  
  safe_name <- function(x) {
    x <- gsub("[^A-Za-z0-9_\\-]+", "_", x)
    gsub("_+", "_", x)
  }
  
  make_unique_path <- function(path) {
    if (overwrite || !file.exists(path)) return(path)
    base <- sub("(.*)(\\.[^.]+)$", "\\1", path)
    ext  <- sub(".*(\\.[^.]+)$", "\\1", path)
    i <- 1L
    repeat {
      cand <- paste0(base, "-", i, ext)
      if (!file.exists(cand)) return(cand)
      i <- i + 1L
    }
  }
  
  compute_bin_limits_global <- function(df, bins, qpair, mode) {
    if (is.null(qpair)) return(NULL)
    stopifnot(length(qpair) == 2)
    if (mode == "absolute") return(sort(as.numeric(qpair)))
    
    rx <- range(df$pct_chg,  finite = TRUE)
    ry <- range(df$socio_val, finite = TRUE)
    if (!all(is.finite(rx)) || !all(is.finite(ry)) || diff(rx) <= 0 || diff(ry) <= 0) return(NULL)
    bx <- seq(rx[1], rx[2], length.out = bins + 1L)
    by <- seq(ry[1], ry[2], length.out = bins + 1L)
    ix <- pmax(1L, pmin(bins, findInterval(df$pct_chg,  bx, all.inside = TRUE)))
    iy <- pmax(1L, pmin(bins, findInterval(df$socio_val, by, all.inside = TRUE)))
    counts <- as.integer(table(factor(ix, levels = 1:bins),
                               factor(iy, levels = 1:bins)))
    nz <- counts[counts > 0L]
    if (length(nz) == 0L) return(NULL)
    stats::quantile(nz, probs = sort(pmax(0, pmin(1, as.numeric(qpair)))), names = FALSE, na.rm = TRUE)
  }
  
  # safer default run_id that doesn't require digest
  if (is.null(run_id)) {
    run_id <- paste0("run", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  services <- unique(both_long$service)
  out_files <- character(0)
  
  for (svc in services) {
    plot_data <- both_long %>% dplyr::filter(.data$service == svc) %>% drop_flat_facets()
    if (nrow(plot_data) == 0) next
    
    bg_data <- dplyr::filter(plot_data, .data$hotspot_flag == "Non-hotspot")
    fg_data <- dplyr::filter(plot_data, .data$hotspot_flag == "Hotspot")
    
    if (!per_facet_stretch) {
      # global/service stretch
      bg_limits <- compute_bin_limits_global(bg_data, bg_bins, bg_fill_limits, limits_mode)
      hs_limits <- compute_bin_limits_global(fg_data, hs_bins, hs_fill_limits, limits_mode)
      
      p <- ggplot2::ggplot(NULL, ggplot2::aes(x = .data$pct_chg, y = .data$socio_val)) +
        ggplot2::stat_bin2d(
          data  = bg_data, bins = bg_bins,
          ggplot2::aes(fill = after_stat(count)), alpha = bg_alpha, na.rm = TRUE
        ) +
        ggplot2::scale_fill_gradientn(
          colors = bg_palette, trans = bg_trans, limits = bg_limits,
          oob = scales::squish, name = "Non-hotspot density"
        ) +
        ggnewscale::new_scale_fill() +
        ggplot2::stat_bin2d(
          data  = fg_data, bins = hs_bins,
          ggplot2::aes(fill = after_stat(count)), alpha = hs_alpha, na.rm = TRUE
        ) +
        ggplot2::scale_fill_gradientn(
          colors = hs_palette, trans = hs_trans, limits = hs_limits,
          oob = scales::squish, name = "Hotspot density"
        )
    } else {
      # per-facet stretch via pre-binned tiles
      empty_tiles <- tibble::tibble(
        x = numeric(0), y = numeric(0), count = integer(0),
        socio_label = character(0), width = numeric(0), height = numeric(0),
        fill = numeric(0)
      )
      
      bin_to_tiles <- function(df, bins, qpair, mode) {
        if (!"socio_label" %in% names(df)) return(empty_tiles)
        if (nrow(df) == 0) return(empty_tiles)
        df %>%
          dplyr::group_by(.data$socio_label) %>%
          dplyr::group_modify(function(d, ...) {
            rx <- range(d$pct_chg,  finite = TRUE)
            ry <- range(d$socio_val, finite = TRUE)
            if (!all(is.finite(rx)) || !all(is.finite(ry)) || diff(rx) <= 0 || diff(ry) <= 0) return(empty_tiles[0,])
            bx <- seq(rx[1], rx[2], length.out = bins + 1L)
            by <- seq(ry[1], ry[2], length.out = bins + 1L)
            ix <- pmax(1L, pmin(bins, findInterval(d$pct_chg,  bx, all.inside = TRUE)))
            iy <- pmax(1L, pmin(bins, findInterval(d$socio_val, by, all.inside = TRUE)))
            tab <- as.matrix(table(factor(ix, levels = 1:bins),
                                   factor(iy, levels = 1:bins)))
            xmid <- (bx[-1] + bx[-length(bx)]) / 2
            ymid <- (by[-1] + by[-length(by)]) / 2
            grid <- expand.grid(x = xmid, y = ymid)
            grid$count <- as.vector(tab)
            
            # per-facet limits
            if (is.null(qpair)) {
              nz <- grid$count[grid$count > 0]
              if (length(nz) == 0) return(empty_tiles[0,])
              lo <- min(nz); hi <- max(grid$count)
            } else if (mode == "absolute") {
              lims <- sort(as.numeric(qpair)); lo <- lims[1]; hi <- lims[2]
            } else {
              nz <- grid$count[grid$count > 0]
              if (length(nz) == 0) return(empty_tiles[0,])
              qs <- stats::quantile(nz, probs = sort(pmax(0, pmin(1, as.numeric(qpair)))),
                                    names = FALSE, na.rm = TRUE)
              lo <- qs[1]; hi <- qs[2]
            }
            
            fill <- (grid$count - lo) / (hi - lo + 1e-9)
            fill <- pmax(0, pmin(1, fill))
            
            tibble::tibble(
              x = grid$x, y = grid$y, count = grid$count,
              socio_label = d$socio_label[1],
              width = diff(bx)[1], height = diff(by)[1],
              fill = fill
            )
          }) %>%
          dplyr::ungroup()
      }
      
      bg_tiles <- bin_to_tiles(bg_data, bg_bins, bg_fill_limits, limits_mode)
      fg_tiles <- bin_to_tiles(fg_data, hs_bins, hs_fill_limits, limits_mode)
      
      p <- ggplot2::ggplot(NULL)
      if (nrow(bg_tiles) > 0) {
        p <- p +
          ggplot2::geom_tile(
            data = bg_tiles,
            ggplot2::aes(x = .data$x, y = .data$y, fill = .data$fill,
                         width = .data$width, height = .data$height),
            alpha = bg_alpha
          ) +
          ggplot2::scale_fill_gradientn(
            colors = bg_palette, limits = c(0,1), oob = scales::squish,
            name = "Non-hotspot (rel.)"
          )
      }
      if (nrow(fg_tiles) > 0) {
        p <- p +
          ggnewscale::new_scale_fill() +
          ggplot2::geom_tile(
            data = fg_tiles,
            ggplot2::aes(x = .data$x, y = .data$y, fill = .data$fill,
                         width = .data$width, height = .data$height),
            alpha = hs_alpha
          ) +
          ggplot2::scale_fill_gradientn(
            colors = hs_palette, limits = c(0,1), oob = scales::squish,
            name = "Hotspot (rel.)"
          )
      }
      if (nrow(bg_tiles) == 0 && nrow(fg_tiles) == 0) {
        next  # nothing to draw for this service
      }
    }
    
    spec_caption <- paste0(
      "id=", ifelse(is.null(run_id), "", run_id),
      " | trim=", paste(trim_p, collapse = ","),
      " | mode=", limits_mode,
      " | stretch=", if (per_facet_stretch) "facet" else "service",
      " | bg_bins=", bg_bins, " hs_bins=", hs_bins
    )
    
    p <- p +
      ggplot2::facet_wrap(~ socio_label, scales = "free_y", nrow = 2) +
      ggplot2::labs(
        title = paste("Hotspot vs non-hotspot density:", svc),
        x = "Ecosystem service % change",
        y = "Socioeconomic variable",
        caption = spec_caption
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        strip.text      = ggplot2::element_text(face = "bold"),
        plot.title      = ggplot2::element_text(hjust = 0.5),
        axis.text       = ggplot2::element_text(size = 9),
        legend.position = "right",
        plot.caption    = ggplot2::element_text(size = 7, color = "grey30")
      )
    
    if (is.null(run_id)) {
      file_tag <- "untagged"
    } else {
      file_tag <- run_id
    }
    png_file <- file.path(out_dir, paste0(safe_name(svc), "_", file_tag, filename_suffix))
    png_file <- make_unique_path(png_file)
    
    if (use_ragg) {
      ggplot2::ggsave(
        filename = png_file, plot = p,
        width = width, height = height, dpi = dpi, bg = "white",
        device = ragg::agg_png
      )
    } else {
      ggplot2::ggsave(
        filename = png_file, plot = p,
        width = width, height = height, dpi = dpi, bg = "white"
      )
    }
    
    if (print_interactive && interactive() && !isTRUE(getOption("knitr.in.progress"))) print(p)
    
    out_files <- c(out_files, png_file)
    rm(p); gc()
  }
  
  invisible(out_files)
}
