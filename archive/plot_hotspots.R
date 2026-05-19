# plot_hotspot_density_bin2d.R

#' Faceted hotspot vs non-hotspot density overlays (2D binned heatmaps)
#'
#' Draw per-service, faceted 2D density overlays using `stat_bin2d`. You can:
#' - plot both layers together (separate fill scales via ggnewscale), or
#' - plot only hotspots, or only non-hotspots (with customizable palette).
#' Supports quantile "stretch" globally (per service) or locally (per facet).
#'
#' @param hotspots_df Data frame of hotspot rows (long format).
#' @param inverse_df  Data frame of non-hotspot (complement) rows.
#' @param socio_labels Named character vector: raw socio var -> facet label.
#' @param which_layers One of "both","hotspots","nonhotspots".
#' @param drop_services Character vector of services to exclude in both inputs.
#' @param trim_p Length-2 numeric, pct_chg quantile trim per service (x-axis).
#' @param y_trim_p NULL or length-2 numeric, optional socio_val quantile trim per service (y-axis).
#' @param bg_bins,hs_bins Integer bin counts for non-hotspot / hotspot heatmaps.
#' @param bg_alpha,hs_alpha Opacity for each layer (0–1).
#' @param bg_trans,hs_trans Count transform for fill ("identity","sqrt","log10").
#'
#' @param bg_palette,hs_palette,single_palette Color palettes (character vectors).
#'   If `NULL`, palettes are generated from viridis options below.
#' @param bg_palette_option,hs_palette_option,single_palette_option Viridis options
#'   ("A"=magma,"B"=inferno,"C"=plasma,"D"=viridis,"E"=cividis). Used when the
#'   corresponding `*_palette` is `NULL`. Defaults: bg="C", hs="B", single="D".
#' @param palette_n Number of colors to generate when using viridis options.
#' @param bg_reverse,hs_reverse,single_reverse Reverse palettes?
#'
#' @param bg_fill_limits,hs_fill_limits Length-2 numeric. If limits_mode="quantile",
#'   values are quantiles in [0,1]; if "absolute", absolute count limits. Use NULL to auto.
#' @param single_fill_limits Length-2 numeric for single-layer stretch (same semantics).
#' @param single_limits_mode "quantile" or "absolute" for single-layer stretch.
#' @param limits_mode "quantile" or "absolute" for the two-layer ("both") case.
#' @param per_facet_stretch Logical. If TRUE, stretch per facet; else per service.
#'
#' @param hs_contours Logical; if TRUE, draw hotspot `stat_density_2d` contours to
#'   improve contrast. Guarded to only draw when enough points.
#' @param hs_contour_color,hs_contour_size Contour styling.
#'
#' @param out_dir Output directory; created if missing.
#' @param filename_suffix File suffix for images.
#' @param run_id Optional tag stamped into filenames/caption (else timestamp).
#' @param overwrite If FALSE, makes a unique filename on collision.
#' @param width,height,dpi PNG size/resolution.
#' @param use_ragg Use ragg::agg_png device for reliability.
#' @param hide_inline Hide inline figures when knitting.
#' @param print_interactive Print plots when interactive and not knitting.
#'
#' @return Invisibly, character vector of written file paths.
#' @export
plot_hotspot_density_bin2d <- function(
    hotspots_df,
    inverse_df,
    socio_labels,
    which_layers = c("both","hotspots","nonhotspots"),
    drop_services = NULL,
    trim_p  = c(0.01, 0.99),
    y_trim_p = NULL,
    bg_bins = 80, hs_bins = 50,
    bg_alpha = 0.70, hs_alpha = 0.95,
    bg_trans = "sqrt", hs_trans = "sqrt",
    
    bg_palette = NULL,
    hs_palette = NULL,
    single_palette = NULL,
    bg_palette_option     = "C",  # plasma (cool)
    hs_palette_option     = "B",  # inferno (warm) -> contrasts nicely
    single_palette_option = "D",  # viridis
    palette_n = 256,
    bg_reverse = FALSE, hs_reverse = FALSE, single_reverse = FALSE,
    
    bg_fill_limits = c(0.02, 0.98),
    hs_fill_limits = c(0.02, 0.98),
    single_fill_limits = c(0.02, 0.98),
    single_limits_mode = c("quantile","absolute"),
    limits_mode = c("quantile","absolute"),
    per_facet_stretch = FALSE,
    
    hs_contours = FALSE,
    hs_contour_color = "#222222",
    hs_contour_size  = 0.25,
    
    out_dir = "output_charts",
    filename_suffix = "_two_scales_bin2d.png",
    run_id = NULL,
    overwrite = FALSE,
    width = 10, height = 6, dpi = 300,
    use_ragg = TRUE,
    hide_inline = TRUE,
    print_interactive = TRUE
) {
  which_layers       <- match.arg(which_layers)
  limits_mode        <- match.arg(limits_mode)
  single_limits_mode <- match.arg(single_limits_mode)
  socio_vars <- names(socio_labels)
  
  if (hide_inline && isTRUE(getOption("knitr.in.progress"))) {
    knitr::opts_chunk$set(fig.show = "hide")
  }
  
  # Drop services in BOTH inputs
  if (!is.null(drop_services) && length(drop_services)) {
    hotspots_df <- dplyr::filter(hotspots_df, !.data$service %in% drop_services)
    inverse_df  <- dplyr::filter(inverse_df,  !.data$service %in% drop_services)
  }
  
  # Palette resolver -------------------------------------------------------
  resolve_palette <- function(p, opt, n, rev = FALSE) {
    if (!is.null(p)) return(p)
    pal <- viridisLite::viridis(n, option = opt, direction = if (rev) -1 else 1)
    pal
  }
  bg_pal     <- resolve_palette(bg_palette,     bg_palette_option,     palette_n, bg_reverse)
  hs_pal     <- resolve_palette(hs_palette,     hs_palette_option,     palette_n, hs_reverse)
  single_pal <- resolve_palette(single_palette, single_palette_option, palette_n, single_reverse)
  
  save_png_safe <- function(file, plot, width, height, dpi, bg = "white", use_ragg = TRUE) {
    ok <- FALSE
    if (use_ragg && requireNamespace("ragg", quietly = TRUE)) {
      try({
        ggplot2::ggsave(file, plot, width = width, height = height, dpi = dpi, bg = bg,
                        device = ragg::agg_png)
        ok <- TRUE
      }, silent = TRUE)
    }
    if (!ok) {
      ggplot2::ggsave(file, plot, width = width, height = height, dpi = dpi, bg = bg,
                      device = "png")
    }
  }
  
  # tag & combine
  hot_df  <- dplyr::mutate(hotspots_df, hotspot_flag = 1L)
  non_df  <- dplyr::mutate(inverse_df,  hotspot_flag = 0L)
  both_df <- dplyr::bind_rows(hot_df, non_df)
  
  # long + numeric + trims
  both_long <- both_df %>%
    tidyr::pivot_longer(
      cols = tidyselect::all_of(socio_vars),
      names_to = "socio_var", values_to = "socio_val"
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
    ) %>% dplyr::ungroup() %>%
    dplyr::filter(.data$pct_chg >= .data$pct_low, .data$pct_chg <= .data$pct_high) %>%
    dplyr::mutate(socio_label = socio_labels[as.character(.data$socio_var)])
  
  # optional y-trim (per service)
  if (!is.null(y_trim_p)) {
    both_long <- both_long %>%
      dplyr::group_by(.data$service, .data$socio_label) %>%
      dplyr::mutate(
        y_low  = stats::quantile(.data$socio_val, y_trim_p[1], na.rm = TRUE),
        y_high = stats::quantile(.data$socio_val, y_trim_p[2], na.rm = TRUE)
      ) %>% dplyr::ungroup() %>%
      dplyr::filter(.data$socio_val >= .data$y_low, .data$socio_val <= .data$y_high)
  }
  
  # helpers ---------------------------------------------------------------
  drop_flat_facets <- function(df) {
    df %>%
      dplyr::group_by(.data$socio_label) %>%
      dplyr::filter(dplyr::n() > 1,
                    dplyr::n_distinct(.data$pct_chg)   > 1,
                    dplyr::n_distinct(.data$socio_val) > 1) %>%
      dplyr::ungroup()
  }
  safe_name <- function(x) {
    x <- gsub("[^A-Za-z0-9_\\-]+", "_", x); gsub("_+", "_", x)
  }
  make_unique_path <- function(path) {
    if (overwrite || !file.exists(path)) return(path)
    base <- sub("(.*)(\\.[^.]+)$", "\\1", path); ext <- sub(".*(\\.[^.]+)$", "\\1", path)
    i <- 1L; repeat { cand <- paste0(base, "-", i, ext); if (!file.exists(cand)) return(cand); i <- i + 1L }
  }
  compute_bin_limits_global <- function(df, bins, qpair, mode) {
    if (is.null(qpair)) return(NULL)
    stopifnot(length(qpair) == 2)
    if (mode == "absolute") return(sort(as.numeric(qpair)))
    rx <- range(df$pct_chg,  finite = TRUE); ry <- range(df$socio_val, finite = TRUE)
    if (!all(is.finite(rx)) || !all(is.finite(ry)) || diff(rx) <= 0 || diff(ry) <= 0) return(NULL)
    bx <- seq(rx[1], rx[2], length.out = bins + 1L); by <- seq(ry[1], ry[2], length.out = bins + 1L)
    ix <- pmax(1L, pmin(bins, findInterval(df$pct_chg,  bx, all.inside = TRUE)))
    iy <- pmax(1L, pmin(bins, findInterval(df$socio_val, by, all.inside = TRUE)))
    counts <- as.integer(table(factor(ix, levels = 1:bins), factor(iy, levels = 1:bins)))
    nz <- counts[counts > 0L]; if (length(nz) == 0L) return(NULL)
    stats::quantile(nz, probs = sort(pmax(0, pmin(1, as.numeric(qpair)))), names = FALSE, na.rm = TRUE)
  }
  
  # per-facet binning helper
  empty_tiles <- tibble::tibble(
    x = numeric(0), y = numeric(0), count = integer(0),
    width = numeric(0), height = numeric(0), fill = numeric(0)
  )
  bin_to_tiles <- function(df, bins, qpair, mode) {
    if (!"socio_label" %in% names(df) || nrow(df) == 0) return(empty_tiles)
    df %>%
      dplyr::group_by(.data$socio_label) %>%
      dplyr::group_modify(function(d, ...) {
        rx <- range(d$pct_chg,  finite = TRUE); ry <- range(d$socio_val, finite = TRUE)
        if (!all(is.finite(rx)) || !all(is.finite(ry)) || diff(rx) <= 0 || diff(ry) <= 0) return(empty_tiles[0,])
        bx <- seq(rx[1], rx[2], length.out = bins + 1L); by <- seq(ry[1], ry[2], length.out = bins + 1L)
        ix <- pmax(1L, pmin(bins, findInterval(d$pct_chg,  bx, all.inside = TRUE)))
        iy <- pmax(1L, pmin(bins, findInterval(d$socio_val, by, all.inside = TRUE)))
        tab  <- as.matrix(table(factor(ix, levels = 1:bins), factor(iy, levels = 1:bins)))
        xmid <- (bx[-1] + bx[-length(bx)]) / 2; ymid <- (by[-1] + by[-length(by)]) / 2
        grid <- expand.grid(x = xmid, y = ymid); counts <- as.vector(tab)
        
        if (is.null(qpair)) {
          nz <- counts[counts > 0L]; if (length(nz) == 0L) return(empty_tiles[0,])
          lo <- min(nz); hi <- max(counts)
        } else if (mode == "absolute") {
          lims <- sort(as.numeric(qpair)); lo <- lims[1]; hi <- lims[2]
        } else {
          nz <- counts[counts > 0L]; if (length(nz) == 0L) return(empty_tiles[0,])
          qs <- stats::quantile(nz, probs = sort(pmax(0, pmin(1, as.numeric(qpair)))), names = FALSE, na.rm = TRUE)
          lo <- qs[1]; hi <- qs[2]
        }
        fill <- (counts - lo) / (hi - lo + 1e-9); fill <- pmax(0, pmin(1, fill))
        tibble::tibble(x = grid$x, y = grid$y, count = as.integer(counts),
                       width = diff(bx)[1], height = diff(by)[1], fill = fill)
      }) %>% dplyr::ungroup()
  }
  
  if (is.null(run_id)) run_id <- paste0("run", format(Sys.time(), "%Y%m%d_%H%M%S"))
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  services  <- unique(both_long$service)
  out_files <- character(0)
  
  for (svc in services) {
    plot_data <- both_long %>% dplyr::filter(.data$service == svc) %>% drop_flat_facets()
    if (nrow(plot_data) == 0) next
    
    bg_data <- dplyr::filter(plot_data, .data$hotspot_flag == "Non-hotspot")
    fg_data <- dplyr::filter(plot_data, .data$hotspot_flag == "Hotspot")
    
    use_bg <- which_layers %in% c("both","nonhotspots")
    use_fg <- which_layers %in% c("both","hotspots")
    
    if (!per_facet_stretch) {
      p <- ggplot2::ggplot(NULL, ggplot2::aes(x = .data$pct_chg, y = .data$socio_val))
      
      if (use_bg) {
        lims_bg <- compute_bin_limits_global(bg_data, bg_bins,
                                             if (which_layers=="both") bg_fill_limits else single_fill_limits,
                                             if (which_layers=="both") limits_mode       else single_limits_mode)
        p <- p +
          ggplot2::stat_bin2d(
            data  = bg_data, bins = bg_bins,
            ggplot2::aes(fill = after_stat(count)), alpha = bg_alpha, na.rm = TRUE
          ) +
          ggplot2::scale_fill_gradientn(
            colors = if (which_layers=="both") bg_pal else single_pal,
            trans  = if (which_layers=="both") bg_trans else bg_trans,
            limits = lims_bg, oob = scales::squish,
            name   = if (which_layers=="both") "Non-hotspot density" else
              if (which_layers=="nonhotspots") "Non-hotspot density" else "Density"
          )
      }
      
      if (use_fg) {
        if (use_bg) p <- p + ggnewscale::new_scale_fill()
        lims_fg <- compute_bin_limits_global(fg_data, hs_bins,
                                             if (which_layers=="both") hs_fill_limits else single_fill_limits,
                                             if (which_layers=="both") limits_mode       else single_limits_mode)
        p <- p +
          ggplot2::stat_bin2d(
            data  = fg_data, bins = hs_bins,
            ggplot2::aes(fill = after_stat(count)), alpha = hs_alpha, na.rm = TRUE
          ) +
          ggplot2::scale_fill_gradientn(
            colors = if (which_layers=="both") hs_pal else single_pal,
            trans  = if (which_layers=="both") hs_trans else hs_trans,
            limits = lims_fg, oob = scales::squish,
            name   = if (which_layers=="both") "Hotspot density" else
              if (which_layers=="hotspots") "Hotspot density" else "Density"
          )
        
        # Optional hotspot contours to pop the signal
        if (hs_contours && nrow(fg_data) >= 100 &&
            dplyr::n_distinct(fg_data$pct_chg) > 1L &&
            dplyr::n_distinct(fg_data$socio_val) > 1L) {
          p <- p + ggplot2::stat_density_2d(
            data = fg_data,
            ggplot2::aes(x = .data$pct_chg, y = .data$socio_val),
            color = hs_contour_color, linewidth = hs_contour_size, bins = 6
          )
        }
      }
      
    } else {
      # per-facet stretch via tiles
      p <- ggplot2::ggplot(NULL)
      if (use_bg) {
        bg_tiles <- bin_to_tiles(bg_data, bg_bins,
                                 if (which_layers=="both") bg_fill_limits else single_fill_limits,
                                 if (which_layers=="both") limits_mode       else single_limits_mode)
        if (nrow(bg_tiles) > 0) {
          p <- p +
            ggplot2::geom_tile(
              data = bg_tiles,
              ggplot2::aes(x = .data$x, y = .data$y, fill = .data$fill,
                           width = .data$width, height = .data$height),
              alpha = bg_alpha
            ) +
            ggplot2::scale_fill_gradientn(
              colors = if (which_layers=="both") bg_pal else single_pal,
              limits = c(0,1), oob = scales::squish,
              name   = if (which_layers=="both") "Non-hotspot (rel.)" else "Density (rel.)"
            )
        }
      }
      if (use_fg) {
        fg_tiles <- bin_to_tiles(fg_data, hs_bins,
                                 if (which_layers=="both") hs_fill_limits else single_fill_limits,
                                 if (which_layers=="both") limits_mode       else single_limits_mode)
        if (nrow(fg_tiles) > 0) {
          if (use_bg) p <- p + ggnewscale::new_scale_fill()
          p <- p +
            ggplot2::geom_tile(
              data = fg_tiles,
              ggplot2::aes(x = .data$x, y = .data$y, fill = .data$fill,
                           width = .data$width, height = .data$height),
              alpha = hs_alpha
            ) +
            ggplot2::scale_fill_gradientn(
              colors = if (which_layers=="both") hs_pal else single_pal,
              limits = c(0,1), oob = scales::squish,
              name   = if (which_layers=="both") "Hotspot (rel.)" else "Density (rel.)"
            )
          
          if (hs_contours && nrow(fg_data) >= 100 &&
              dplyr::n_distinct(fg_data$pct_chg) > 1L &&
              dplyr::n_distinct(fg_data$socio_val) > 1L) {
            p <- p + ggplot2::stat_density_2d(
              data = fg_data,
              ggplot2::aes(x = .data$pct_chg, y = .data$socio_val),
              color = hs_contour_color, linewidth = hs_contour_size, bins = 6
            )
          }
        }
      }
      if (length(p$layers) == 0) next
    }
    
    spec_caption <- paste0(
      "id=", run_id %||% "",
      " | layers=", which_layers,
      " | trim=", paste(trim_p, collapse = ","),
      if (!is.null(y_trim_p)) paste0(" | ytrim=", paste(y_trim_p, collapse=",")) else "",
      " | mode=", if (which_layers=="both") limits_mode else single_limits_mode,
      " | stretch=", if (per_facet_stretch) "facet" else "service",
      " | bins=", if (which_layers %in% c("both","nonhotspots")) bg_bins else hs_bins
    )
    
    p <- p +
      ggplot2::facet_wrap(~ socio_label, scales = "free_y", nrow = 2) +
      ggplot2::labs(
        title = paste(
          c("Non-hotspots","Hotspots","Hotspot vs non-hotspot density")
          [match(which_layers, c("nonhotspots","hotspots","both"))], ":", svc
        ),
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
    
    tag <- paste0(run_id, "_", which_layers)
    png_file <- file.path(out_dir, paste0(safe_name(svc), "_", tag, filename_suffix))
    png_file <- make_unique_path(png_file)
    
    save_png_safe(png_file, p, width = width, height = height, dpi = dpi, bg = "white", use_ragg = use_ragg)
    if (print_interactive && interactive() && !isTRUE(getOption("knitr.in.progress"))) print(p)
    
    out_files <- c(out_files, png_file)
    rm(p); gc()
  }
  
  invisible(out_files)
}

# tiny null-coalescing helper
`%||%` <- function(x, y) if (is.null(x)) y else x

