# ==============================================================================
# Hotspot Magnitude Boxplots (per grouping: income_grp, region_wb, WWF_biome, nev_name)
# ==============================================================================
#
# Centralized 2026-09-02 out of analysis/hotspot_extraction.qmd's inline boxplot chunk. That
# qmd's own render is slow (it's the long per-country-export document), so any pure plotting/
# style change to these figures previously required a full re-render just to see the result.
# This function reads the same plt_long.rds that hotspot_extraction.qmd already writes to disk,
# so a standalone run (see run_hotspot_boxplots.R) takes a fraction of the time. hotspot_
# extraction.qmd now sources this file and calls generate_hotspot_boxplots() with its own
# in-memory plt_long/HOTS_CFG instead of duplicating the plotting logic inline -- single source
# of truth, not a second copy to silently drift (see docs/runbook.md's service-config-drift
# section for why that risk is real on this project specifically).
#
# Legend redesign, 2026-09-02: dropped the old per-group color-swatch legend (an invisible
# geom_point() alpha=0 hack) in favor of make_key_panel()'s plain-text key (R/plotting_functions.R).
# Also dropped the in-plot title/subtitle -- these figures are always embedded with an external
# Quarto figure caption in the paper, so the title just duplicated it. Font sizes bumped
# throughout -- flagged as too small at the full-page-width these get embedded at.
# ==============================================================================

library(dplyr)
library(ggplot2)
library(here)
library(scales)
library(patchwork)

source(here("R", "paths.R"))
source(here("R", "service_config.R"))
source(here("R", "plotting_functions.R"))

biome_labels <- get_biome_labels()

get_short_name <- function(g, grp_name) {
  g <- as.character(g)
  if (any(grp_name %in% c("WWF_biome", "biome"))) {
    res <- biome_labels[g]
    unname(ifelse(is.na(res), g, res))
  } else {
    # income_grp's own raw values carry a leading ordinal ("1. High income: OECD", "3. Upper
    # middle income", ...) -- left as-is, this doubled up with the key's own "N: " numbering
    # (e.g. "1: 1. High income: OECD"), found 2026-09-02.
    sub("^\\d+\\.\\s*", "", g)
  }
}

#' Generate the unified abs/pct hotspot-magnitude boxplots for one or more groupings
#'
#' @param plt_long Long-format data frame with columns: service, abs_chg, pct_chg, and each
#'   grouping column (income_grp, region_wb, WWF_biome, nev_name).
#' @param groupings Character vector of grouping column names to iterate over.
#' @param pct_cutoff HOTS_CFG$pct_cutoff -- the tail fraction defining a hotspot.
#' @param loss_services Character vector of services whose hotspot direction is "decline"
#'   (HOTS_CFG$loss) -- unioned with ratio_names() internally, since ratios share the same
#'   "good when high" direction as the amounts.
#' @param all_services Character vector of services to include in the boxplots (typically
#'   c(service_names(), ratio_names())).
generate_hotspot_boxplots <- function(plt_long, groupings, pct_cutoff, loss_services, all_services) {
  groupings <- intersect(groupings, names(plt_long))
  if (length(groupings) == 0) {
    warning("No requested groupings found in plt_long; skipping hotspot boxplots.")
    return(invisible(NULL))
  }

  for (gc in groupings) {
    n_valid <- sum(!is.na(plt_long[[gc]]))
    if (n_valid == 0) {
      message("Skipping boxplots for ", gc, ": all values are NA.")
      next
    }

    hotspots_all <- plt_long %>%
      dplyr::filter(!is.na(.data[[gc]])) %>%
      # Lakes / Rock & Ice aren't meaningful ES categories -- excluded everywhere else this
      # project reports by WWF_biome (make_global_change_5panel.R, hotspot_synthesis.qmd's
      # intensity charts); this boxplot generator was the one place that gap survived, found
      # 2026-09-02 by comparing against the intensity chart's 14-biome key.
      {if (gc == "WWF_biome") dplyr::filter(., !.data[[gc]] %in% c("Lakes", "Rock & Ice")) else .} %>%
      dplyr::group_by(service) %>%
      dplyr::mutate(
        is_loss = service %in% c(loss_services, ratio_names()),
        cutoff = dplyr::if_else(is_loss, quantile(pct_chg, pct_cutoff, na.rm = TRUE), quantile(pct_chg, 1 - pct_cutoff, na.rm = TRUE)),
        is_hotspot = dplyr::if_else(is_loss, pct_chg <= cutoff, pct_chg >= cutoff)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(is_hotspot == TRUE)

    unique_groups <- sort(unique(as.character(hotspots_all[[gc]])))
    group_id_map  <- setNames(seq_along(unique_groups), unique_groups)

    key_labels <- paste0(group_id_map[unique_groups], ": ", get_short_name(unique_groups, gc))
    # ncol scales up for large group counts (e.g. nev_name's ~190 countries) so the key panel
    # stays within ggsave's 50-inch sanity limit -- a fixed ncol=3 hit that ceiling directly.
    key_ncol   <- max(3, ceiling(length(key_labels) / 15))
    key_panel  <- make_key_panel(key_labels, ncol = key_ncol)
    # Row height tightened twice 2026-09-02 (0.65 -> 0.4 -> 0.28): still too much vertical
    # whitespace between key rows even after the first cut.
    key_height <- ceiling(length(key_labels) / key_ncol) * 0.28 + 0.3

    plot_and_save <- function(metric, out_filename, n_cols = 2, width = 12, height = 15) {
      plot_stats <- hotspots_all %>%
        dplyr::filter(service %in% all_services, !is.na(.data[[metric]])) %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(c("service", gc)))) %>%
        dplyr::summarise(
          middle = stats::median(.data[[metric]], na.rm = TRUE),
          lower  = stats::quantile(.data[[metric]], 0.25, na.rm = TRUE),
          upper  = stats::quantile(.data[[metric]], 0.75, na.rm = TRUE),
          iqr    = stats::IQR(.data[[metric]], na.rm = TRUE),
          ymin   = max(min(.data[[metric]], na.rm = TRUE), lower - 1.5 * iqr),
          ymax   = min(max(.data[[metric]], na.rm = TRUE), upper + 1.5 * iqr),
          .groups = "drop"
        ) %>%
        dplyr::group_by(service) %>%
        dplyr::mutate(scaled_fill = scales::rescale(abs(middle))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          group = factor(.data[[gc]], levels = unique_groups),
          # Facets otherwise default to alphabetical (C_Prot_service, C_Risk_Red_Ratio,
          # N_Ret_Ratio, N_retention, ...) -- pin to the canonical order (amounts, then their
          # ratios, R/service_config.R's service_names()/ratio_names() order) used consistently
          # elsewhere in the pipeline (e.g. hotspot_extraction.qmd's svc_order), 2026-09-02.
          service = factor(service, levels = all_services)
        )

      if (nrow(plot_stats) == 0) {
        message("No data to plot for ", gc, " / ", metric)
        return(invisible(NULL))
      }

      metric_label <- if (metric == "pct_chg") "Percent Change (%)" else "Absolute Change"

      p <- ggplot(plot_stats, aes(x = group, ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax)) +
        geom_boxplot(stat = "identity", aes(fill = scaled_fill)) +
        facet_wrap(~service, scales = "free", ncol = n_cols) +
        labs(x = NULL, y = metric_label) +
        # Fill encodes each box's own |median| relative to other groups in the same facet
        # (darker = larger) -- deliberately unlabeled (guide="none"), a secondary visual cue, not
        # a value readers need to look up; the key panel below is only for the x-axis numbers.
        scale_fill_distiller(palette = "Reds", direction = 1, guide = "none") +
        scale_x_discrete(labels = function(x) group_id_map[x]) +
        theme_minimal(base_size = 14) +
        theme(
          strip.text  = element_text(face = "bold", size = 15),
          axis.text.y = element_text(size = 13),
          axis.text.x = element_text(size = 13, angle = 45, hjust = 1),
          axis.title  = element_text(size = 15)
        )

      out_dir <- here::here("outputs", "plots", "boxplots_unified", gc)
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      fname <- file.path(out_dir, out_filename)
      ggsave(fname, p / key_panel + plot_layout(heights = c(10, key_height)),
             width = width, height = height + key_height, bg = "white", dpi = 300)
      message("Saved: ", fname)
    }

    plot_and_save("abs_chg", "boxplots_abs.png")
    plot_and_save("pct_chg", "boxplots_pct.png")
  }
}
