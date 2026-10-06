# Absolute and relative change by group (biome, region, income group, country), Path A.
# Plotting half of zonal_stats_toolkit/compare_and_plot_changes.R, reading the map_data CSVs in
# outputs/plots/output_plots_diff/ (which carry the 2026-10-01 nitrogen reruns) instead of
# rebuilding them from zonal_stats_toolkit outputs that are not on this machine.
# Groups are named on the axis; no numbered colour legend.
#
# Usage: Rscript scripts/mapping/make_combined_diffs.R [out_dir]
#   default out_dir: outputs/plots/output_plots_diff

suppressMessages({library(dplyr); library(readr); library(ggplot2); library(patchwork)})
setwd(here::here())

in_dir <- "outputs/plots/output_plots_diff"
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args)) args[1] else in_dir
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

services <- c(
  N_export = "Nitrogen export", Sed_export = "Sediment export", C_Risk = "Coastal risk",
  N_Ret_Ratio = "Nitrogen retention ratio", Sed_Ret_Ratio = "Sediment retention ratio",
  C_Risk_Red_Ratio = "Coastal risk reduction ratio",
  Pollination = "Pollination", Nature_Access = "Nature access"
)
# services where an increase is adverse; for the rest a decrease is adverse
adverse_if_up <- c("N_export", "Sed_export", "C_Risk")

groups <- list(
  biome      = list(col = "WWF_biome",  title = "Biome",             top_n = NA),
  region_wb  = list(col = "region_wb",  title = "World Bank region", top_n = NA),
  income_grp = list(col = "income_grp", title = "Income group",      top_n = NA),
  country    = list(col = "nev_name",   title = "Country",           top_n = 5)
)

status_colors <- c(Adverse = "#F07D00", Favourable = "#009191", `No change` = "gray70")

direction <- function(svc, v) {
  case_when(v == 0 | is.na(v) ~ "No change",
            (v > 0) == (svc %in% adverse_if_up) ~ "Adverse",
            TRUE ~ "Favourable")
}

panel <- function(d, value, title, xlab, show_names, scales) {
  ggplot(d, aes(x = label, y = .data[[value]])) +
    geom_col(aes(fill = .data[[paste0("dir_", value)]]), width = 0.8) +
    { if (value == "mean_val") geom_linerange(aes(ymin = mean_val - se_val, ymax = mean_val + se_val),
                                              alpha = 0.5, linewidth = 0.5) } +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.3) +
    coord_flip() +
    facet_wrap(~ service_name, scales = scales, ncol = 3, labeller = label_wrap_gen(20)) +
    scale_x_discrete(labels = function(x) sub("__.*$", "", x)) +
    scale_y_continuous(n.breaks = 4, labels = scales::label_number(big.mark = ",", drop0trailing = TRUE)) +
    scale_fill_manual(values = status_colors, name = "Direction of change") +
    labs(title = title, x = NULL, y = xlab) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
          strip.text = element_text(face = "bold", size = 11),
          panel.grid.major.y = element_blank(),
          panel.spacing.x = unit(1.2, "lines"),
          axis.text.x = element_text(size = 8),
          axis.text.y = if (show_names) element_text(size = 9) else element_blank())
}

for (g in names(groups)) {
  cfg <- groups[[g]]
  d <- read_csv(file.path(in_dir, paste0(g, "_map_data.csv")), show_col_types = FALSE) |>
    filter(service %in% names(services)) |>
    mutate(group = .data[[cfg$col]],
           service_name = factor(services[service], levels = services),
           dir_mean_val = factor(direction(service, mean_val), levels = names(status_colors)),
           dir_sym_pct_change = factor(direction(service, sym_pct_change), levels = names(status_colors)))
  if (g == "income_grp") d <- mutate(d, group = sub("^[0-9]\\. ", "", group))

  if (is.na(cfg$top_n)) {
    lev <- sort(unique(d$group), decreasing = TRUE)   # alphabetical from the top after coord_flip
    if (g == "income_grp") lev <- rev(c("High income: OECD", "Upper middle income", "Lower middle income", "Low income"))
    d_abs <- mutate(d, label = factor(group, levels = lev))
    d_pct <- d_abs |> filter(!is.na(sym_pct_change))
    scales <- "free_x"; names_on_pct <- FALSE
  } else {
    # countries: largest and smallest top_n per service, excluding the 10% smallest by valid area
    pick <- function(v) d |> group_by(service) |>
      filter(!is.na(.data[[v]]), valid_count >= quantile(valid_count, 0.10, na.rm = TRUE)) |>
      arrange(.data[[v]], .by_group = TRUE) |>
      filter(row_number() <= cfg$top_n | row_number() > n() - cfg$top_n) |> ungroup() |>
      mutate(label = reorder(paste(group, service, sep = "__"), .data[[v]]))
    d_abs <- pick("mean_val"); d_pct <- pick("sym_pct_change")
    scales <- "free"; names_on_pct <- TRUE
  }

  p <- (panel(d_abs, "mean_val", "Absolute change per unit area", "2020 minus 1992", TRUE, scales) |
        panel(d_pct, "sym_pct_change", "Relative change (SPC, %)", "Symmetric percentage change, 1992 to 2020",
              names_on_pct, scales)) +
    plot_annotation(title = cfg$title,
                    theme = theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

  out <- file.path(out_dir, paste0(g, "_combined_diffs.png"))
  ggsave(out, p, width = 16, height = if (g == "country") 12 else 9, bg = "white", dpi = 300)
  message("wrote ", out)
}
