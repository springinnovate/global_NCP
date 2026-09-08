# Reusable plotting functions for Global NCP Quarto book
# Extracted from analysis/eda_dashboard.qmd for consistency across chapters

library(dplyr)
library(ggplot2)
library(tidytext)
library(patchwork)

#' Plain-text "N: Name" key panel, for charts whose y/x-axis is abbreviated to a number
#'
#' Centralized 2026-09-02: was an ad hoc local copy in hotspot_synthesis.qmd's intensity-chart
#' chunk before this, and about to become a second independent copy in the boxplot generator --
#' pulled here first instead, per this project's own hard-won lesson this week about local copies
#' of the same logic silently drifting (see docs/runbook.md's service-config-drift section).
#' Replaces an older per-group color-swatch legend (an invisible geom_point() alpha=0 hack) that
#' no longer serves a purpose once bars stopped being colored by group. Format changed from
#' "[N] Name" to "N: Name" and default size bumped 11->15pt same day -- flagged as still too small
#' relative to the rest of the figure (axis/strip text already at 13-16pt).
#'
#' @param labels Character vector of "N: Name" strings, in the desired 1..N reading order.
#' @param ncol Number of columns to wrap the key into.
#' @param text_size Point size of the key text (not mm -- converted internally).
#' @return A ggplot object (a text-only grid, no axes), meant to be stacked below the main plot
#'   with patchwork (`main_plot / make_key_panel(labels) + plot_layout(heights = c(10, key_height))`).
make_key_panel <- function(labels, ncol = 3, text_size = 15) {
  n <- length(labels)
  nrow <- ceiling(n / ncol)
  df <- data.frame(
    label = labels,
    col   = (seq_len(n) - 1) %% ncol,
    row   = nrow - ((seq_len(n) - 1) %/% ncol)
  )
  ggplot(df, aes(x = col, y = row, label = label)) +
    geom_text(hjust = 0, size = text_size / .pt) +
    scale_x_continuous(limits = c(-0.05, ncol), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.4, nrow + 0.6), expand = c(0, 0)) +
    theme_void()
}

# Biome name mappings for cleaner labels
get_biome_labels <- function() {
  c(
    'Tropical & Subtropical Moist Broadleaf Forests' = 'Trop/Subtrop Moist Broadleaf',
    'Tropical & Subtropical Dry Broadleaf Forests' = 'Trop/Subtrop Dry Broadleaf',
    'Tropical & Subtropical Coniferous Forests' = 'Trop/Subtrop Coniferous',
    'Temperate Broadleaf & Mixed Forests' = 'Temp Broadleaf/Mixed',
    'Temperate Coniferous Forests' = 'Temp Coniferous',
    'Boreal Forests/Taiga' = 'Boreal/Taiga',
    'Tropical & Subtropical Grasslands, Savannas & Shrublands' = 'Trop/Subtrop Grass/Sav/Shrub',
    'Temperate Grasslands, Savannas & Shrublands' = 'Temp Grass/Sav/Shrub',
    'Flooded Grasslands & Savannas' = 'Flooded Grass/Savannas',
    'Montane Grasslands & Shrublands' = 'Montane Grass/Shrub',
    'Tundra' = 'Tundra',
    'Mediterranean Forests, Woodlands & Scrub' = 'Mediterranean',
    'Deserts & Xeric Shrublands' = 'Deserts & Xeric Shrub',
    'Mangroves' = 'Mangroves'
  )
}

#' Create Relative Intensity Plot by Grouping Variable
#'
#' Displays hotspot relative intensity (disproportionate burden) by geographic,
#' socioeconomic, or ecological grouping. Red bars indicate groups with
#' disproportionately high hotspot concentration (relative_intensity > 1).
#'
#' @param df Data frame with columns: service, grouping_var, group, relative_intensity
#' @param grp_var Grouping variable name (e.g., "region_wb", "income_grp", "WWF_biome")
#'
#' @return ggplot object
#' @export
make_intensity_plot <- function(df, grp_var) {
  if (nrow(df) == 0) return(ggplot() + theme_void() + ggtitle("No data"))

  d <- df %>%
    filter(grouping_var == grp_var) %>%
    filter(!is.na(group))

  if (nrow(d) == 0) return(ggplot() + theme_void() + ggtitle("No data for this grouping"))

  # Apply biome label simplifications if biome grouping
  if (grp_var == "WWF_biome") {
    biome_labels <- get_biome_labels()
    d <- d %>%
      mutate(group = ifelse(group %in% names(biome_labels),
                            biome_labels[group],
                            group))
  }

  # Set factor levels for consistent alphabetical ordering (reverse for descending)
  all_groups <- sort(unique(d$group))
  d <- d %>% mutate(group = factor(group, levels = rev(all_groups)))

  ggplot(d, aes(x = relative_intensity, y = group, fill = relative_intensity > 1)) +
    geom_col() +
    geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 0.5) +
    scale_fill_manual(
      values = c("TRUE" = "#E83737", "FALSE" = "gray70"),
      guide = "none"
    ) +
    facet_wrap(~service, scales = "free_x", ncol = 2) +
    labs(
      x = "Relative Intensity Score\n(Share of Hotspots / Share of Area)",
      y = NULL,
      title = "Disproportionate Burden: Hotspot Concentration"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      panel.grid.minor.y = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(hjust = 0, size = 14, face = "bold")
    )
}

#' Create Multi-Service Overlap Plot
#'
#' Displays the average number of overlapping ecosystem service hotspots
#' per grid cell by grouping variable. Higher values indicate compound risk zones.
#'
#' @param df Data frame with columns: service, grouping, group, mean_hotspots
#' @param grp_var Grouping variable name (e.g., "region_wb", "income_grp")
#'
#' @return ggplot object
#' @export
make_multi_plot <- function(df, grp_var) {
  if (nrow(df) == 0) return(ggplot() + theme_void() + ggtitle("No data"))

  d <- df %>%
    filter(grouping == grp_var) %>%
    filter(!is.na(group))

  if (nrow(d) == 0) return(ggplot() + theme_void() + ggtitle("No data for this grouping"))

  # Apply biome label simplifications if biome grouping
  if (grp_var == "WWF_biome") {
    biome_labels <- get_biome_labels()
    d <- d %>%
      mutate(group = ifelse(group %in% names(biome_labels),
                            biome_labels[group],
                            group))
  }

  # Order strictly alphabetical
  d <- d %>%
    mutate(group = factor(group, levels = rev(sort(unique(as.character(group))))))

  ggplot(d, aes(x = mean_hotspots, y = group, fill = mean_hotspots)) +
    geom_col() +
    scale_fill_distiller(
      palette = "Reds",
      direction = 1,
      guide = "none"
    ) +
    labs(
      x = "Average Number of Overlapping Hotspots per 10km Cell",
      y = NULL,
      title = "Compound Risk: Multi-Service Overlap"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      panel.grid.minor.y = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(hjust = 0, size = 14, face = "bold")
    )
}

#' Create Country-Level Relative Intensity Plot
#'
#' Displays hotspot relative intensity ranked by country, faceted by service.
#' Countries with relative_intensity > 1 are colored red.
#'
#' @param df Data frame with columns: service, grouping_var, group, relative_intensity
#'
#' @return ggplot object
#' @export
make_country_intensity_plot <- function(df) {
  d <- df %>%
    filter(grouping_var == "nev_name") %>%
    filter(!is.na(group))

  if (nrow(d) == 0) return(ggplot() + theme_void() + ggtitle("No data"))

  # Order by intensity within each facet (service)
  d <- d %>%
    mutate(group_ordered = reorder_within(group, relative_intensity, service))

  ggplot(d, aes(x = relative_intensity, y = group_ordered, fill = relative_intensity > 1)) +
    geom_col() +
    geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 0.5) +
    scale_fill_manual(
      values = c("TRUE" = "#E83737", "FALSE" = "gray70"),
      guide = "none"
    ) +
    facet_wrap(~service, scales = "free_y", ncol = 2) +
    scale_y_reordered() +
    labs(
      x = "Relative Intensity Score",
      y = NULL,
      title = "Country-Level Hotspot Concentration"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "none",
      axis.text.y = element_text(size = 7),
      panel.grid.minor.y = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(hjust = 0, size = 14, face = "bold")
    )
}

#' Create Country-Level Multi-Service Overlap Plot
#'
#' Ranks countries by average number of overlapping hotspots.
#'
#' @param df Data frame with columns: grouping, group, mean_hotspots
#'
#' @return ggplot object
#' @export
make_country_multi_plot <- function(df) {
  d <- df %>%
    filter(grouping == "nev_name") %>%
    filter(!is.na(group))

  if (nrow(d) == 0) return(ggplot() + theme_void() + ggtitle("No data"))

  # Order strictly descending by value
  d <- d %>%
    arrange(mean_hotspots) %>%
    mutate(group = factor(group, levels = unique(group)))

  ggplot(d, aes(x = mean_hotspots, y = group, fill = mean_hotspots)) +
    geom_col() +
    scale_fill_distiller(
      palette = "Reds",
      direction = 1,
      guide = "none"
    ) +
    labs(
      x = "Average Number of Overlapping Hotspots",
      y = NULL,
      title = "Country Compound Risk Ranking"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "none",
      axis.text.y = element_text(size = 7),
      panel.grid.minor.y = element_blank(),
      plot.title = element_text(hjust = 0, size = 14, face = "bold")
    )
}
