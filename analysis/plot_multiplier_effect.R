library(tidyverse)
library(here)
library(scales)

# 1. Load Data
data_path <- here("outputs", "tables", "exposure_comparison_compiled.csv")
df <- read_csv(data_path)

# Define the groupings we want to plot and export
groupings <- c("region_wb", "income_grp", "WWF_biome", "country")

for (grp_col in groupings) {
  
  # 2. Prepare Data for Dumbbell Plot
  plot_data <- df %>%
    filter(
      overlap_category == "all hotspots",
      !!sym(grp_col) != "Global",
      !!sym(grp_col) != "nan",
      !is.na(!!sym(grp_col))
    ) %>%
    # Aggregate across all other dimensions FIRST
    group_by(!!sym(grp_col), exposure_type) %>%
    summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
    # Pivot wider to get exposure types in separate columns
    pivot_wider(
      names_from = exposure_type,
      values_from = population,
      values_fill = 0
    )

  # Safely ensure all required columns exist
  if (!"travel_footprint" %in% names(plot_data)) plot_data$travel_footprint <- 0
  if (!"hydrological" %in% names(plot_data)) plot_data$hydrological <- 0
  if (!"combined_total" %in% names(plot_data)) plot_data$combined_total <- 0

  plot_data <- plot_data %>%
    # Calculate the multiplier effect
    mutate(
      multiplier = travel_footprint / hydrological
    ) %>%
    # Order regions by their combined total exposure
    arrange(combined_total) %>%
    mutate(!!sym(grp_col) := factor(!!sym(grp_col), levels = unique(!!sym(grp_col))))
    
  # EXPORT THE TABLE FOR THE BOOK/PAPER!
  write_csv(plot_data, here("outputs", "tables", paste0("multiplier_summary_", grp_col, ".csv")))
  
  # Skip generating giant messy plots for country-level (just keep the CSV table)
  if (grp_col == "country") next
  
  # 3. Plotting
  p <- ggplot(plot_data) +
    # Dumbbell segment connecting the two exposures
    geom_segment(
      aes(x = hydrological, xend = travel_footprint, y = !!sym(grp_col), yend = !!sym(grp_col)),
      color = "gray70", linewidth = 1.5
    ) +
    # Hydrological points
    geom_point(aes(x = hydrological, y = !!sym(grp_col), color = "Hydrological (Downstream)"), size = 4) +
    # Travel footprint points
    geom_point(aes(x = travel_footprint, y = !!sym(grp_col), color = "Access-Based (Travel)"), size = 4) +
    
    # Pseudo-log scale allows 0 values without evaluating to -Inf and dropping points
    scale_x_continuous(trans = "pseudo_log", breaks = c(0, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9), labels = label_number(scale_cut = cut_short_scale())) +
    scale_color_manual(values = c("Hydrological (Downstream)" = "#007930", "Access-Based (Travel)" = "#F07D00")) +
    
    labs(
      title = "The Multiplier Effect of Ecosystem Service Hotspots",
      subtitle = paste("Comparing exposure mechanisms by", grp_col),
      x = "Exposed Population (Log Scale)",
      y = NULL,
      color = "Exposure Mechanism"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  ggsave(here("outputs", "plots", paste0("exposure_multiplier_dumbbell_", grp_col, ".png")), p, width = 10, height = 6, bg = "white")
}