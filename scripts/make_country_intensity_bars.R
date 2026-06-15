library(dplyr)
library(ggplot2)
library(readr)
library(here)

# 1. Setup and Config
out_dir <- here("outputs", "plots", "country_summaries")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

data_file <- here("data", "processed", "tables", "hotspot_area_stats.csv")

# 2. Main Logic Function
generate_country_intensity_bars <- function(stats_file, top_n = 20) {
  if(!file.exists(stats_file)) {
    message("WARNING: Data file not found: ", stats_file)
    return(NULL)
  }
  
  message("Processing country-level hotspot intensity...")
  stats_data <- read_csv(stats_file, show_col_types = FALSE)
  
  # Ensure the country data exists
  if (!"country" %in% unique(stats_data$grouping_var) && !"nev_name" %in% unique(stats_data$grouping_var)) {
    message("WARNING: 'country' or 'nev_name' grouping not found in data.")
    return(NULL)
  }
  
  country_col <- if ("country" %in% stats_data$grouping_var) "country" else "nev_name"
  
  country_data <- stats_data %>%
    filter(grouping_var == country_col) %>%
    filter(!is.na(group))
  
  # Get list of services
  services <- unique(country_data$service)
  
  for (svc in services) {
    # Filter for the service, arrange by pct_area (intensity), and take top N
    plot_data <- country_data %>%
      filter(service == svc) %>%
      arrange(desc(pct_area)) %>%
      head(top_n) %>%
      mutate(group = reorder(group, pct_area)) # Order factors for plotting
      
    if (nrow(plot_data) == 0) next
    
    # Generate horizontal bar plot
    p <- ggplot(plot_data, aes(x = pct_area, y = group, fill = pct_area)) +
      geom_col() +
      scale_fill_viridis_c(option = "magma", direction = -1, guide = "none") +
      labs(
        title = paste("Hotspot Intensity for", svc),
        subtitle = "Top countries by percentage of area identified as a hotspot",
        x = "Hotspot Intensity (% of Country Area)",
        y = NULL
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank()
      )
      
    # Save the plot
    safe_svc <- gsub("[^A-Za-z0-9]", "_", svc)
    out_file <- file.path(out_dir, paste0("intensity_ranking_", safe_svc, ".png"))
    ggsave(out_file, p, width = 8, height = 6, bg = "white")
    message("Saved: ", out_file)
  }
}

# 3. Execution Pipeline
if (sys.nframe() == 0) {
    generate_country_intensity_bars(data_file)
}
