# ==============================================================================
# Compare Local vs. Downstream Exposure for Compound Hotspots
# ==============================================================================

# 1. Load Required Libraries
library(tidyverse)
library(sf)
library(ggplot2)
library(scales)
library(glue)

# 2. Setup Paths
# Adjust to your project structure
data_dir <- "C:/projects/global_NCP/data"
csv_base_dir <- file.path(data_dir, "processed", "hotspots", "hotspot_beneficiaries")

# 3. Load the Serviceshed / Downstream Data
# We know there are four CSVs for '2 or more', '3 or more', '4 or more', and 'all hotspots'
folders <- list.dirs(csv_base_dir, recursive = FALSE)

load_serviceshed_data <- function(folders) {
  df_list <- lapply(folders, function(folder) {
    csv_file <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)[1]
    if (length(csv_file) > 0) {
      df <- read_csv(csv_file, show_col_types = FALSE)
      
      # Extract threshold name from folder name
      thresh_name <- basename(folder)
      df <- df %>% mutate(threshold = thresh_name)
      return(df)
    }
  })
  bind_rows(df_list)
}

serviceshed_df <- load_serviceshed_data(folders)

# Standardize threshold labels
serviceshed_df <- serviceshed_df %>%
  mutate(
    threshold = case_when(
      threshold == "all hotspots" ~ ">= 1 Service",
      threshold == "2 or more overlapping" ~ ">= 2 Services",
      threshold == "3 or more overlapping" ~ ">= 3 Services",
      threshold == "4 or more overlapping" ~ ">= 4 Services",
      TRUE ~ threshold
    )
  )

# 4. Load the Grid and Hotspot Count to Calculate Local (In-Situ) Exposure
# We need to recreate the `df_overlap` used in the synthesis script to get local population
gpkg_grid <- list.files(file.path(data_dir, "processed"), pattern = "grid_.*\\.gpkg$", full.names = TRUE)[1]

# In a real run, you might want to load the exact grid and overlapping counts directly from your R objects or `.rds`.
# Assuming `df_overlap` has columns `fid`, `hotspot_count`, and `GHS_POP_E2020_GLOBE_sum` 
# For demonstration, we'll outline the logic you would use:
cat("Loading grid and computing local exposure...\n")

grid_df <- st_read(gpkg_grid, quiet = TRUE) %>% st_drop_geometry()

# NOTE: We reconstruct the `df_overlap` logic directly from plt_long.rds
plt_long_path <- file.path(data_dir, "processed", "plt_long.rds")
if(file.exists(plt_long_path)) {
  plt_long <- readRDS(plt_long_path)
  
  # Configuration based on hotspot_synthesis.qmd defaults
  pct_cutoff <- 0.05
  loss_services <- c("Nature_Access", "Pollination", "N_export", "Sed_export", "C_Risk")
  gain_services <- c("N_Ret_Ratio", "Sed_Ret_Ratio", "C_Risk_Red_Ratio")
  
  # Function to identify single hotspots (simplified thresholding)
  # For exact matching, it is recommended to source extract_hotspots.R
  # Here we use a robust approximation:
  hs_flags <- plt_long %>%
    group_by(service) %>%
    mutate(
      is_hotspot = case_when(
        service %in% loss_services & pct_chg <= quantile(pct_chg, pct_cutoff, na.rm = TRUE) ~ 1,
        service %in% gain_services & pct_chg >= quantile(pct_chg, 1 - pct_cutoff, na.rm = TRUE) ~ 1,
        TRUE ~ 0
      )
    ) %>%
    filter(is_hotspot == 1) %>%
    ungroup() %>%
    select(fid, service)
    
  # Count overlaps per cell
  hotspot_counts <- hs_flags %>% count(fid, name = "hotspot_count")
  
  # Join with canonical grid to get population
  df_overlap <- grid_df %>%
    left_join(hotspot_counts, by = c("RasterVal" = "fid")) %>%
    mutate(hotspot_count = replace_na(hotspot_count, 0))
    
  local_exposure <- df_overlap %>%
    mutate(
      thresh_1 = ifelse(hotspot_count >= 1, GHS_POP_E2020_GLOBE_sum, 0),
      thresh_2 = ifelse(hotspot_count >= 2, GHS_POP_E2020_GLOBE_sum, 0),
      thresh_3 = ifelse(hotspot_count >= 3, GHS_POP_E2020_GLOBE_sum, 0),
      thresh_4 = ifelse(hotspot_count >= 4, GHS_POP_E2020_GLOBE_sum, 0)
    ) %>%
    summarise(
      `>= 1 Service` = sum(thresh_1, na.rm = TRUE),
      `>= 2 Services` = sum(thresh_2, na.rm = TRUE),
      `>= 3 Services` = sum(thresh_3, na.rm = TRUE),
      `>= 4 Services` = sum(thresh_4, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = everything(), names_to = "threshold", values_to = "local_population")
    
  # Join with serviceshed data
  comparison_df <- serviceshed_df %>%
    left_join(local_exposure, by = "threshold")
    
  print(comparison_df)
} else {
  warning("plt_long.rds not found. Run hotspot_extraction.qmd first.")
}

# 5. Visualizing the Multiplier Effect
comparison_df <- comparison_df %>%
  mutate(multiplier = union_population / local_population)

print(comparison_df)

# Save comparison dataframe
out_table_dir <- "C:/projects/global_NCP/outputs/tables"
out_plot_dir <- "C:/projects/global_NCP/outputs/plots"

if(!dir.exists(out_table_dir)) dir.create(out_table_dir, recursive = TRUE)
if(!dir.exists(out_plot_dir)) dir.create(out_plot_dir, recursive = TRUE)

write_csv(comparison_df, file.path(out_table_dir, "exposure_comparison.csv"))

p <- ggplot(comparison_df) +
  geom_segment(aes(x = local_population, xend = union_population, y = threshold, yend = threshold), color = "grey50") +
  geom_point(aes(x = local_population, y = threshold, color = "Local (In-Situ)"), size = 4) +
  geom_point(aes(x = union_population, y = threshold, color = "Total Exposed (Including Downstream)"), size = 4) +
  scale_x_continuous(labels = scales::unit_format(unit = "B", scale = 1e-9)) +
  labs(
    title = "Multiplier Effect: Downstream Population vs. Local Population",
    x = "Exposed Population",
    y = "Compound Hotspot Severity",
    color = "Exposure Type"
  ) +
  theme_minimal()

ggsave(file.path(out_plot_dir, "multiplier_effect.png"), p, width = 10, height = 6, dpi = 300)
cat("Saved plot to outputs/plots/multiplier_effect.png\n")
