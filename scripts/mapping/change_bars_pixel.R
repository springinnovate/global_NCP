# ==============================================================================
# PIPELINE: Zonal Stats Consolidation, Cleaning, and Visualization
# Project: Global NCP Analysis
# ==============================================================================

library(tidyverse)
source("../R/paths.R")

# --- 0. CONFIGURATION & HELPERS ---
# Define a single source of truth for name cleaning to use throughout the script
clean_service_names <- function(column_names) {
  column_names %>%
    # Legacy cleaning (kept for reference)
    # str_remove_all("_md5_[a-z0-9]+|_[0-9]{8}_[0-9]{6}") %>%
    # str_remove_all("_tnc|_esa|_value|_compressed|_mar|mar|_lspop2019|_ESA|ine_mod_ESA") %>%
    # str_replace_all("_Rt_change_coastal_risk.*", "_coastal_risk") %>%
    # str_replace_all("_Service_change_coastal_risk.*", "_coastal_service") %>%
    # str_replace_all("_nature_access_.*", "_nature_access") %>%
    # str_replace_all("_Nitrogen_Export_diff", "_nitrogen_export") %>%
    # str_replace_all("_Pollination_diff", "_pollination") %>%
    # str_replace_all("_Sediment_Export_diff", "_sediment_export") %>%
    
    # Simplified cleaning as column names are now cleaner
    tolower() %>%
    str_replace_all("__+", "_") %>%
    str_replace_all("_$", "")
}

# --- 1. DATA INGESTION & STACKING ---
data_dir_zonal <- project_dir("data", "zonal_analysis_new")

# Fallback: Check if data is in the OneDrive folder if not found in the repo
if (!dir.exists(data_dir_zonal)) {
  onedrive_path <- "C:/Users/JerónimoRodríguezEsc/OneDrive - World Wildlife Fund, Inc/PROJECTS/Global_NCP/data/zonal_analysis_new"
  if (dir.exists(onedrive_path)) {
    data_dir_zonal <- onedrive_path
    message("Using local OneDrive data path: ", data_dir_zonal)
  }
}

if (!dir.exists(data_dir_zonal)) stop("Data directory not found: ", data_dir_zonal)

file_list <- list.files(path = data_dir_zonal, pattern = "\\.csv$", full.names = TRUE) %>% 
  .[!str_detect(., "data[._](combined|filtered|final|change|ES)")]

tt_combined <- map_df(file_list, ~{
  df <- read_csv(.x, show_col_types = FALSE)
  # Extract filename from full path and remove timestamp/extension
  grp_name <- basename(.x) %>% str_remove("_[0-9]{8}_[0-9]{6}\\.csv$") %>% str_remove("\\.csv$")
  
  # Robust renaming of the first column to 'unit'
  colnames(df)[colnames(df) == "unit"] <- "unit_original"
  colnames(df)[1] <- "unit"
  
  df %>%
    mutate(grouping = grp_name, unit = as.character(unit)) %>%
    select(grouping, unit, everything())
})

# write_csv(tt_combined, "data.combined.csv")

# --- 2. FILTERING & CLEANING ---
# Keep only relevant summary stats and change variables
tt_filtered <- tt_combined %>% 
  select(grouping, unit, starts_with(c("mean_", "stdev_", "valid_count_"))) %>%
  filter(unit != "Antarctica") %>%
  # Calculate difference of means for ratio variables (Latest - Earliest)
  # Note: These will not have associated stdev/se, which is expected.
  mutate(
    mean_N_ret_ratio_diff = mean_N_ret_ratio_2020 - mean_N_ret_ratio_1992,
    mean_Sed_ret_ratio_diff = mean_Sed_ret_ratio_2020 - mean_Sed_ret_ratio_1992
  )

# TODO: Future QAQC - Check if valid pixel counts are consistent between years.
# We previously implemented a check for >5% difference in valid_count between 1992 and 2020.
# This flagged some units (e.g. coastal areas) where the mask changed significantly.
# We decided to disable this for now as the ratio variables might behave differently, 
# but it is a relevant test for future iterations to ensure "apples to apples" comparison.

# Select only the "change" or "diff" columns
tt_ch <- tt_filtered %>% 
  select(grouping, unit, contains("change"), contains("diff"))

# Apply the unified cleaning function # Lecacy also, the name cleaning has been sorted of. 
names(tt_ch) <- clean_service_names(names(tt_ch))

write_csv(tt_ch, "change_variables_cleaned.csv")

# --- 3. ANALYSIS (Standard Error Calculation) ---
# Reshape to long format to calculate SE across all services
tt_analysis <- tt_ch %>%
  select(-matches("X1|unnamed|source_file")) %>%
  pivot_longer(
    cols = -c(grouping, unit),
    names_to = c(".value", "service"),
    names_pattern = "(mean|stdev|valid_count)_(.*)"
  ) %>%
  mutate(se = stdev / sqrt(valid_count)) %>%
  filter(!is.na(mean), mean != 0) # Focus on meaningful changes

# Save wide version for Becky/Rich (Mean and SE columns)
tt_final_wide <- tt_analysis %>%
  select(grouping, unit, service, mean, se) %>%
  pivot_wider(
    names_from = service, 
    values_from = c(mean, se),
    names_glue = "{.value}_{service}"
  )

write_csv(tt_final_wide, "final_ES_change_analysis.csv")

# --- 4. VISUALIZATION ---

# We use a more specific name 'target_group' to avoid any confusion with column names
generate_es_plot <- function(target_group) {
  
  # Use .env$ to explicitly pull 'target_group' from the function argument
  data_subset <- tt_analysis %>% 
    filter(grouping == .env$target_group)
  
  # Skip if no data for this group
  if(nrow(data_subset) == 0) {
    message(paste("No data found for", target_group))
    return(NULL)
  }
  
  # Calculate height based on number of units (0.22 inches per unit)
  # This ensures long lists like Countries are readable
  num_units <- length(unique(data_subset$unit))
  calc_height <- max(6, num_units * 0.22)
  
  message(paste("Generating plot for:", target_group, "(Units:", num_units, ")"))
  
  p <- ggplot(data_subset, aes(x = mean, y = reorder(unit, mean), fill = service)) +
    geom_col(alpha = 0.7, show.legend = FALSE) +
    geom_errorbar(aes(xmin = mean - se, xmax = mean + se), width = 0.3, color = "grey30", na.rm = TRUE) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    facet_wrap(~service, scales = "free_x") +
    theme_minimal() +
    labs(
      title = paste("Ecosystem Service Change:", target_group),
      subtitle = "Bars = Mean Change | Error Bars = +/- 1 SE (1992-2020)",
      x = "Mean Change Value", 
      y = NULL
    ) +
    theme(
      strip.text = element_text(face = "bold", size = 11),
      axis.text.y = element_text(size = 7)
    )
  
  # Save with limitsize = FALSE to handle the long country plots
  ggsave(
    filename = paste0("ES_plot_", target_group, ".png"), 
    plot = p, 
    width = 14, 
    height = calc_height, 
    limitsize = FALSE
  )
}

# 5. EXECUTION
# Get the list of groupings and run the function for each
unique_groupings <- unique(tt_analysis$grouping)

# walk() is like a loop that doesn't print messy output
walk(unique_groupings, generate_es_plot)
