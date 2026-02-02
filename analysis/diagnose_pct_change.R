# R script to diagnose pct_change < -100
# Make sure to have the 'tidyverse' and 'here' packages installed.
# install.packages(c("tidyverse", "here"))

library(tidyverse)
library(here)

# Load the data
file_path <- here("analysis_data", "biome_20251224_034818.csv")
data <- read_csv(file_path)

# Identify all percentage change columns
# Investigator note: The original analysis used a different pattern.
# The user might need to adjust this if the columns are not found.
pct_chg_cols <- data %>%
  select(matches("_change$")) %>%
  names()

# Loop through each pct_change column to find and diagnose problematic rows
for (pct_col in pct_chg_cols) {

  # From the Pct col, get the initial and final columns
  base_name <- str_remove(pct_col, "_change")

  # find the initial and final columns that has the most similar name
  initial_col <- names(data)[grepl(paste0(base_name, ".*1992"), names(data))]
  final_col <- names(data)[grepl(paste0(base_name, ".*2020"), names(data))]

  # We can only proceed if we uniquely identify the columns
  if (length(initial_col) == 1 && length(final_col) == 1) {
    # now that we have the columns, let's calculate the percentage change
    # and find the rows where the absolute difference is greater than 0.1
    # this is to account for floating point errors

    # Calculate percent change internally
    data_with_pct <- data %>%
        mutate(
            calculated_pct_change = ((.data[[final_col]] - .data[[initial_col]]) / .data[[initial_col]])
        )

    # find rows where the pre-calculated change is < -100
    # but let's represent it as < -1
    problem_rows <- data_with_pct %>%
        filter(calculated_pct_change < -1)


    if (nrow(problem_rows) > 0) {
      cat("----------------------------------------\n")
      cat("Found issues in column:", pct_col, "\n")
      cat("----------------------------------------\n")

      # Select and show the relevant columns
      diagnosis_df <- problem_rows %>%
        select(
          WWF_biome,
          initial_value = all_of(initial_col),
          final_value = all_of(final_col),
          pre_calculated_change = all_of(pct_col),
          calculated_pct_change
        )

      print(diagnosis_df)

    } else {
        cat("No issues found for: ", pct_col, "\n")
    }
  } else {
    cat("Could not uniquely identify initial/final columns for:", pct_col, "\n")
    cat("You may need to inspect the column names manually.\n")
  }
}
