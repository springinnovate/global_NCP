library(dplyr)

# Load using base R to avoid readr/vroom memory-mapping segfaults in Git Bash
area_stats <- read.csv("data/processed/tables/hotspot_area_stats.csv", stringsAsFactors = FALSE)

# Set up logging to permanently save the audit results
out_file <- "outputs/audit_summary.txt"
sink(out_file, split = TRUE) # split=TRUE prints to console AND saves to file

cat("\n======================================================\n")
cat("   GROUND-TRUTH AUDIT: Income Group Disparity\n")
cat("======================================================\n")

# Calculate the average relative intensity for each income group across all services
income_stats <- area_stats %>%
  filter(grouping_var == "income_grp", !is.na(group)) %>%
  group_by(group) %>%
  summarise(avg_relative_intensity = mean(relative_intensity, na.rm = TRUE)) %>%
  arrange(desc(avg_relative_intensity))

print(income_stats)

# Extract specific values to test the "2-3x" claim
low_income <- income_stats %>% filter(grepl("Low income", group, ignore.case=TRUE)) %>% pull(avg_relative_intensity)
high_income <- income_stats %>% filter(grepl("High income", group, ignore.case=TRUE)) %>% pull(avg_relative_intensity)

if(length(low_income) > 0 && length(high_income) > 0) {
  ratio <- mean(low_income) / mean(high_income)
  cat(sprintf("\n[RESULT] Low Income intensity is %.2f times higher than High Income intensity.\n", ratio))
}
cat("======================================================\n\n")

# 2. Audit Regional Intensities
cat("\n======================================================\n")
cat("   GROUND-TRUTH AUDIT 2: Regional Disparity\n")
cat("======================================================\n")

region_stats <- area_stats %>%
  filter(grouping_var == "region_wb", !is.na(group)) %>%
  group_by(group) %>%
  summarise(avg_relative_intensity = mean(relative_intensity, na.rm = TRUE)) %>%
  arrange(desc(avg_relative_intensity))

print(region_stats)

# 3. Audit the Attribution Gap (Drivers)
cat("\n======================================================\n")
cat("   GROUND-TRUTH AUDIT 3: The Attribution Gap\n")
cat("======================================================\n")

driver_files <- list.files("data/processed/tables", pattern = "^lcc_es_hotspot_overlap", full.names = TRUE)

if(length(driver_files) > 0) {
  drivers <- read.csv(driver_files[1], stringsAsFactors = FALSE)
  avg_overlap <- mean(drivers$pct_overlap, na.rm = TRUE)
  cat(sprintf("\n[RESULT] On average, Land Cover Change explains %.1f%% of ES Hotspots.\n", avg_overlap))
  cat(sprintf("[RESULT] The Attribution Gap (Unexplained by Conversion) is %.1f%%.\n", 100 - avg_overlap))
} else {
  cat("\n[WARNING] Could not find the lcc_es_hotspot_overlap CSV to audit drivers.\n")
}

# 4. Audit Biome Intensities
cat("\n======================================================\n")
cat("   GROUND-TRUTH AUDIT 4: Biome Disparity\n")
cat("======================================================\n")

biome_stats <- area_stats %>%
  filter(grouping_var == "WWF_biome", !is.na(group)) %>%
  group_by(group) %>%
  summarise(avg_relative_intensity = mean(relative_intensity, na.rm = TRUE)) %>%
  arrange(desc(avg_relative_intensity))

print(biome_stats, n = 15)

# 5. Audit Country Intensities (Filtered for micro-states)
cat("\n======================================================\n")
cat("   GROUND-TRUTH AUDIT 5: Country Disparity (n_total >= 100)\n")
cat("======================================================\n")

cat("[DEBUG] Available grouping variables in dataset:\n")
cat(paste(unique(area_stats$grouping_var), collapse=", "), "\n\n")

country_stats <- area_stats %>%
  filter(grepl("countr|nev_name|admin", grouping_var, ignore.case = TRUE), !is.na(group)) %>%
  filter(as.numeric(n_total) >= 100) %>% # Filter out micro-states (< 10,000 sq km)
  group_by(group) %>%
  summarise(
    avg_relative_intensity = mean(relative_intensity, na.rm = TRUE),
    total_hotspots = sum(n_hot, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_relative_intensity))

print(country_stats, n = 15)
cat("======================================================\n\n")

sink()
cat("[INFO] Full audit summary successfully saved to:", out_file, "\n\n")
