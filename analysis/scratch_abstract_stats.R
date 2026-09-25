# Scratch: compute global mean SPC per service + biome/region/income breakdowns
# for the Results 4.1 (Global Pattern of Change) narrative expansion Becky asked for.
suppressMessages({
  library(sf)
  library(dplyr)
  library(tidyr)
})

g <- st_read("data/processed/10k_change_calc.gpkg", quiet = TRUE,
             query = 'SELECT continent, "WWF_biome", region_wb, income_grp,
               c_risk_pct_chg, n_export_pct_chg, sed_export_pct_chg,
               pollination_pct_chg, nature_access_pct_chg
               FROM "10k_change_calc"')
df <- st_drop_geometry(g)
cat("rows before exclusion:", nrow(df), "\n")

df <- df %>% filter(!continent %in% c("Antarctica", "Seven seas (open ocean)"),
                     !WWF_biome %in% c("Lakes", "Rock & Ice"))
cat("rows after exclusion:", nrow(df), "\n")

services <- c(N_export = "n_export_pct_chg", Sed_export = "sed_export_pct_chg",
              C_Risk = "c_risk_pct_chg", Pollination = "pollination_pct_chg",
              Nature_Access = "nature_access_pct_chg")

cat("\n== GLOBAL MEAN % CHANGE PER SERVICE ==\n")
global_means <- sapply(services, function(col) mean(df[[col]], na.rm = TRUE))
print(round(global_means, 4))

summarize_by <- function(group_col) {
  df %>%
    group_by(.data[[group_col]]) %>%
    summarise(across(all_of(unname(services)), ~mean(.x, na.rm = TRUE)), n = n(), .groups = "drop") %>%
    rename(!!!setNames(unname(services), names(services)))
}

cat("\n== BY WWF_biome ==\n")
by_biome <- summarize_by("WWF_biome")
print(as.data.frame(by_biome), digits = 3)

cat("\n== BY region_wb ==\n")
by_region <- summarize_by("region_wb")
print(as.data.frame(by_region), digits = 3)

cat("\n== BY income_grp ==\n")
by_income <- summarize_by("income_grp")
print(as.data.frame(by_income), digits = 3)

# Which biome/region exceeds the GLOBAL MEAN by the most, per service (signed distance)
cat("\n== BIOME EXCEEDING GLOBAL MEAN BY MOST, PER SERVICE ==\n")
for (s in names(services)) {
  dev <- by_biome[[s]] - global_means[[s]]
  idx <- which.max(abs(dev))
  cat(sprintf("%s: %s (%.2f%% vs global %.2f%%)\n", s, by_biome$WWF_biome[idx], by_biome[[s]][idx]*100, global_means[[s]]*100))
}
cat("\n== REGION EXCEEDING GLOBAL MEAN BY MOST, PER SERVICE ==\n")
for (s in names(services)) {
  dev <- by_region[[s]] - global_means[[s]]
  idx <- which.max(abs(dev))
  cat(sprintf("%s: %s (%.2f%% vs global %.2f%%)\n", s, by_region$region_wb[idx], by_region[[s]][idx]*100, global_means[[s]]*100))
}

write.csv(by_biome, "analysis/scratch_by_biome_pct_chg.csv", row.names = FALSE)
write.csv(by_region, "analysis/scratch_by_region_pct_chg.csv", row.names = FALSE)
write.csv(by_income, "analysis/scratch_by_income_pct_chg.csv", row.names = FALSE)
cat("\nWritten scratch CSVs to analysis/\n")
