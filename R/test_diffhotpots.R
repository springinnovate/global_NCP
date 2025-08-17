# testing that the differences in the values for rotection, that at points a looked like the same whre actually diffeent. 
# 
# test <- plt %>%
#   select(fid, Rt_mean_pct_chg, Rt_nohab_mean_pct_chg, Rt_service_mean_pct_chg) %>%
#   mutate(
#     diff_rt_nohab = Rt_mean_pct_chg - Rt_nohab_mean_pct_chg,
#     diff_rt_service = Rt_mean_pct_chg - Rt_service_mean_pct_chg,
#     diff_nohab_service = Rt_nohab_mean_pct_chg - Rt_service_mean_pct_chg
#   ) %>%
#   summarise(
#     across(starts_with("diff"), list(
#       min = min,
#       max = max,
#       mean = mean,
#       sd = sd
#     ), na.rm = TRUE)
#   )
# 
# 
# ly <- st_layers('/Users/rodriguez/Library/CloudStorage/OneDrive-WorldWildlifeFund,Inc/Ref_Data/vector/grid10_km_c_f.gpkg')
# grid1 <- st_read('/Users/rodriguez/Library/CloudStorage/OneDrive-WorldWildlifeFund,Inc/Ref_Data/vector/grid_10km_ct.gpkg')
# ct <- st_read('/Users/rodriguez/Library/CloudStorage/OneDrive-WorldWildlifeFund,Inc/Ref_Data/vector/ee_r264_correspondence_fid.gpkg')
# 
# grid1 <- grid1 %>% filter(!is.na(country_fid))
# 
# 
# ct <- ct[c(1,12,22,25,26,27,31,35)]
# ct <- st_drop_geometry(ct)
# 
# grid1 <- left_join(grid1, ct, by = c("c_fid" = "id"))
# 
# 
# st_write(grid1,'/Users/rodriguez/Library/CloudStorage/OneDrive-WorldWildlifeFund,Inc/Ref_Data/vector/grid_10km_f.gpkg', append=FALSE)

# hist_plot <- ggplot(sf_f, aes(x = Rt_ratio_mean_pct_chg)) +
#   geom_histogram(bins = 30, fill = "steelblue", color = "black") +
#   labs(
#     title = "Distribution of pct_chg for C_Risk_Red_Ratio",
#     x = "% Change",
#     y = "Count"
#   ) +
#   theme_minimal()
# 
# print(hist_plot)
# 
# # 2. Extract summary statistics
# summary_stats <- sf_f %>%
#   summarise(
#     count = n(),
#     min = min(Rt_ratio_mean_pct_chg, na.rm = TRUE),
#     q1 = quantile(Rt_ratio_mean_pct_chg, 0.25, na.rm = TRUE),
#     median = median(Rt_ratio_mean_pct_chg, na.rm = TRUE),
#     mean = mean(Rt_ratio_mean_pct_chg, na.rm = TRUE),
#     q3 = quantile(Rt_ratio_mean_pct_chg, 0.75, na.rm = TRUE),
#     max = max(Rt_ratio_mean_pct_chg, na.rm = TRUE),
#     n_unique = n_distinct(Rt_ratio_mean_pct_chg)
#   )
# 
# print(summary_stats)
# 
# # Inspecting and cleaning weirdly skewed data in Rt_ratio_mean_pct_chg
# 
# # Step 1: Drop Inf/-Inf and NA
# sf_clean <- sf_f %>%
#   filter(
#     is.finite(Rt_ratio_mean_pct_chg),
#     !is.na(Rt_ratio_mean_pct_chg)
#   )
# 
# # Step 2: Drop geometry to simplify
# sf_tbl <- sf_clean %>% st_drop_geometry()
# 
# # Step 3: Summary stats
# summary_stats <- sf_tbl %>%
#   summarise(
#     count = n(),
#     min = min(Rt_ratio_mean_pct_chg),
#     q1 = quantile(Rt_ratio_mean_pct_chg, 0.25),
#     median = median(Rt_ratio_mean_pct_chg),
#     mean = mean(Rt_ratio_mean_pct_chg),
#     q3 = quantile(Rt_ratio_mean_pct_chg, 0.75),
#     max = max(Rt_ratio_mean_pct_chg),
#     n_unique = n_distinct(Rt_ratio_mean_pct_chg)
#   )
# 
# print(summary_stats)
# 
# # Step 4: Histogram with trimmed y-axis
# hist_plot <- ggplot(sf_tbl, aes(x = Rt_ratio_mean_pct_chg)) +
#   geom_histogram(bins = 100, fill = "steelblue", color = "white") +
#   scale_x_continuous(trans = "log10", labels = scales::comma_format()) +
#   labs(
#     title = "Distribution of % Change in Rt_ratio_mean (log-scaled)",
#     x = "% Change",
#     y = "Count"
#   ) +
#   theme_minimal()
# 
# print(hist_plot)