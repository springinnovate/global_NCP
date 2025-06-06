df_hotspots <-plt_long %>%
  group_by(service) %>%
  mutate(
    upper_threshold = quantile(pct_ch, 0.999, na.rm = TRUE),
    lower_threshold = quantile(pct_ch, 0.001, na.rm = TRUE),
    hotspot_flag = case_when(
      pct_ch >= upper_threshold ~ "high",
      pct_ch <= lower_threshold ~ "low",
      TRUE ~ NA_character_
    ),
    hotspot_binary = !is.na(hotspot_flag)
  ) %>%
  ungroup()