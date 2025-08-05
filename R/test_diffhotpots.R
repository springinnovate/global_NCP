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
