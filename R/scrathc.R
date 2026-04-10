lc_c <- st_read("C:/Users/JerónimoRodríguezEsc/OneDrive - World Wildlife Fund, Inc/PROJECTS/Global_NCP/data/interim/c_protection_1992_2020_joined.gpkg")
names(lc_c)


lc_metrics <- st_read("C:/Users/JerónimoRodríguezEsc/OneDrive - World Wildlife Fund, Inc/PROJECTS/Global_NCP/Final_outputs/10k_lcc_metrics.gpkg")
names(lc_metrics)

lcc_t <- st_read("C:/Users/JerónimoRodríguezEsc/OneDrive - World Wildlife Fund, Inc/PROJECTS/Global_NCP/data/processed/10k_lcc_granular_metrics_test.gpkg")


names(lcc_t)


synth_fid <- read.csv("C:/Users/JerónimoRodríguezEsc/projects/zonal_stats_toolkit/output_raw/grid_10km_20260219_224417.csv")
names(synth_fid)


library(sf)

k_ch_calc <- st_read("C:/Users/JerónimoRodríguezEsc/OneDrive - World Wildlife Fund, Inc/PROJECTS/Global_NCP/Final_outputs/10k_change_calc.gpkg")

k_ch_calc2 <- st_read("C:/Users/JerónimoRodríguezEsc/OneDrive - World Wildlife Fund, Inc/PROJECTS/Global_NCP/Final_outputs/10k_change_calc_v2.gpkg")


names(k_ch_calc)

names(k_ch_calc2)

unique()

head(k_ch_calc2)


per_ha <- rast("C:/Users/JerónimoRodríguezEsc/OneDrive - World Wildlife Fund, Inc/PROJECTS/Global_NCP/data/base_years_ha/global_n_export_tnc_esa1992_compressed_md5_728edc.tif")

plot(per_ha)
hist(per_ha)


access_csv <- read_csv("C:/Users/JerónimoRodríguezEsc/OneDrive - World Wildlife Fund, Inc/PROJECTS/Global_NCP/Final_outputs/grid_10km_20260306_213018.csv")

grid_10_base <- st_read("C:/Users/JerónimoRodríguezEsc/OneDrive - World Wildlife Fund, Inc/PROJECTS/Global_NCP/Final_outputs/grid_10km_land_synth_zonal_2026_03_09_22_06_33.gpkg")


grid_old <- st_read("C:/Users/JerónimoRodríguezEsc/OneDrive - World Wildlife Fund, Inc/PROJECTS/Global_NCP/Final_outputs/grid_10km_with_stats.gpkg")

grid_lc <- st_read("C:/Users/JerónimoRodríguezEsc/OneDrive - World Wildlife Fund, Inc/PROJECTS/Global_NCP/Final_outputs/10k_lcc_metrics.gpkg")

grid_old <- grid_old[,c(1:15)]
names(grid_10_base)
names(grid_old)

ee <- st_read("C:/Users/JerónimoRodríguezEsc/OneDrive - World Wildlife Fund, Inc/PROJECTS/Global_NCP/data/reference/cartographic_ee_r264_correspondence.gpkg")

head(grid_10_base)

head(grid_old)

names(access_csv)


grid_10_base <- grid_10_base %>% 
  mutate(temp_fid = row_number())

# For the old grid, we create the FID, select the specific columns we want to bring over, 
# and explicitly drop the geometry to force R to treat it as a lightweight dataframe.
grid_old_clean <- grid_old %>% 
  mutate(temp_fid = row_number()) %>%
  select(temp_fid, starts_with("mean_C_Risk")) %>% # Grab only the C_Risk columns
  st_drop_geometry() 

# 2. Perform the lightning-fast tabular join and clean up
grid_joined <- grid_10_base %>%
  left_join(grid_old_clean, by = "temp_fid") %>%
  select(-temp_fid) # Drop the temporary ID so it doesn't clutter your final output


grid_joined <- grid_joined %>%
  rename_with(
    ~ str_replace(.x, "^mean_(.*)", "//1_mean"), 
    starts_with("mean_")
  )
# 3. Export the combined result
# Using .rds for fast R loading later, or .gpkg if you need to open it in QGIS
st_write(grid_joined, "C:/Users/JerónimoRodríguezEsc/OneDrive - World Wildlife Fund, Inc/PROJECTS/Global_NCP/Final_outputs/10k_grid_services_base.gpkg", append = FALSE)
# st_write(grid_joined, "Final_outputs/grid_10km_joined.gpkg", append = FALSE)


grid_f <- st_read( "C:/Users/JerónimoRodríguezEsc/OneDrive - World Wildlife Fund, Inc/PROJECTS/Global_NCP/Final_outputs/10k_grid_services_base.gpkg")

exploded <- st_read("C:/Users/JerónimoRodríguezEsc/OneDrive - World Wildlife Fund, Inc/PROJECTS/Global_NCP/data/processed/_exploded_cartographic_regions.gpkg")


names(exploded)



hs <- st_read("C:/Users/JerónimoRodríguezEsc/OneDrive - World Wildlife Fund, Inc/PROJECTS/Global_NCP/Final_outputs/hotspots/abs/global/hotspots_global_abs.gpkg")
plot(hs$geom)
plot(hs)
hs %>%
  st_drop_geometry() %>% # Drops geometry to make it run instantly
  count(hotspot_count)




library(sf)
library(sf)

input_file <- "C:/Users/JerónimoRodríguezEsc/OneDrive - World Wildlife Fund, Inc/PROJECTS/Global_NCP/Final_outputs/hotspots/abs/global/hotspots_global_abs.gpkg"
output_file <- "C:/Users/JerónimoRodríguezEsc/OneDrive - World Wildlife Fund, Inc/PROJECTS/Global_NCP/Final_outputs/hotspots/abs/global/hotspots_global_abs_4326_wrapped.gpkg"

message("Reading file...")
hs <- st_read(input_file)

message("Projecting to WGS 84 (EPSG:4326)...")
hs_4326 <- st_transform(hs, 4326)

message("Wrapping dateline...")
hs_wrapped <- st_wrap_dateline(hs_4326, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))

message("Saving safe wrapped version...")
st_write(hs_wrapped, output_file, delete_dsn = TRUE)
message("Done! Load this new _4326_wrapped.gpkg file into your QGIS project.")
