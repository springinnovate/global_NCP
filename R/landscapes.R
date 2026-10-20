# Landscapes used to describe the change maps in the paper ("Global Patterns of Change").
# Approximated from country, WWF biome and cell latitude/longitude (the grid carries no ecoregion
# field), e.g. the Cerrado as Brazil's tropical savanna biome. One definition for both the 10 km
# cell statistics (analysis/change_geography.R) and the native-pixel statistics
# (analysis/change_map_stats_native.R).
#
# d needs columns nev_name, WWF_biome, region_wb, lon, lat (cell centre, degrees).
assign_landscape <- function(d) {
  savanna <- "Tropical & Subtropical Grasslands, Savannas & Shrublands"
  moist   <- "Tropical & Subtropical Moist Broadleaf Forests"
  dry     <- "Tropical & Subtropical Dry Broadleaf Forests"
  ssa     <- d$region_wb %in% "Sub-Saharan Africa"
  with(d, dplyr::case_when(
    nev_name == "Brazil" & WWF_biome == savanna                                       ~ "Cerrado (Brazil, tropical savanna)",
    ((nev_name == "Paraguay" & WWF_biome == savanna) |
     (nev_name == "Argentina" & WWF_biome == savanna & lat > -31) |
     (nev_name == "Bolivia" & WWF_biome %in% c(savanna, dry) & lat < -18))          ~ "Chaco (Paraguay, N Argentina, SE Bolivia)",
    nev_name == "Brazil" & WWF_biome == moist & lon < -44                             ~ "Brazilian Amazon (moist forest west of 44 W)",
    ssa & WWF_biome == savanna & lat > 8 & lat < 18 & lon < 40                        ~ "Sahel and Sudanian savanna (8-18 N)",
    ssa & WWF_biome == savanna & lat < -5 & lat > -20                                 ~ "Southern African savanna, miombo (5-20 S)",
    ssa & WWF_biome == moist & abs(lat) < 5                                           ~ "Congo Basin and equatorial African forest (5 S-5 N)",
    nev_name %in% c("Indonesia", "Malaysia") & WWF_biome == moist                    ~ "Indonesia and Malaysia (moist forest)",
    nev_name %in% c("Cambodia", "Laos", "Vietnam", "Thailand", "Myanmar")             ~ "Mainland Southeast Asia",
    nev_name == "China" & lon > 100                                                   ~ "Eastern China (east of 100 E)",
    nev_name %in% c("India", "Bangladesh", "Pakistan", "Nepal")                       ~ "South Asia (India, Pakistan, Bangladesh, Nepal)",
    nev_name %in% c("Iran", "Iraq", "Turkey", "Syria", "Kazakhstan", "Uzbekistan",
                    "Turkmenistan", "Kyrgyzstan", "Tajikistan", "Afghanistan")        ~ "Middle East and Central Asia drylands",
    nev_name %in% c("Canada", "Russia") & WWF_biome == "Boreal Forests/Taiga"         ~ "Boreal Canada and Russia",
    nev_name == "United States of America" & lat < 50 & lon > -125                   ~ "Conterminous United States",
    region_wb == "Europe & Central Asia" & lon < 40 & lat > 35                        ~ "Europe (west of 40 E)",
    TRUE ~ NA_character_))
}

LANDSCAPE_ORDER <- c(
  "Chaco (Paraguay, N Argentina, SE Bolivia)", "Brazilian Amazon (moist forest west of 44 W)",
  "Cerrado (Brazil, tropical savanna)", "Indonesia and Malaysia (moist forest)", "Mainland Southeast Asia",
  "Sahel and Sudanian savanna (8-18 N)", "Southern African savanna, miombo (5-20 S)",
  "Congo Basin and equatorial African forest (5 S-5 N)", "Eastern China (east of 100 E)",
  "Middle East and Central Asia drylands", "Boreal Canada and Russia", "Conterminous United States",
  "Europe (west of 40 E)", "South Asia (India, Pakistan, Bangladesh, Nepal)", "World")
