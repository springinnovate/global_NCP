
# Load the rasters with the target data.
tiffes <- file.path(wd, list.files(wd, pattern="tif"))
#tiffes <-  tiffes[c(36:40, 46:50, 66:70, 86:90)]
tiffes <-  tiffes[c(1:5)]

# Step 1: Extract file names, product names, and country names
file_names <- basename(tiffes)  # Extract file names from paths

# Extract product names and country names
product_names <- sub("_[A-Z]+\\.tif$", "", file_names)  # Remove "_COUNTRY.tif" to get product name
country_names <- sub(".*_(.*?)\\.tif$", "\\1", file_names)  # Extract country name from file name

# Step 2: Create a dataframe to organize the information
file_info <- data.frame(
  FilePath = tiffes,
  Product = product_names,
  Country = country_names,
  stringsAsFactors = FALSE
)

# Step 3: Sort the dataframe by Country first, then by Product
file_info <- file_info %>%
  arrange(Country, Product)

# Step 4: Extract the sorted file paths
tiffes <- file_info$FilePath

baseES <- lapply(tiffes, rast)

num <- length(unique(product_names))
# Group the list of ES as nested list, one for eah country
baseES <- split(baseES, rep(1:length(lc), each = num))

# Step 5: Add the backgrounds

bkg <- map2(
  bkg, 
  baseES.,
  function(r1,r2){
    r1 <- resample(r1,r2, method = "near", , threads= 10)
  })


baseES <- map2(
  baseES,   # The list of raster sublists
  bkg,     # The list of backgrounds
  function(rasters, bkg) {
    # Apply mask to each raster in the sublist using the corresponding country vector
    lapply(rasters, function(raster) merge(raster, bkg))
  }
)

rm(lc)

# Step 2: Apply mask for each group of rasters using the corresponding country vector
baseES_ap <- map2(
  baseES,   # The list of raster sublists
  pa,     # The list of country polygons
  function(rasters, vector) {
    # Apply mask to each raster in the sublist using the corresponding country vector
    lapply(rasters, function(raster) mask(raster, vector))
  }
)
# "api" stands for "outside". I don´t know why I gave tho name, it makes no sense 
baseES_api <- map2(
  baseES,   # The list of raster sublists
  pa,     # The list of country polygons
  function(rasters, vector) {
    # Apply mask to each raster in the sublist using the corresponding country vector
    lapply(rasters, function(raster) mask(raster, vector, inverse=TRUE))
  }
)

# Step 3: Flatten back the list to get a single list of masked rasters
baseES_ap <- unlist(baseES_ap, recursive = FALSE)
baseES_api <- unlist(baseES_api, recursive = FALSE)
################################

# This extracts the background/the total area of interest. This is necessary because we need to know the share of the total area that is under the target 
# class/status (protected area, restoration, land cover whatever) as a share of the total. This is necessary because we want to compare this share with the share of the service/target data to check compare between inside/outside. This is the whole purpose of this part. 
msk_ap <- baseES_ap[1]   # there must be a smarter way to do this. It needs to work with different quantities, not only on a 3 step basis
msk_api <- baseES_api[1]

msk_ap <- lapply(msk_ap, function(r) {
  # Replace all non-NA values with 1 using ifel()
  ifel(!is.na(r), 1, NA)
})
msk_no_ap <- lapply(msk_api, function(r) {
  # Replace all non-NA values with 1 using ifel()
  r <- ifel(!is.na(r), 1, NA)
})

# add background to create binary mask 
msk_ap <- map2(msk_ap, bkg, merge)

```

# Extract the shares
#####################################################
# using lsm_c_ca requiues prjected coordoantes and ths can take too long to calcualte. Just count the pixels!
# freq_ap <- lapply(msk_ap, function(r){
#   f <- lsm_c_ca(r)
# })
freq_ap <- lapply(msk_ap, function(r){
  f <- freq(r)
})

country_names <- unique(country_names)

freq_ap <- lapply(seq_along(freq_ap), function(i) {
  freq_ap[[i]]$country <- country_names[i]  # Add country column
  freq_ap[[i]]  # Return the modified dataframe
})
freq_ap <- as_tibble(do.call(rbind, freq_ap))

freq_ap <- freq_ap %>%
  group_by(country) %>%
  mutate(share = (count / sum(count)) * 100) %>%
  ungroup()

#####################################################
# leave it here, but this has the frequencies. from here getting the shares is very easy. I always forget tho. 

# Create background masks ### I DON'T THINK THIS IS NECESSARYANYMORE. CAN BE REMOVED!
# msk_ap <- lapply(baseES_ap, function(r){
#   r <- classify(r, cbind(0, Inf, 1), others = NA)
# })
# 
# msk_api <- lapply(baseES_api, function(r){
#   r <- classify(r, cbind(0, Inf, 1), others = NA)
# })
# 
# # split again to have nested lists
# msk_ap <- split(msk_ap, rep(1:length(bkg), each = 3))
# msk_api <- split(msk_api, rep(1:length(bkg), each = 3))


##################################################################
##################################################################

#calculate ES output
# Define a function to compute the total sum for each SpatRaster
compute_sum <- function(raster) {
  total_sum <- global(raster, fun = "sum", na.rm = TRUE)[,1]  # Use global() to get the sum
  return(total_sum)
}

totSE_ap <-  lapply(baseES_ap, compute_sum)#, mc.cores = detectCores() - 4)
totSE_api <-  lapply(baseES_api, compute_sum)#, mc.cores = detectCores() - 4)


totSE_ap <- as_tibble(unlist(totSE_ap))
totSE_api <- as_tibble(unlist(totSE_api))
totSE <- cbind(file_info[c(2,3)], totSE_api, totSE_ap)

#rearrange columns 
names(totSE) <- c("Service", "Country","Outside", "Inside")
totSE <- left_join(totSE,freq_ap)#!!!!!!!!!


# dropoutside 
freq_ap <- freq_ap %>% filter(layer == 1)  %>%  filter(value == 1)
freq_ap <- freq_ap %>% select(!c(1,2))



names(totSE) <- c("Service", "Country","Outside", "Inside")
totSE <- left_join(totSE,freq_r)


# I need to add and then get the total!!!!!!!! 
totSE <- totSE %>% mutate(WholeArea= Outside+Inside)
# The freaking error is here!!!!!
totSE <- totSE %>% mutate(share_service= (Inside/WholeArea)*100)

names(freq_ap) <- c("pix_count_pa", "Country","share_area")
#rearrange columns 
freq_ap <- freq_ap %>% select(c(3,1,2))
names(totSE) <- c("Service", "Country","WholeArea", "PArea")
totSE <- left_join(totSE,freq_ap)


totSE <- totSE %>% mutate(share_service= (PArea/WholeArea)*100)

names(totSE) <- c("Service", "Country", "WholeArea", "PArea", "Share_Area", "pix_count_pa", "Share_Service")
#########################################################################
#calculate ES output


#rearrange columns 
names(totSE) <- c("Service", "Country","Outside", "Inside")
totSE <- left_join(totSE,freq_r)

# I need to add and then get the total!!!!!!!! 
totSE <- totSE %>% mutate(WholeArea= Outside+Inside)
# The freaking error is here!!!!!
totSE <- totSE %>% mutate(share_service= (Inside/WholeArea)*100)

# clean the service name:
totSE$Service <- sub("_.*", "", totSE$Service)
#rearrange columns 
totSE <- totSE %>%
  # Create a new column with simplified service names
  mutate(Service = case_when(
    Service == "global_n_retention_esamod2_compressed" ~ "Nitrogen Retention",
    Service == "global_sed_retention_esamod2_compressed" ~ "Sediment Retention",
    Service == "pollination_ppl_fed_on_ag_10s_esa2020" ~ "Pollination",
    Service == "nature_access_ESA" ~ "Access",
    Service == "access_ESA_2020" ~ "Access",
    TRUE ~ Service  # Keep other names unchanged, if any
  ))
write.csv(totSE, file=here('output_data', "service_prov_PA.csv"))


totSE_l <- totSE %>%
  select(Service, share_service, share_area, Country) %>%
  pivot_longer(cols = c(share_service, share_area),
               names_to = "Metric",
               values_to = "Value")


totSE_l <- totSE_l %>%
  filter(!(Metric == "share_area" & duplicated(paste(Country, Metric))))

totSE_l <- totSE_l %>% mutate(Service=case_when(
  Metric== "Share_Area" ~ "area",
  TRUE ~ Service 
))

# i don't think i should save this anymore 
save(totSE_l, file= here('output_data', 'serv_dist1.RData'))



totSE_l <- totSE_l %>%
  mutate(Service = fct_relevel(Service, 
                               "area",
                               "Nitrogen Retention", 
                               "Sediment Retention", 
                               "Pollination" 
  ))


totSE_l <- totSE_l %>%
  group_by(Country, Metric) %>%
  filter(!(Metric == "Share_Area" & row_number() > 1)) %>%
  ungroup()

###############################################################

baseES_api_v <- lapply(baseES_api, function(r){
  t <- as_tibble(values(r, na.rm=TRUE))
})

baseES_ap_v <- lapply(baseES_ap, function(r){
  t <- as_tibble(values(r, na.rm=TRUE))
})

# this lineIt adds the id for the service
# There is a better way to do it, check the next part 
file_info[1] <- NULL
baseES_api_v <- lapply(seq_along(baseES_api_v), function(i) {
  cbind(baseES_api_v[[i]], file_info[i,])
})

baseES_api_v <- lapply(baseES_api_v, function(r){
  r <- as_tibble(r)
})

baseES_ap_v <- lapply(seq_along(baseES_ap_v), function(i) {
  cbind(baseES_ap_v[[i]], file_info[i,])
})

baseES_ap_v <- lapply(baseES_ap_v, function(r){
  r <- as_tibble(r)
})
new_col <- "Value"
# Use lapply to rename the first column of each tibble
baseES_api_v <- lapply(baseES_api_v, function(tbl) {
  tbl %>%
    rename(!!new_col := 1)  # Rename first column using column position
})

baseES_ap_v <- lapply(baseES_ap_v, function(tbl) {
  tbl %>%
    rename(!!new_col := 1)  # Rename first column using column position
})

##################### do not run inside the Rmd!!!!!!!!
baseES_api_v <- do.call(rbind, baseES_api_v)
baseES_ap_v <- do.call(rbind, baseES_ap_v)
##################### do not run inside the Rmd!!!!!!!!
###########################################################
###########################################################


baseES_v <- baseES_api_v%>% mutate(set = "outside")
baseESap_v <- baseES_ap_v%>% mutate(set = "inside")
baseES_vf <- rbind(baseES_v, baseESap_v)
baseES_vf <- baseES_vf %>% rename(Service = Product)

# Change the name of the services 
baseES_vf <- baseES_vf %>%
  # Create a new column with simplified service names
  mutate(Service = case_when(
    Service == "global_n_retention_esamod2_compressed" ~ "Nitrogen Retention",
    Service == "global_sed_retention_esamod2_compressed" ~ "Sediment Retention",
    Service == "pollination_ppl_fed_on_ag_10s_esa2020" ~ "Pollination",
    Service == "nature_access_ESA" ~ "Nature Access",
    Service == "nature_access_ESA_2020" ~ "Nature Access",
    Service == "access_ESA_2020" ~ "Nature Access",
    TRUE ~ Service  # Keep other names unchanged, if any
  )) 

baseES_vf$Service <- factor(baseES_vf$Service, levels = c(
  "Nitrogen Retention", "Sediment Retention", "Pollination", "Nature Access"))

baseES_vf <- baseES_vf %>% distinct()




write.csv(baseES_vf, here('output_data', "distributionaccess_pa.csv"))


base_all <- read.csv(here('output_data', "distributionServ_pa.csv"))
base_all <- base_all[-1]

baseES_vf <- rbind(base_all, baseES_vf)
baseES_vf <- as_tibble(baseES_vf)

write.csv(baseES_vf, here('output_data', "distributionServ_pa.csv"))

```

esamod <- rast(here(inpath, 'ESAmodVCFv2_md5_05407ed305c24604eb5a38551cddb031.tif'))
esamod <- crop(esamod, poly)
esamod <- mask(esamod, poly)
writeRaster(esamod, paste0(here("ESA_LC"),'/', 'esaLC_', nam[2], '.tif'), overwrite=TRUE)

n <- 33 #this is therestoration for pollination
t <- rast(tiffes[[n]])
clean_filename <- function(filename) {
  sub("_md5.*", "", filename)
}
outpath <- here(outpath) 

cropped <-crop(t, poly)
masked <- mask(cropped, poly)

# Extract the base filename before 'md5'
base_filename <- clean_filename(basename(tiffes[[n]]))

# Write the cropped and masked rasters to files
writeRaster(masked, paste0(outpath, "/", base_filename, "_", nam[2], ".tif"), overwrite = TRUE)

#Coastal risk reduction This was addewd later. Check if there are some missing that i need to add.
c_risk_red<- rast(here(inpath, "ESAmodVCFv2_cv_habitat_value_md5_c01e9b17aee323ead79573d66fa4020d.tif"))

c_risk_red <- crop(c_risk_red, poly)
c_risk_red <- mask(c_risk_red, poly)
 writeRaster(c_risk_red, paste0(here(outpath, 'coastal_risk_reduction_'), nam[2], '.tif'), overwrite=TRUE)
#remember that the layer as such, keeps the name in a slot and that we are using that. 
#rest naturebase


outpath <- here('restoration_naturebase')

rest <- rast(here("restoration_naturebase", "nbsrestorationtest.tif"))
rest<- crop(rest, poly)
rest <- mask(rest, poly)
writeRaster(rest, paste0(here(outpath, 'restoration_'), nam[2], '.tif'), overwrite=TRUE)


wd <- here(inpath) # i know, this is stupid but not feel like thinking. 

This i
tiffes2 <- file.path(here(inpath), list.files(paste0(here(inpath)),pattern= '.tif$'))
tiffes <- file.path(here(inpath), list.files(paste0(here(inpath)),pattern= '2020-1992'))
tiffes <- file.path(here(inpath), list.files(paste0(here(inpath)),pattern= 'esamod'))
tiffes <- file.path(here(inpath), list.files(paste0(here(inpath)),pattern= 'ESAmod'))
tiffes <- file.path(here(inpath), list.files(paste0(here(inpath)),pattern= '.tif$'))

tiffes <- tiffes

tiffes <- tiffes[-c(4,5,7)]
tiffes <- tiffes[-4]

tiffes <- tiffes[-3]
tiffes <- tiffes[-2]


tiffes2 <- tiffes2[c(25,26)]
tiff# Loop through each geotiff file
for (i in seq_along(tiffes)) {
  # Get the current tiff file
  tiff_file <- tiffes[i]
  tiffes
  # Load the current geotiff (don't load all at once)
  access <- rast(tiff_file)
  
  # Apply crop and mask operations to each polygon
  cropped <-crop(access, poly)
  masked <- mask(cropped, poly)
  
  # Extract the base filename before 'md5'
  base_filename <- clean_filename(basename(tiff_file))
  
  # Write the cropped and masked rasters to files
    writeRaster(masked, paste0(here('cropped_raster_data'), "/", base_filename, "_", nam[2], ".tif"), overwrite = TRUE)
  }
# Optionally, clear the variable to free up memory
  rm(access, cropped, masked)
  gc()  # Garbage collection to clear memory
}



# Nature access: number of people within 1 hour travel
acc <- rast(here(inpath, 'nature_access_lspop2019_esa2020modVCFhab_md5_a6519ebd8b941444921e749da2e645bb.tif'))
#Load template. Where did I get it from? 
acc2 <- rast(here(inpath, 'access_WGS84.tif'))
acc <- project(acc,acc2)
writeRaster(acc, here(inpath, 'nature_access_lspop2019_esa2020modVCFhab_md5_a6519ebd8b941444921e749da2e645bb_WGS84.tif'))

  
  # Nature Accessibility Data Because i have to reproejcxt. check if there are some left to do. 
  # Has to be reprojected, so that is a bit annoying but no big deal, alrady done. I think the issue is that one is still pending

acc <- rast(here('Downloaded_data_ES', 'nature_access_lspop2019_esa2020modVCFhab_md5_a6519ebd8b941444921e749da2e645bb_WGS84.tif'))
acc <- crop(acc, poly)
acc <- mask(acc, poly)
writeRaster(acc, paste0(here('cropped_raster_data'),'/', 'nature_access_ESA_2020_', nam[2], '.tif'), overwrite=TRUE)

# Access restoration
acc <- rast(here('Downloaded_data_ES', 'nature_access_lspop2019_esa2020modVCFhab_md5_a6519ebd8b941444921e749da2e645bb.tif'))
#Load template. Where did I get it from? 
acc2 <- rast(here(inpath, 'access_WGS84.tif'))
acc <- project(acc,acc2)
writeRaster(acc, here(inpath, 'nature_access_lspop2019_esa2020modVCFhab_md5_a6519ebd8b941444921e749da2e645bb_WGS84.tif'))


# Nature Accessibility Data Because i have to reproejcxt. check if there are some left to do. 
# Has to be reprojected, so that is a bit annoying but no big deal, alrady done. I think the issue is that one is still pending
acc<- rast(here('Downloaded_data_ES','/nature_access_lspop2019_esa2020modVCFhab_md5_a6519ebd8b941444921e749da2e645bb_WGS84.tif'))
acc <- crop(acc, poly)
acc <- mask(acc, poly)
writeRaster(acc, paste0(here(outpath, 'nature_access_ESA_2020_'), nam[2], '.tif'), overwrite=TRUE)

# Access restoration
acces_rest <- rast(here(inpath, 'nature_access_diff_Sc3v1_PNVnoag-esa2020.tif'))
acces_rest <- project(acces_rest, acc, method='bilinear')
acces_rest <- crop(acces_rest, poly)
acces_rest <- mask(acces_rest, poly)
writeRaster(acces_rest, paste0(here(outpath, 'nature_access_diff_Sc3v1_PNVnoag-esa2020_'), nam[2], '.tif'), overwrite=TRUE)


# Access 60m 2019
acces_rest <- rast(here(inpath, 'global_people_access_population_2019_60.0m_md5_d264d371bd0d0a750b002a673abbb383.tif'))
acces_rest <- project(acces_rest, acc, method='bilinear')
acces_rest <- crop(acces_rest, poly)
acces_rest <- mask(acces_rest, poly)
writeRaster(acces_rest, paste0(here(outpath,  'global_people_access_population_2019_60.0m_'), nam[2], '.tif'), overwrite=TRUE)


  