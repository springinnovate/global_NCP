inpath <- '/home/jeronimo/global_ES_modeling/esos-c/data_2/ndr/global_dem_3s_md5_22d0c3809af491fa09d03002bdf09748'
outpath <- '/home/jeronimo/global_ES_modeling/esos-c/data/ndr'
output_vrt <- "/path/to/output/global_dem.vrt"
tiffes <- file.path(list.files(inpath, pattern="\\.tif$", full.names = TRUE))

# Build VRT
gdalbuildvrt(gdalfile = tiffes, output.vrt = paste0(outpath, '/', "global_dem.vrt"))

tiles <- lapply(tiffes,rast)
assembled <- do.call(terra::merge, tiles)
writeRaster(assembled, paste0(outpath,'/', 'DEM_global.tif'))

dem <- rast(paste0(outpath,'/', 'dem_global_3s.tif'))

############
# Fix nitrogen input

nitro <- rast("/Users/rodriguez/OneDrive - World Wildlife Fund, Inc/Global_ES_mapping/global_input_data/nci_current_n_app_extens_background_md5_42b028.tif")
# Create mask to get the values to subst 
rcl <- matrix(c(
  0, Inf, 0   # Any value from 0 to Infinity becomes 1
), ncol = 3, byrow = TRUE)

#create mask to get the values that i will have to replace (water and ice too, it does not matter as the model treats water as NAs at the end anyway)
msk <- classify(nitro, rcl)
# load base lc map
lc <- rast("/Users/rodriguez/Library/CloudStorage/OneDrive-WorldWildlifeFund,Inc/Global_ES_mapping/ESA_LC/ESA_LC_2020.tif")
#
lc <- crop(lc,msk)
lc <- subst(lc, from=c(210), to =NA)
lc <- mask(lc, msk, inverse=TRUE)
plot(lc)
lc <- trim(lc)
lc <- classify(lc, rcl)

nitro <- merge(nitro, lc)
writeRaster(nitro, "/Users/rodriguez/OneDrive - World Wildlife Fund, Inc/Global_ES_mapping/global_input_data/nci_current_n_app_extens_background_md5_42b028_filled.tif")


#######


lc_rast <- rast('/home/jeronimo/global_ES_modeling/esos-c/data/ndr/ESA_LC/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif', lyrs=10)
writeRaster(lc_rast, '/home/jeronimo/global_ES_modeling/esos-c/data/ndr/landcover_gl_2001.tif', overwrite=TRUE)
lc_rast <- rast('/home/jeronimo/global_ES_modeling/esos-c/data/ndr/ESA_LC/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif', lyrs=13)
writeRaster(lc_rast, '/home/jeronimo/global_ES_modeling/esos-c/data/ndr/landcover_gl_2004.tif', overwrite=TRUE)
lc_rast <- rast('/home/jeronimo/global_ES_modeling/esos-c/data/ndr/ESA_LC/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif', lyrs=16)
writeRaster(lc_rast, '/home/jeronimo/global_ES_modeling/esos-c/data/ndr/landcover_gl_2007.tif', overwrite=TRUE)
lc_rast <- rast('/home/jeronimo/global_ES_modeling/esos-c/data/ndr/ESA_LC/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif', lyrs=19)
writeRaster(lc_rast, '/home/jeronimo/global_ES_modeling/esos-c/data/ndr/landcover_gl_2010.tif', overwrite=TRUE)
lc_rast <- rast('/home/jeronimo/global_ES_modeling/esos-c/data/ndr/ESA_LC/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif', lyrs=22)
writeRaster(lc_rast, '/home/jeronimo/global_ES_modeling/esos-c/data/ndr/landcover_gl_2013.tif', overwrite=TRUE)



biph <- read.csv('/home/jeronimo/global_ES_modeling/esos-c/data_2/ndr/esa_biophysical_0916_md5_b2886c.csv')


sheds <- st_read('/home/jeronimo/global_ES_modeling/esos-c/data/ndr/watersheds_gl.shp')
sheds <- st_transform(sheds, crs= "EPSG:6933")
st_write(sheds, '/home/jeronimo/global_ES_modeling/esos-c/data/ndr/watersheds_gl_pr.shp')
?st_write





