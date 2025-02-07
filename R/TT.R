inpath <- '/home/jeronimo/global_ES_modeling/esos-c/data_2/ndr/global_dem_3s_md5_22d0c3809af491fa09d03002bdf09748'
outpath <- '/home/jeronimo/global_ES_modeling/esos-c/data_2/ndr'
output_vrt <- "/path/to/output/global_dem.vrt"
tiffes <- file.path(list.files(inpath, pattern="\\.tif$", full.names = TRUE))

# Build VRT
gdalbuildvrt(gdalfile = tiffes, output.vrt = paste0(outpath, '/', "global_dem.vrt"))



tiles <- lapply(tiffes,rast)
assembled <- do.call(terra::merge, tiles)
writeRaster(assembled, paste0(outpath,'/', 'DEM_global.tif'))


lc_rast <- rast('/home/jeronimo/global_ES_modeling/esos-c/data_2/ndr/ESA_LC/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif', lyrs=1)
writeRaster(lc_rast, '/home/jeronimo/global_ES_modeling/esos-c/data_2/ndr/landcover_gl_1.tif')

biph <- read.csv('/home/jeronimo/global_ES_modeling/esos-c/data_2/ndr/esa_biophysical_0916_md5_b2886c.csv')
