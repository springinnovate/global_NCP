inpath <-"/home/jeronimo/data/global_ncp/raw/Spring/Inspring" 
outpath <- "/home/jeronimo/OneDrive/Global_ES_mapping"
tiffes <- list.files(inpath, pattern=".tif")

sed_ret_92 <- rast(paste0(inpath, '/',tiffes[24]))
sed_ret_20 <- rast(paste0(inpath, '/',tiffes[25]))
 sed_ret_diff<- sed_ret_20-sed_ret_92
writeRaster(sed_ret_diff, paste0(outpath, '/', "sed_ret_diff.tif"))

n_export_92 <- rast(paste0(inpath, '/',tiffes[3]))
n_export_20 <- rast(paste0(inpath, '/',tiffes[4]))
n_export_diff<- n_export_20-n_export_92
writeRaster(n_export_diff, paste0(outpath, '/', "n_export_diff.tif"))