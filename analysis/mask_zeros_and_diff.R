library(terra)

# --- 1. Setup Paths ---
# Using the path provided in your request
data_dir <- "C:/Users/JerónimoRodríguezEsc/OneDrive - World Wildlife Fund, Inc/PROJECTS/Global_NCP/data/Raw"

# Find the files (assuming they are the only ones with 1992/2020 in the name in that folder)
f_1992 <- list.files(data_dir, pattern = "1992.*\\.tif$", full.names = TRUE)
f_2020 <- list.files(data_dir, pattern = "2020.*\\.tif$", full.names = TRUE)

# Safety check
if(length(f_1992) == 0) stop("1992 file not found in the specified directory.")
if(length(f_2020) == 0) stop("2020 file not found in the specified directory.")

# Load Rasters
# Taking the first match if multiple exist, but usually there should be one per year
r_1992 <- rast(f_1992[1])
r_2020 <- rast(f_2020[1])

# --- 2. Create Mask (Pixels that are 0 in BOTH) ---
# We identify pixels where 1992 is 0 AND 2020 is 0
mask_zeros <- (r_1992 == 0) & (r_2020 == 0)

# Export the mask
writeRaster(mask_zeros, file.path(data_dir, "mask_zeros_both.tif"), overwrite = TRUE)

# --- 3. Apply Mask (Set those pixels to NA) ---
# mask() function: where mask_zeros is 1 (TRUE), set the value to NA
r_1992_masked <- mask(r_1992, mask_zeros, maskvalues = 1, updatevalue = NA)
r_2020_masked <- mask(r_2020, mask_zeros, maskvalues = 1, updatevalue = NA)

# Export Masked Rasters with "_masked" suffix
out_1992 <- gsub("\\.tif$", "_masked.tif", f_1992[1])
out_2020 <- gsub("\\.tif$", "_masked.tif", f_2020[1])

writeRaster(r_1992_masked, out_1992, overwrite = TRUE)
writeRaster(r_2020_masked, out_2020, overwrite = TRUE)

# --- 4. Calculate Difference (Latest - Earliest) ---
diff_raster <- r_2020_masked - r_1992_masked

# Export Difference
writeRaster(diff_raster, file.path(data_dir, "difference_2020_minus_1992.tif"), overwrite = TRUE)

print("Processing complete. Files exported to data directory.")
