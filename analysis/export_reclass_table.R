# ==============================================================================
# Export LCC Reclassification Table for Presentation
# ==============================================================================

library(readr)
library(here)

# 1. Define the data exactly as it appears in the methodology
reclass_data <- data.frame(
  ESA_CCI_Raw_Classes = c(
    "10, 11, 12, 20, 30, 40 (Rainfed & Irrigated Cropland)",
    "190 (Urban / Built-up)",
    "50, 60-62, 70-72, 80-82, 90, 100 (Tree Cover)",
    "160, 170 (Flooded Trees / Mangroves)*",
    "110, 120-122, 130 (Grassland / Shrubland)",
    "140, 150-153 (Sparse Vegetation)",
    "180 (Shrub or herbaceous cover, flooded)",
    "200-202 (Bare areas)",
    "210 (Water bodies)",
    "220 (Permanent snow and ice)"
  ),
  Simplified_9_Class_System = c(
    "1: Cropland",
    "6: Urban",
    "2: Forest",
    "2: Forest",
    "3: Grassland/Shrubland",
    "4: Sparse",
    "5: Wetland",
    "7: Bare",
    "8: Water",
    "9: Snow/Ice"
  ),
  Binary_Classification = c(
    "Transformed (Human Footprint)",
    "Transformed (Human Footprint)",
    "Natural (Primary)",
    "Natural (Primary)",
    "Transformed (Rangeland/Pasture)",
    "Natural",
    "Natural",
    "Natural",
    "Natural",
    "Natural"
  )
)

# 2. Define output path and export
out_dir <- here("outputs", "tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_file <- file.path(out_dir, "lcc_reclassification_table.csv")

write_csv(reclass_data, out_file)
message("Saved presentation-ready LCC reclassification table to: ", out_file)