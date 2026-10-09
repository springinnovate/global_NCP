#!/bin/bash

# This script provides a template for the robust rasterization workflow
# developed for the Global NCP project. It demonstrates how to:
# 1) Reproject a GeoPackage to EPSG:8857 (Equal Earth) using a Python script.
# 2) Rasterize a continuous attribute column (e.g., hotspot_count).
# 3) Rasterize multiple binary attribute columns (e.g., individual service hotspot flags) in a loop.
#
# Key principles:
# - Always reproject to an equal-area CRS (like EPSG:8857) first to ensure accurate 10km grid cells.
# - Use `gdal_rasterize` for direct, reliable vector-to-raster conversion.
# - Standardize output resolution to 10000x10000 meters (10km x 10km).
# - Use appropriate data types (`-ot`) and nodata values (`-a_nodata`).
# - Apply LZW compression (`-co "COMPRESS=LZW"`) for efficient storage.

# --- Configuration Variables (EDIT THESE) ---

# Input GeoPackage path (original, unprojected)
ORIGINAL_GPKG="/path/to/your/original_data.gpkg"

# Output GeoPackage path (reprojected to EPSG:8857)
REPROJECTED_GPKG="/path/to/your/reprojected_data_epsg8857.gpkg"

# Base output directory for rasters
OUTPUT_RASTER_DIR="/path/to/your/output/rasters/"

# Name of the continuous column to rasterize (e.g., 'hotspot_count', 'c_risk_abs_chg')
CONTINUOUS_COLUMN="your_continuous_column_name"

# Output TIFF filename for the continuous column
CONTINUOUS_RASTER_NAME="continuous_column_raster.tif"

# Data type for the continuous raster (e.g., Float32 for change values, Byte for counts)
CONTINUOUS_RASTER_DTYPE="Float32"

# NoData value for the continuous raster (e.g., -9999 for Float32, 255 for Byte)
CONTINUOUS_RASTER_NODATA="-9999"

# List of binary columns to rasterize (e.g., 'C_Risk', 'Pollination')
# These columns should contain 0s and 1s, or 1s and NAs.
BINARY_COLUMNS=("binary_col_1" "binary_col_2" "binary_col_3")

# Output subdirectory for binary rasters
BINARY_OUTPUT_SUBDIR="binary_flags/"

# NoData value for binary rasters (typically 0, so non-hotspots are transparent)
BINARY_RASTER_NODATA="0"

# --- End Configuration ---

# Create output directories if they don't exist
mkdir -p "$OUTPUT_RASTER_DIR"
mkdir -p "$OUTPUT_RASTER_DIR/$BINARY_OUTPUT_SUBDIR"

echo "--- Starting Rasterization Workflow ---"

# --- Step 1: Reproject the GeoPackage to EPSG:8857 ---
echo "1. Reprojecting GeoPackage: $ORIGINAL_GPKG to $REPROJECTED_GPKG"
python3 Python_scripts/reproject_vector.py \
   "$ORIGINAL_GPKG" \
   "$REPROJECTED_GPKG" \
    --crs EPSG:8857

if [ $? -ne 0 ]; then
    echo "Error during reprojection. Exiting."
    exit 1
fi

# --- Step 2: Rasterize a Continuous Column ---
echo "2. Rasterizing continuous column: $CONTINUOUS_COLUMN"
gdal_rasterize \
    -a "$CONTINUOUS_COLUMN" \
    -tr 10000 10000 \
    -a_nodata "$CONTINUOUS_RASTER_NODATA" \
    -ot "$CONTINUOUS_RASTER_DTYPE" \
    -a_srs EPSG:8857 \
    -co "COMPRESS=LZW" \
    "$REPROJECTED_GPKG" \
    "$OUTPUT_RASTER_DIR/$CONTINUOUS_RASTER_NAME"

if [ $? -ne 0 ]; then
    echo "Error during continuous column rasterization. Exiting."
    exit 1
fi

# --- Step 3: Rasterize Binary Columns in a Loop ---
echo "3. Rasterizing binary columns..."
for COL in "${BINARY_COLUMNS[@]}"; do
    echo "   - Rasterizing binary column: $COL"
    gdal_rasterize \
        -a "$COL" \
        -tr 10000 10000 \
        -a_nodata "$BINARY_RASTER_NODATA" \
        -ot Byte \
        -a_srs EPSG:8857 \
        -co "COMPRESS=LZW" \
        "$REPROJECTED_GPKG" \
        "$OUTPUT_RASTER_DIR/$BINARY_OUTPUT_SUBDIR/${COL}.tif"
    if [ $? -ne 0 ]; then
        echo "Error rasterizing binary column $COL. Continuing with next, but check logs."
    fi
done

echo "--- Rasterization Workflow Complete ---"

