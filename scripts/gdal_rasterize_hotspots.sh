#!/bin/bash
# ==============================================================================
# GDAL Rasterization Workflow for Hotspots
# Replaces the deprecated Python/rasterio approach to ensure perfect 1:1 grid 
# alignment, avoid ghost rasters, and leverage native GDAL C-level performance.
# ==============================================================================

# Base directories
DATA_DIR="data/processed/hotspots"
OUT_DIR="${DATA_DIR}/rasters"
mkdir -p "${OUT_DIR}"

# Columns to rasterize (Compound Risk count + Individual Services)
COLUMNS=(
    "hotspot_count"
    "Nature_Access"
    "Sed_Ret_Ratio"
    "Pollination"
    "C_Risk"
    "C_Risk_Red_Ratio"
    "N_export"
    "N_Ret_Ratio"
    "Sed_export"
)

# Metrics to process
METRICS=("abs" "pct")

for METRIC in "${METRICS[@]}"; do
    INPUT_GPKG="${DATA_DIR}/${METRIC}/global/hotspots_global_${METRIC}.gpkg"
    REPROJ_GPKG="${DATA_DIR}/${METRIC}/global/hotspots_global_${METRIC}_epsg8857.gpkg"

    echo "=========================================================="
    echo "Processing Metric: ${METRIC^^}"
    echo "=========================================================="

    if [ ! -f "${INPUT_GPKG}" ]; then
        echo "[WARNING] Input file not found: ${INPUT_GPKG}. Skipping."
        continue
    fi

    # Step 1: Reproject to Equal Earth (EPSG:8857) to ensure metric 10km cells
    echo "1. Reprojecting to Equal Earth (EPSG:8857)..."
    # -nln sets the layer name to match the file (helps GDAL find it easily)
    ogr2ogr -f "GPKG" -t_srs EPSG:8857 -nln "hotspots" "${REPROJ_GPKG}" "${INPUT_GPKG}"
    echo "✓ Reprojected vector saved to ${REPROJ_GPKG}"

    # Step 2: Rasterize each column
    echo "2. Rasterizing columns..."
    for COL in "${COLUMNS[@]}"; do
        OUT_TIF="${OUT_DIR}/${COL}_${METRIC}.tif"
        echo "   -> Rasterizing ${COL} to ${OUT_TIF}..."
        
        # -tr 10000 10000 specifies 10km x 10km target resolution
        # -ot Byte stores the 0/1 (or low counts) compactly to save space
        gdal_rasterize -l "hotspots" -a "${COL}" -tr 10000 10000 -a_nodata 255 -ot Byte -co COMPRESS=LZW "${REPROJ_GPKG}" "${OUT_TIF}"
    done
    echo "✓ Rasterization complete for ${METRIC^^}."
    echo ""
done

echo "=========================================================="
echo "All rasterization tasks finished! Outputs in ${OUT_DIR}"
echo "=========================================================="