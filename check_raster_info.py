
import rasterio

def get_raster_info(filepath):
    try:
        with rasterio.open(filepath) as src:
            info = {
                "count": src.count,
                "width": src.width,
                "height": src.height,
                "dtype": src.dtypes[0],
                "nodata": src.nodata,
                "crs": src.crs.to_string(),
                "transform": src.transform,
                "block_shapes": src.block_shapes
            }
            return info
    except Exception as e:
        return {"error": str(e)}

retention_file = "/home/jeronimo/data/global_ncp/raw/Spring/Inspring/global_n_retention_ESAmar_1992_fertilizer_current_valid_md5_86031b.tif"
export_file = "/home/jeronimo/data/global_ncp/raw/Spring/Inspring/global_n_export_tnc_esa1992_compressed_md5_728edc.tif"

print(f"Info for {retention_file}:")
print(get_raster_info(retention_file))
print(f"Info for {export_file}:")
print(get_raster_info(export_file))
