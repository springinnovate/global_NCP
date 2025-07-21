import rasterio

template_fp = "/home/jeronimo/OneDrive/global_NCP/data/input_rasters/LandCovers/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1_md5_2ed6285e6f8ec1e7e0b75309cc6d6f9f.tif"

with rasterio.open(template_fp) as src:
    print(f"Raster size: {src.width} x {src.height}")
    print(f"Total pixels: {src.width * src.height:,}")
    print(f"CRS: {src.crs}")
    print(f"Resolution: {src.res}")
