import argparse
import geopandas as gpd
import numpy as np
import sys

def verify_grid(filepath):
    """
    Verifies the dimensions and, more importantly, the area of grid cells
    in a vector file, accounting for different CRS types.
    """
    print("--> Entered verify_grid function.")
    try:
        print(f"Checking grid cells in: {filepath}")
        gdf = gpd.read_file(filepath)
        print(f"--> Successfully read file. CRS: {gdf.crs}")

        if gdf.empty:
            print("!!! ERROR: The GeoPackage is empty. Cannot verify grid cells. !!!", file=sys.stderr)
            return

        first_geom = gdf.geometry.iloc[0]
        print("--> Accessed first geometry.")

        # --- 1. Bounding Box Dimensions (can be misleading) ---
        print("--> Calculating bounding box...")
        bounds = first_geom.bounds
        width = bounds[2] - bounds[0]
        height = bounds[3] - bounds[1]
        print("\n--- Bounding Box Dimensions ---")
        if gdf.crs.is_geographic:
            print(f"Width: {width:.4f} [DEGREES]")
            print(f"Height: {height:.4f} [DEGREES]")
        else:
            print(f"Width: {width:.2f} meters")
            print(f"Height: {height:.2f} meters")
        print("NOTE: Bounding box dimensions can be misleading after reprojection.")

        # --- 2. Area Calculation (the correct method for equal-area grids) ---
        print("--> Calculating area...")
        area_native_units = first_geom.area
        print("--> Area calculation complete.")
        print("\n--- Polygon Area ---")
        if gdf.crs.is_geographic:
            print(f"Area in native units: {area_native_units:.6f} [SQUARE DEGREES]")
        else:
            area_sq_km = area_native_units / 1_000_000
            print(f"Area in native units: {area_native_units:,.2f} square meters")
            print(f"Area in square km: {area_sq_km:.2f} km^2")
            is_100_sq_km = np.isclose(area_sq_km, 100, atol=5) # Allow tolerance
            print(f"Is area ~100 sq km? -> {is_100_sq_km}")

    except Exception as e:
        print(f"!!! An error occurred: {e} !!!", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    print("--> Script started.")
    parser = argparse.ArgumentParser(description="Verifies the dimensions and area of grid cells in a vector file.")
    parser.add_argument("input_vector", help="Path to the input vector file.")
    args = parser.parse_args()
    print(f"--> Arguments parsed. Input file: {args.input_vector}")
    verify_grid(args.input_vector)
    print("--> Script finished.")