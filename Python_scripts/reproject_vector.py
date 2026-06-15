import argparse
import geopandas as gpd
import os
import sys

def reproject_vector(input_path, output_path, target_crs):
    """
    Reprojects a vector file to a specified target CRS.
    """
    try:
        print(f"Reading vector data from: {input_path}")
        gdf = gpd.read_file(input_path)
        print(f"Source CRS detected: {gdf.crs.to_string()}")

        if gdf.crs == target_crs:
            print(f"Input file is already in the target CRS ({target_crs}). No reprojection needed.")
            if os.path.abspath(input_path) != os.path.abspath(output_path):
                print(f"Copying to {output_path}")
                os.makedirs(os.path.dirname(output_path), exist_ok=True)
                gdf.to_file(output_path, driver="GPKG")
            return

        print(f"Reprojecting to {target_crs}...")
        reprojected_gdf = gdf.to_crs(target_crs)
        print("Reprojection complete.")

        print(f"Writing reprojected vector to: {output_path}")
        os.makedirs(os.path.dirname(output_path), exist_ok=True)
        reprojected_gdf.to_file(output_path, driver="GPKG", layer=os.path.splitext(os.path.basename(output_path))[0])
        print("Process complete.")

    except Exception as e:
        print(f"An error occurred: {e}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Reprojects a vector file to a specified target CRS.")
    parser.add_argument("input_vector", help="Path to the input vector file.")
    parser.add_argument("output_vector", help="Path for the output reprojected vector file.")
    parser.add_argument("--crs", type=str, required=True, help="Target CRS for the output (e.g., 'EPSG:8857').")
    args = parser.parse_args()
    reproject_vector(args.input_vector, args.output_vector, args.crs)