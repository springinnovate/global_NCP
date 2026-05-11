import argparse
import fiona
import sys

def list_columns(filepath):
    """
    Lists the attribute column names from a vector file.

    This is a lightweight utility that uses fiona to read only the schema
    without loading the entire dataset into memory.

    Args:
        filepath (str): Path to the input vector file (e.g., GeoPackage).
    """
    try:
        with fiona.open(filepath, 'r') as source:
            # The properties of the schema is an ordered dictionary of columns
            columns = source.schema['properties'].keys()
            print(f"Columns in '{filepath}':")
            for col in columns:
                print(f"- {col}")
    except Exception as e:
        print(f"Error: Could not read file '{filepath}'.\nDetails: {e}", file=sys.stderr)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Lists all column names from a vector file's attribute table.")
    parser.add_argument("input_vector", help="Path to the input vector file (e.g., a GeoPackage).")
    args = parser.parse_args()
    list_columns(args.input_vector)