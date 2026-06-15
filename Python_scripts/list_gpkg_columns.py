import argparse
import sqlite3
import sys

def list_columns(filepath):
    """
    Lists the attribute column names from a vector file.
    This is a lightweight utility that uses the built-in sqlite3 library to read 
    GeoPackage schemas directly without requiring GDAL or Fiona.
    Args:
        filepath (str): Path to the input vector file (e.g., GeoPackage).
    """
    try:
        conn = sqlite3.connect(filepath)
        cursor = conn.cursor()
        cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'gpkg_%' AND name NOT LIKE 'sqlite_%' AND name NOT LIKE 'rtree_%'")
        tables = cursor.fetchall()
        for table in tables:
            print(f"Columns in table '{table[0]}' of '{filepath}':")
            cursor.execute(f"PRAGMA table_info({table[0]})")
            for col in cursor.fetchall():
                print(f"- {col[1]} ({col[2]})")
        conn.close()
    except Exception as e:
        print(f"Error: Could not read file '{filepath}'.\nDetails: {e}", file=sys.stderr)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Lists all column names from a vector file's attribute table.")
    parser.add_argument("input_vector", help="Path to the input vector file (e.g., a GeoPackage).")
    args = parser.parse_args()
    list_columns(args.input_vector)