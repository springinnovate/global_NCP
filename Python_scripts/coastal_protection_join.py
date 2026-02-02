from pathlib import Path
import os
import argparse

import geopandas as gpd
import numpy as np


def _set_geom(gdf):
    if "geom" in gdf.columns and gdf.geometry.name != "geom":
        gdf = gdf.set_geometry("geom")
    return gdf


def _default_interim():
    root = Path(os.environ.get("GLOBAL_NCP_DATA", "/home/jeronimo/data/global_ncp"))
    return root / "interim"


def parse_args():
    interim = _default_interim()
    parser = argparse.ArgumentParser(
        description="Join coastal protection point layers (1992 vs 2020) and compute deltas."
    )
    parser.add_argument(
        "--cp-1992",
        type=Path,
        default=interim / "c_protection_1992.gpkg",
        help="Input 1992 coastal protection points (GPKG).",
    )
    parser.add_argument(
        "--cp-2020",
        type=Path,
        default=interim / "c_protection_2020.gpkg",
        help="Input 2020 coastal protection points (GPKG).",
    )
    parser.add_argument(
        "--out",
        type=Path,
        default=interim / "c_protection_1992_2020_joined.gpkg",
        help="Output joined GPKG.",
    )
    return parser.parse_args()


def main():
    args = parse_args()
    cp_1992_path = args.cp_1992
    cp_2020_path = args.cp_2020

    print(f"Reading: {cp_1992_path}")
    cp_1992 = gpd.read_file(cp_1992_path)
    print(f"Reading: {cp_2020_path}")
    cp_2020 = gpd.read_file(cp_2020_path)

    cp_1992 = _set_geom(cp_1992)
    cp_2020 = _set_geom(cp_2020)

    # Ensure derived columns exist if missing from input
    for df in [cp_1992, cp_2020]:
        if "Rt_service" not in df.columns and "Rt" in df.columns and "Rt_nohab_all" in df.columns:
            df["Rt_service"] = df["Rt_nohab_all"] - df["Rt"]

        if "Rt_ratio" not in df.columns and "Rt_service" in df.columns and "Rt_nohab_all" in df.columns:
            df["Rt_ratio"] = np.where(
                df["Rt_nohab_all"] != 0,
                df["Rt_service"] / df["Rt_nohab_all"],
                0
            )

    cp_1992 = cp_1992.rename(
        columns={
            "Rt": "Rt_1992",
            "Rt_nohab_all": "Rt_nohab_all_1992",
            "Rt_service": "Rt_service_1992",
            "Rt_ratio": "Rt_ratio_1992",
        }
    )
    cp_2020 = cp_2020.rename(
        columns={
            "Rt": "Rt_2020",
            "Rt_nohab_all": "Rt_nohab_all_2020",
            "Rt_service": "Rt_service_2020",
            "Rt_ratio": "Rt_ratio_2020",
        }
    )

    if cp_1992.crs != cp_2020.crs:
        cp_2020 = cp_2020.to_crs(cp_1992.crs)

    # Fast join for identical point locations: merge on geometry bytes.
    cp_1992["geom_wkb"] = cp_1992.geometry.to_wkb()
    cp_2020["geom_wkb"] = cp_2020.geometry.to_wkb()

    drop_geom_col = cp_2020.geometry.name
    joined = cp_1992.merge(
        cp_2020.drop(columns=[drop_geom_col], errors="ignore"),
        on="geom_wkb",
        how="left",
    )
    joined = joined.drop(columns=["geom_wkb"])
    joined["Rt_diff_1992_2020"] = joined["Rt_1992"] - joined["Rt_2020"]
    joined["Rt_nohab_all_diff_1992_2020"] = (
        joined["Rt_nohab_all_1992"] - joined["Rt_nohab_all_2020"]
    )
    joined["Rt_pct_chg_1992_2020"] = np.where(
        joined["Rt_1992"] != 0,
        (joined["Rt_1992"] - joined["Rt_2020"]) / joined["Rt_1992"] * 100,
        np.nan,
    )
    joined["Rt_nohab_all_pct_chg_1992_2020"] = np.where(
        joined["Rt_nohab_all_1992"] != 0,
        (joined["Rt_nohab_all_1992"] - joined["Rt_nohab_all_2020"])
        / joined["Rt_nohab_all_1992"]
        * 100,
        np.nan,
    )
    joined["Rt_ratio_diff_1992_2020"] = joined["Rt_ratio_1992"] - joined["Rt_ratio_2020"]
    joined["Rt_ratio_pct_chg_1992_2020"] = np.where(
        joined["Rt_ratio_1992"] != 0,
        (joined["Rt_ratio_1992"] - joined["Rt_ratio_2020"])
        / joined["Rt_ratio_1992"]
        * 100,
        np.nan,
    )

    out_path = args.out
    joined.to_file(out_path, driver="GPKG")
    print(f"Wrote: {out_path}")


if __name__ == "__main__":
    main()
