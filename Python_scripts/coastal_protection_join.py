from pathlib import Path

import geopandas as gpd
import numpy as np


def _set_geom(gdf):
    if "geom" in gdf.columns and gdf.geometry.name != "geom":
        gdf = gdf.set_geometry("geom")
    return gdf


def main():
    cp_1992_path = Path(
        r"C:/Users/JerónimoRodríguezEsc/OneDrive - World Wildlife Fund, Inc/PROJECTS/Global_NCP/data/interim/c_protection_1992.gpkg"
    )
    cp_2020_path = Path(
        r"C:/Users/JerónimoRodríguezEsc/OneDrive - World Wildlife Fund, Inc/PROJECTS/Global_NCP/data/interim/c_protection_2020.gpkg"
    )

    print(f"Reading: {cp_1992_path}")
    cp_1992 = gpd.read_file(cp_1992_path)
    print(f"Reading: {cp_2020_path}")
    cp_2020 = gpd.read_file(cp_2020_path)

    cp_1992 = _set_geom(cp_1992)
    cp_2020 = _set_geom(cp_2020)

    cp_1992 = cp_1992.rename(
        columns={"Rt": "Rt_1992", "Rt_nohab_all": "Rt_nohab_all_1992"}
    )
    cp_2020 = cp_2020.rename(
        columns={"Rt": "Rt_2020", "Rt_nohab_all": "Rt_nohab_all_2020"}
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

    out_path = cp_1992_path.parent / "c_protection_1992_2020_joined.gpkg"
    joined.to_file(out_path, driver="GPKG")
    print(f"Wrote: {out_path}")


if __name__ == "__main__":
    main()
