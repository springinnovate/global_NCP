"""How much of the world's cropland falls in each Köppen climate x crop group cell?

Scoping for a global SWY cropland parameterization (docs/swy/cn_kc_biome_coverage_registry.md,
bucket 2): cropland CN/Kc corrections would be looked up by climate zone x crop group x water
regime. Before building that table, this measures how many cells actually matter, i.e. how few
climate x crop combinations cover most of the global crop area.

- Climate: Beck et al. (2018) Köppen-Geiger, 1km, 30 classes, aggregated to MapSPAM's 5-arcmin
  grid by majority (exactly 10x10 1km pixels per MapSPAM cell).
- Crops: MapSPAM 2020 v2 physical area (ha), all 46 crops, "ALL" technology (irrigated + rainfed
  combined; the irrigated/rainfed split is not downloaded yet, so water regime isn't a key here).
  Grouped into six hydrologic groups, roughly following TR-55 cropland treatments.

Outputs (data/swy/shared/scoping/):
    koppen_crop_area.csv          area (Mha) per Köppen class x crop group
    koppen_main_crop_area.csv     same, Köppen main group (A/B/C/D/E) x crop group
"""
import glob
import os

import numpy as np
import pandas as pd
import rasterio
from rasterio.enums import Resampling

KOPPEN_PATH = "data/swy/shared/koppen_geiger/Beck_KG_V1_present_0p0083.tif"
MAPSPAM_DIR = "data/swy/shared/mapspam/raw_downloads"
OUT_DIR = "data/swy/shared/scoping"

KOPPEN = {
    1: "Af", 2: "Am", 3: "Aw", 4: "BWh", 5: "BWk", 6: "BSh", 7: "BSk", 8: "Csa", 9: "Csb",
    10: "Csc", 11: "Cwa", 12: "Cwb", 13: "Cwc", 14: "Cfa", 15: "Cfb", 16: "Cfc", 17: "Dsa",
    18: "Dsb", 19: "Dsc", 20: "Dsd", 21: "Dwa", 22: "Dwb", 23: "Dwc", 24: "Dwd", 25: "Dfa",
    26: "Dfb", 27: "Dfc", 28: "Dfd", 29: "ET", 30: "EF",
}
MAIN_GROUP = {"A": "A tropical", "B": "B dry", "C": "C temperate", "D": "D continental", "E": "E polar"}

CROP_GROUPS = {
    "paddy rice": ["RICE"],  # includes upland rice; the irrigated/rainfed split would separate it
    "small grains": ["WHEA", "BARL", "OCER", "RAPE"],
    "pulses": ["BEAN", "CHIC", "COWP", "LENT", "PIGE", "OPUL"],
    "sugarcane": ["SUGC"],
    "tree & perennial": ["BANA", "PLNT", "CITR", "CNUT", "COCO", "COFF", "RCOF", "OILP", "RUBB",
                         "TEAS", "TEMF", "TROF"],
    "row crops": ["MAIZ", "SORG", "MILL", "PMIL", "COTT", "SOYB", "SUNF", "GROU", "SESA", "OOIL",
                  "OFIB", "TOBA", "VEGE", "POTA", "SUGB", "TOMA", "ONIO", "CASS", "SWPO", "YAMS",
                  "ORTS", "REST"],
}


def main():
    os.makedirs(OUT_DIR, exist_ok=True)
    crop_to_group = {c: g for g, cs in CROP_GROUPS.items() for c in cs}

    with rasterio.open(os.path.join(MAPSPAM_DIR, "RICE.tif")) as ref:
        shape = ref.shape
    with rasterio.open(KOPPEN_PATH) as src:
        koppen = src.read(1, out_shape=shape, resampling=Resampling.mode)
    print(f"Köppen aggregated to {koppen.shape}")

    group_area = {}
    seen = set()
    for path in sorted(glob.glob(f"{MAPSPAM_DIR}/*.tif")):
        crop = os.path.basename(path)[:-4]
        group = crop_to_group.get(crop)
        assert group is not None, f"crop {crop} not assigned to a group"
        seen.add(crop)
        with rasterio.open(path) as src:
            arr = src.read(1).astype("float64")
            arr[~np.isfinite(arr) | (arr < 0)] = 0
        group_area[group] = group_area.get(group, 0) + arr
    missing = set(crop_to_group) - seen
    print(f"crops read: {len(seen)}; listed but not on disk: {sorted(missing) or 'none'}")

    rows = []
    for group, area in group_area.items():
        by_class = np.bincount(koppen.ravel(), weights=area.ravel(), minlength=31)
        for k, name in KOPPEN.items():
            rows.append({"koppen": name, "main": MAIN_GROUP[name[0]], "crop_group": group,
                         "area_mha": by_class[k] / 1e6})
        unclassified = by_class[0] / 1e6
        if unclassified > 0.01:
            print(f"  {group}: {unclassified:.2f} Mha in cells with no Köppen class (coast/water)")

    df = pd.DataFrame(rows)
    total = df.area_mha.sum()
    print(f"\nTotal classified crop physical area: {total:,.0f} Mha")

    main = df.groupby(["main", "crop_group"], as_index=False).area_mha.sum()
    main["share"] = main.area_mha / total
    main = main.sort_values("area_mha", ascending=False).reset_index(drop=True)
    main["cumulative_share"] = main.share.cumsum()
    main.to_csv(os.path.join(OUT_DIR, "koppen_main_crop_area.csv"), index=False)

    fine = df[df.area_mha > 0].copy()
    fine["share"] = fine.area_mha / total
    fine = fine.sort_values("area_mha", ascending=False).reset_index(drop=True)
    fine["cumulative_share"] = fine.share.cumsum()
    fine.to_csv(os.path.join(OUT_DIR, "koppen_crop_area.csv"), index=False)

    print("\n== Köppen main group x crop group (sorted by area) ==")
    print(main.round(3).to_string())
    print("\n== Main group x crop group: area (Mha) ==")
    print(main.pivot(index="main", columns="crop_group", values="area_mha").round(0).to_string())
    for thr in (0.5, 0.8, 0.9):
        n_main = int((main.cumulative_share < thr).sum()) + 1
        n_fine = int((fine.cumulative_share < thr).sum()) + 1
        print(f"cells covering {thr:.0%} of crop area: {n_main} of {len(main)} (main groups), "
              f"{n_fine} of {len(fine)} (30 Köppen classes)")
    print("\n== Top 15 Köppen class x crop group cells ==")
    print(fine.head(15).round(3).to_string())


if __name__ == "__main__":
    main()
