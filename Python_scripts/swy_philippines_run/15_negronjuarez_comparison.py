"""Compare the Negron Juarez forest-Kc test run (09i) against WWF-SIPA's baseline, same pattern
as `12_scatter_comparison.py` and the earlier CN/Kc factorial series (`14_factorial_scatter_
comparison.py`). The real question: does this close more of the "our CN + her Kc" cell's remaining
B gap (ratio 1.21, r=0.82, from the 2026-09-21 factorial) on its own, without borrowing her Kc.

QF is included for a sanity check only -- Kc doesn't feed QF, so it should be ~identical to
09e_run_swy_ph_snapped_grid.py's own QF (same CN, same DEM/grid, only Kc changed).
"""
import os

import numpy as np
import rasterio
from rasterio.warp import Resampling, reproject

NCP_WORKSPACE = "data/swy/philippines/workspace_becky_inputs_90m_negronjuarez_kc"
NCP_SUFFIX = "negronjuarez_forest_kc"
BASELINE_WORKSPACE = (
    "data/swy/philippines/rich_shared/workspace_swy_wwf_PH_baseline_historical_climate"
)
BASELINE_LULC_PATH = "data/swy/philippines/INPUTS_SP/ph_baseline_lulc_md5_7f29da.tif"
MASK_DIR = "data/swy/philippines/comparison_maps"

LULC_CLASSES = {
    1: "Annual Crop", 2: "Brush/Shrubs", 3: "Built-up", 4: "Closed Forest", 5: "Fishpond",
    6: "Grassland", 7: "Inland Water", 8: "Mangrove Forest", 9: "Marshland/Swamp",
    10: "Open Forest", 11: "Open/Barren", 12: "Perennial Crop",
}
LULC_COLORS = {
    1: "#e6ab02", 2: "#a6761d", 3: "#666666", 4: "#1b9e77", 5: "#377eb8",
    6: "#66a61e", 7: "#1f78b4", 8: "#7570b3", 9: "#984ea3", 10: "#4daf4a",
    11: "#e7298a", 12: "#d95f02",
}

N_PLOT_SAMPLE = 300_000


def _resample_to_ref(src_path, ref_path, resampling):
    with rasterio.open(ref_path) as ref:
        ref_transform, ref_crs, ref_shape = ref.transform, ref.crs, ref.shape
    with rasterio.open(src_path) as src:
        dst = np.full(ref_shape, np.nan, dtype="float64")
        reproject(
            source=rasterio.band(src, 1), destination=dst,
            src_transform=src.transform, src_crs=src.crs,
            dst_transform=ref_transform, dst_crs=ref_crs,
            resampling=resampling, src_nodata=src.nodata, dst_nodata=np.nan,
        )
    return dst


def run_pair(var, baseline_path, ncp_path, out_png):
    with rasterio.open(ncp_path) as src:
        ncp = src.read(1).astype("float64")
        ncp_nodata = src.nodata

    baseline_resampled = _resample_to_ref(baseline_path, ncp_path, Resampling.average)
    baseline_lulc_resampled = _resample_to_ref(BASELINE_LULC_PATH, ncp_path, Resampling.mode)

    with rasterio.open(os.path.join(MASK_DIR, f"{var.lower()}_valid_mask.tif")) as mask_src:
        comparison_mask = mask_src.read(1) == 1

    valid = np.isfinite(ncp) & np.isfinite(baseline_resampled)
    if ncp_nodata is not None:
        valid &= ~np.isclose(ncp, ncp_nodata)
    valid &= np.isfinite(baseline_lulc_resampled)
    valid &= comparison_mask

    x = baseline_resampled[valid]
    y = ncp[valid]
    lulc = baseline_lulc_resampled[valid].astype(int)

    n = x.size
    corr = float(np.corrcoef(x, y)[0, 1])
    slope, intercept = np.polyfit(x, y, 1)
    print(f"{var}: n={n} corr={corr:.3f} slope={slope:.3f} intercept={intercept:.2f}")
    print(f"{var}: baseline mean={x.mean():.1f} ncp mean={y.mean():.1f} ratio(ncp/baseline)={y.mean()/x.mean():.3f}")

    print(f"{var} by WWF-SIPA LULC class (baseline_mean, ncp_mean, ratio, n_pixels):")
    for code, name in LULC_CLASSES.items():
        m = lulc == code
        if m.sum() < 100:
            continue
        hx, hy = x[m], y[m]
        print(f"  {name:18s} baseline={hx.mean():8.1f} ncp={hy.mean():8.1f} ratio={hy.mean()/hx.mean():.3f} n={m.sum()}")

    rng = np.random.default_rng(42)
    idx = rng.choice(n, N_PLOT_SAMPLE, replace=False) if n > N_PLOT_SAMPLE else np.arange(n)

    import matplotlib.pyplot as plt

    fig, ax = plt.subplots(figsize=(7, 7), dpi=130)
    for code, name in LULC_CLASSES.items():
        m = lulc[idx] == code
        if m.sum() == 0:
            continue
        ax.scatter(x[idx][m], y[idx][m], s=2, alpha=0.25, color=LULC_COLORS[code], label=name, linewidths=0)
    lims = [0, max(x[idx].max(), y[idx].max()) * 1.02]
    ax.plot(lims, lims, "k--", linewidth=1, label="1:1 line")
    ax.set_xlim(lims)
    ax.set_ylim(lims)
    ax.set_xlabel(f"WWF-SIPA baseline {var} (mm/yr)")
    ax.set_ylabel(f"NCP CN + Negron Juarez forest Kc {var} (mm/yr)")
    ax.set_title(f"Philippines {var}: WWF-SIPA baseline vs. NCP CN + Negron Juarez forest Kc\n(n={n:,} pixels, r={corr:.2f})")
    legend = ax.legend(markerscale=6, fontsize=7, loc="upper left", framealpha=0.9)
    handles = getattr(legend, "legend_handles", None) or legend.legendHandles
    for handle in handles:
        handle.set_alpha(1)
    fig.tight_layout()
    fig.savefig(out_png)
    print(f"Written to {out_png}")


def main():
    run_pair(
        "QF",
        f"{BASELINE_WORKSPACE}/QF_wwf_PH_baseline_historical_climate.tif",
        f"{NCP_WORKSPACE}/QF_ph_{NCP_SUFFIX}.tif",
        "docs/reports/swy/swy_ph_negronjuarez_scatter_qf.png",
    )
    run_pair(
        "B",
        "data/swy/philippines/comparison_maps/B_wwf_PH_baseline_historical_climate_masked.tif",
        f"{NCP_WORKSPACE}/B_ph_{NCP_SUFFIX}.tif",
        "docs/reports/swy/swy_ph_negronjuarez_scatter_b.png",
    )


if __name__ == "__main__":
    main()
