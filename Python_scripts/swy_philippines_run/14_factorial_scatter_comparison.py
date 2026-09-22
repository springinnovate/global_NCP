"""Scatter plots for the CN/Kc factorial cells (her CN alone, her Kc alone, both together),
against the WWF-SIPA baseline, reusing 12_scatter_comparison.py's exact plotting style/logic and
the same explicit valid-in-both masks. Promoted here from a one-off scratchpad script (2026-09-21)
since the factorial check has now been reused across two sessions.

Outputs to docs/reports/swy/swy_ph_factorial_scatter_{qf,b}_{herCN,herKc,herCN_herKc}.png (only the
combinations that are actually informative -- QF with her Kc alone is pixel-identical to the main
NCP-Kc/CN-run QF plot, since Kc doesn't feed quickflow at all, so it's skipped here).
"""
import os

import numpy as np
import rasterio
from rasterio.warp import Resampling, reproject
import matplotlib.pyplot as plt

DATA_ROOT = os.environ.get("SWY_DATA_ROOT", "/data")

BASELINE_QF = f"{DATA_ROOT}/swy/philippines/rich_shared/workspace_swy_wwf_PH_baseline_historical_climate/QF_wwf_PH_baseline_historical_climate.tif"
BASELINE_B = f"{DATA_ROOT}/swy/philippines/comparison_maps/B_wwf_PH_baseline_historical_climate_masked.tif"
BASELINE_LULC_PATH = f"{DATA_ROOT}/swy/philippines/INPUTS_SP/ph_baseline_lulc_md5_7f29da.tif"
MASK_QF = f"{DATA_ROOT}/swy/philippines/comparison_maps/qf_valid_mask.tif"
MASK_B = f"{DATA_ROOT}/swy/philippines/comparison_maps/b_valid_mask.tif"
REF_GRID = f"{DATA_ROOT}/swy/philippines/workspace_becky_inputs_90m_snapped/QF_ph_becky_inputs_90m_snapped.tif"
OUT_DIR = "docs/reports/swy"

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

CELLS = [
    # (var, baseline_path, mask_path, ncp_path, out_name, title_suffix)
    ("QF", BASELINE_QF, MASK_QF,
     f"{DATA_ROOT}/swy/philippines/workspace_becky_inputs_90m_herCN_ourKc/QF_ph_becky_inputs_90m_herCN_ourKc.tif",
     "qf_herCN", "her CN + our Kc"),
    ("B", BASELINE_B, MASK_B,
     f"{DATA_ROOT}/swy/philippines/workspace_becky_inputs_90m_herCN_ourKc/B_ph_becky_inputs_90m_herCN_ourKc.tif",
     "b_herCN", "her CN + our Kc"),
    ("B", BASELINE_B, MASK_B,
     f"{DATA_ROOT}/swy/philippines/workspace_becky_inputs_90m_ourCN_herKc/B_ph_becky_inputs_90m_ourCN_herKc.tif",
     "b_herKc", "our CN + her Kc"),
    ("B", BASELINE_B, MASK_B,
     f"{DATA_ROOT}/swy/philippines/workspace_becky_inputs_90m_herCN_herKc/B_ph_becky_inputs_90m_herCN_herKc.tif",
     "b_herCN_herKc", "her CN + her Kc (both)"),
]


def resample_to_ref(src_path, resampling):
    with rasterio.open(REF_GRID) as ref:
        ref_transform, ref_crs, ref_shape = ref.transform, ref.crs, ref.shape
    with rasterio.open(src_path) as src:
        dst = np.full(ref_shape, np.nan, dtype="float64")
        reproject(source=rasterio.band(src, 1), destination=dst,
                  src_transform=src.transform, src_crs=src.crs,
                  dst_transform=ref_transform, dst_crs=ref_crs,
                  resampling=resampling, src_nodata=src.nodata, dst_nodata=np.nan)
    return dst


def make_plot(var, baseline_path, mask_path, ncp_path, out_name, title_suffix):
    with rasterio.open(mask_path) as m:
        mask = m.read(1) == 1
    baseline_resampled = resample_to_ref(baseline_path, Resampling.average)
    baseline_lulc = resample_to_ref(BASELINE_LULC_PATH, Resampling.mode)

    with rasterio.open(ncp_path) as src:
        ncp = src.read(1).astype("float64")
        ncp_nodata = src.nodata

    valid = mask & np.isfinite(ncp) & np.isfinite(baseline_resampled) & np.isfinite(baseline_lulc)
    if ncp_nodata is not None:
        valid &= ~np.isclose(ncp, ncp_nodata)

    x = baseline_resampled[valid]
    y = ncp[valid]
    lulc = baseline_lulc[valid].astype(int)
    n = x.size
    corr = float(np.corrcoef(x, y)[0, 1])

    rng = np.random.default_rng(42)
    idx = rng.choice(n, N_PLOT_SAMPLE, replace=False) if n > N_PLOT_SAMPLE else np.arange(n)

    fig, ax = plt.subplots(figsize=(7, 7), dpi=130)
    for code, name in LULC_CLASSES.items():
        m2 = lulc[idx] == code
        if m2.sum() == 0:
            continue
        ax.scatter(x[idx][m2], y[idx][m2], s=2, alpha=0.25, color=LULC_COLORS[code], label=name, linewidths=0)
    lims = [0, max(x[idx].max(), y[idx].max()) * 1.02]
    ax.plot(lims, lims, "k--", linewidth=1, label="1:1 line")
    ax.set_xlim(lims)
    ax.set_ylim(lims)
    ax.set_xlabel(f"WWF-SIPA baseline {var} (mm/yr)")
    ax.set_ylabel(f"NCP run ({title_suffix}) {var} (mm/yr)")
    ax.set_title(f"Philippines {var}: WWF-SIPA baseline vs. {title_suffix}\n(n={n:,} pixels, r={corr:.2f}, colored by WWF-SIPA LULC class)")
    legend = ax.legend(markerscale=6, fontsize=7, loc="upper left", framealpha=0.9)
    # Same fix as 12_scatter_comparison.py: force full-opacity legend swatches independent of the
    # scatter points' own alpha=0.25 (needed so overlapping clusters stay readable on the plot
    # itself) -- the low alpha made the legend key hard to read, especially for similar hues.
    handles = getattr(legend, "legend_handles", None) or legend.legendHandles
    for handle in handles:
        handle.set_alpha(1)
    fig.tight_layout()

    out_path = os.path.join(OUT_DIR, f"swy_ph_factorial_scatter_{out_name}.png")
    fig.savefig(out_path)
    plt.close(fig)
    print(f"{out_name}: n={n:,} r={corr:.3f} ratio={y.mean()/x.mean():.3f} -> {out_path}")


def main():
    for var, baseline_path, mask_path, ncp_path, out_name, title_suffix in CELLS:
        make_plot(var, baseline_path, mask_path, ncp_path, out_name, title_suffix)


if __name__ == "__main__":
    main()
