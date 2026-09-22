"""Build the two 'hybrid' biophysical tables for a 2x2 CN x Kc factorial design.

Existing cells: (her CN, her Kc) = her own baseline run (already have, no rebuild needed);
(our CN, our Kc) = the NCP Kc/CN run (`workspace_becky_inputs_90m_snapped`, already have). This
script builds the CSV inputs for the two missing cells: (our CN, her Kc) and (her CN, our Kc) --
isolates how much of the QF/B gap traces to CN specifically vs. Kc specifically.

Both source tables (`ph_biophysical_table_ncp_kc_cn.csv`, `biophysical_template_PH_revised.csv`)
share the same row order and `lulc_id` values (07b's own docstring: it only ever overwrites CN_A-D
and Kc_1-12 on top of her original table, so every non-CN/Kc column -- usle_c, usle_p, root_depth,
nathab, affected_by_infra -- is already identical between them; only CN_A-D/Kc_1-12 actually
differ). Swapping columns is therefore a straight substitution, no re-indexing needed -- confirmed
by checking both files' columns/lulc_id order directly before writing this.
"""
import os

import pandas as pd

DATA_ROOT = os.environ.get("SWY_DATA_ROOT", "/data")
INPUTS_SP = os.path.join(DATA_ROOT, "swy/philippines/INPUTS_SP")

OURS_PATH = os.path.join(INPUTS_SP, "ph_biophysical_table_ncp_kc_cn.csv")
HERS_PATH = os.path.join(INPUTS_SP, "biophysical_template_PH_revised.csv")

CN_COLS = ["CN_A", "CN_B", "CN_C", "CN_D"]
KC_COLS = [f"Kc_{m}" for m in range(1, 13)]


def main():
    ours = pd.read_csv(OURS_PATH)
    hers = pd.read_csv(HERS_PATH)

    assert list(ours["lulc_id"]) == list(hers["lulc_id"]), (
        "lulc_id order differs between the two tables -- do not blindly swap columns"
    )

    # our CN, her Kc
    our_cn_her_kc = ours.copy()
    our_cn_her_kc[KC_COLS] = hers[KC_COLS].values
    out1 = os.path.join(INPUTS_SP, "ph_biophysical_table_ourCN_herKc.csv")
    our_cn_her_kc.to_csv(out1, index=False)
    print(f"written: {out1}")

    # her CN, our Kc
    her_cn_our_kc = hers.copy()
    her_cn_our_kc[KC_COLS] = ours[KC_COLS].values
    out2 = os.path.join(INPUTS_SP, "ph_biophysical_table_herCN_ourKc.csv")
    her_cn_our_kc.to_csv(out2, index=False)
    print(f"written: {out2}")

    # sanity print
    print("\nour_cn_her_kc CN_A/Kc_1 sample:")
    print(our_cn_her_kc[["lulc_id", "CN_A", "Kc_1"]].head(3))
    print("\nher_cn_our_kc CN_A/Kc_1 sample:")
    print(her_cn_our_kc[["lulc_id", "CN_A", "Kc_1"]].head(3))


if __name__ == "__main__":
    main()
