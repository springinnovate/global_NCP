"""Build a PLACEHOLDER rain events table for the Borneo SWY test.

Real gap, not a full implementation: InVEST's actual rain-events derivation counts days with
>0.1mm rainfall per month from a DAILY precipitation product. This project's documented method
for that (`calculate_average_monthly_events.py`, described in docs/swy/research_notes.md as
"confirmed working, Earth Engine + CHIRPS daily") relies on Google Earth Engine access this
environment doesn't have configured, and the script file itself was not found in this repo as of
2026-09-10/11 — likely run previously in a Colab/notebook environment and never checked in.

What this script does instead: a rough, clearly-labeled estimate (18 rain days/month, uniform
across all 12 months) based on typical equatorial-climate rain-day frequency for Borneo — NOT
derived from this project's own CHIRPS data. This is good enough to let the model actually run
and check plausibility of the *other* real inputs, but this specific number should be replaced
with a genuine CHIRPS-daily-derived table (via AppEEARS or a direct CHIRPS daily download, same
pattern as the monthly precip already in this project) before treating any quickflow output as
more than a mechanical smoke test.
"""
import os

import pandas as pd

OUT_PATH = "data/borneo_lulc/rain_events_table.csv"
PLACEHOLDER_EVENTS_PER_MONTH = 18  # rough equatorial-climate estimate, NOT derived from data


def main():
    df = pd.DataFrame({"month": range(1, 13), "events": [PLACEHOLDER_EVENTS_PER_MONTH] * 12})
    os.makedirs(os.path.dirname(OUT_PATH), exist_ok=True)
    df.to_csv(OUT_PATH, index=False)
    print(df.to_string(index=False))
    print(f"\nWritten to {OUT_PATH} — PLACEHOLDER values, see module docstring.")


if __name__ == "__main__":
    main()
