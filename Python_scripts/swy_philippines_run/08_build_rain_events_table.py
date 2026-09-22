"""Build a PLACEHOLDER rain events table for the Philippines SWY test.

Mirrors `swy_borneo_run/08_build_rain_events_table.py`'s approach and its limitation — real gap,
not a full implementation, same as Borneo: the documented derivation method
(`calculate_average_monthly_events.py`, Earth Engine + CHIRPS daily) isn't available in this
environment. **Weaker justification than Borneo's version, worth saying plainly rather than
copy-pasting the reasoning uncritically**: Borneo's flat 18-events/month placeholder was
defended as "typical equatorial-climate rain-day frequency" — the Philippines is NOT uniformly
equatorial. It has a real monsoon season (habagat/amihan) and a pronounced west-vs-east coast
contrast plus typhoon-driven rainfall concentrated in certain months, so a single flat
year-round number is a rougher approximation here than it was for Borneo. Kept anyway to let the
model run end-to-end and produce a first, honestly-caveated result — replacing this with a real
CHIRPS-daily-derived, monthly-varying table should be a higher-priority follow-up for the
Philippines specifically than it was for Borneo.
"""
import os

import pandas as pd

OUT_PATH = "data/swy/philippines/lulc/rain_events_table.csv"
PLACEHOLDER_EVENTS_PER_MONTH = 18  # rough estimate, NOT derived from data — see caveat above


def main():
    df = pd.DataFrame({"month": range(1, 13), "events": [PLACEHOLDER_EVENTS_PER_MONTH] * 12})
    os.makedirs(os.path.dirname(OUT_PATH), exist_ok=True)
    df.to_csv(OUT_PATH, index=False)
    print(df.to_string(index=False))
    print(f"\nWritten to {OUT_PATH} — PLACEHOLDER values, see module docstring.")


if __name__ == "__main__":
    main()
