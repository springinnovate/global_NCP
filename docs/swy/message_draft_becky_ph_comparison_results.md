Subject: Philippines comparison — status update, still running

Hi Becky,

Wanted to give you a real update instead of going quiet on this — it's taken a lot longer than I
expected, for a good reason.

Quick answer to your earlier question first: yes, we used your actual land-cover map directly —
the file in the INPUTS folder (`ph_baseline_lulc_md5_7f29da.tif`), matching your `.ini`'s path
exactly. We didn't adjust the ESA map to approximate yours. What we did instead is build a lookup
table translating your 12 land-cover categories into our own curve-number reference table, which
happens to be organized by a different (ESA) classification scheme. That table only supplies the
runoff numbers — it doesn't touch or modify your map. Sorry for not spelling that out clearly the
first time.

Where things actually stand: I ran the comparison and had a first set of results, but caught a real
problem before sending anything — your `.ini` sets `TARGET_PIXEL_SIZE = 30`, but our run had
defaulted to a 90m DEM (reused from an earlier, unrelated test elsewhere, since your inputs don't
include a DEM). That meant our run wasn't actually isolating Kc/CN the way the comparison was
supposed to — resolution was quietly varying too. Fetching a 30m DEM (SRTMGL1, matching your value
exactly) and re-running now — that run is still going as I write this (several hours in; it's a
much bigger raster than before).

While waiting on that, we also spot-checked the two runs' outputs for spatial misalignment — a
water body's position looked slightly off between them on our interactive map, and we wanted to
rule out a real registration bug before trusting any pixel-wise numbers. Good news: a proper
whole-domain check (cross-correlating both land/water masks) found no systematic offset at all —
99%+ agreement at zero shift. What looked like misalignment was resolution differences (30m vs
90m) on a geometrically complex lake, not a real problem. Worth mentioning only so you know we
checked rather than assumed.

Once the new run finishes, I'll send the actual numbers — a real pixel-wise comparison against
your baseline, an interactive map, and the CN/Kc writeup. Didn't want the silence in the meantime
to look like nothing was happening.

Jerónimo
