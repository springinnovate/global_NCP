# Draft GitHub issues for springinnovate/inspring

Three separate issues, grouped by theme. Not posted yet — `gh` CLI isn't installed on this
machine, so either install it (`gh auth login` against your GitHub account) or paste these
into the GitHub web UI at github.com/springinnovate/inspring/issues/new by hand.

All three are grounded in a real Philippines SWY comparison run, not synthetic testing —
happy to share the actual repro workspace/Dockerfile if useful.

---

## Issue 1: Dockerfile and packaging are broken for a fresh install

**Title:** `Dockerfile` references a deleted `requirements.txt`; `setup.py` omits `seasonal_water_yield` from `packages`

**Body:**

Tried a clean `docker build` from this repo's own `Dockerfile` and hit two separate problems:

1. **`Dockerfile` fails immediately** on `pip3 install -r requirements.txt` — that file was
   deleted from the repo (looks like 2022) and never replaced or removed from the Dockerfile.

2. **`setup.py`'s `packages` list omits `inspring.seasonal_water_yield`.** A normal
   `pip install .` compiles the Cython extension fine, but the Python module itself isn't
   importable afterward (`ModuleNotFoundError: No module named 'inspring.seasonal_water_yield'`
   despite the `.so` being present). Workaround: `python setup.py build_ext --inplace` +
   `PYTHONPATH` pointed at the source tree directly, instead of a real install.

Worked around both by building on our own existing environment image rather than this repo's
own Dockerfile. Happy to send a PR adding `seasonal_water_yield` (and checking for any other
missing subpackages) to `setup.py`'s `packages` list, and either restoring `requirements.txt`
or updating the Dockerfile to not reference it.

---

## Issue 2: Three bugs in `user_defined_rain_events_dir`, plus a resampling-method issue affecting any coarse input

**Title:** `user_defined_rain_events_dir` crashes (3 separate bugs); alignment step uses nearest-neighbor for all inputs, including continuous ones coarser than the target grid

**Body:**

Using `user_defined_rain_events_dir` to feed real spatially-distributed monthly rain-event
counts (instead of the flat `rain_events_table_path` placeholder) hits three separate bugs in
`seasonal_water_yield.execute()`, found and fixed in this order:

1. `interpolate_list = ['near'] * len(input_align_list)` is computed *before* the
   `user_defined_rain_events_dir` block extends `input_align_list`/`output_align_list` by the
   12 monthly rasters — any real use of this parameter crashes immediately at alignment with a
   length-mismatch `ValueError`.

2. `n_events_path_list = sorted(os.listdir(args['user_defined_rain_events_dir']))` builds bare
   filenames (no directory prefix), unlike the precip/ET0 lists two lines above which use
   `Path(...).glob('*.tif')` for full paths — and that same bare-filename list gets reused as
   both the alignment *input* and *output* target, so a naive fix risks
   `align_and_resize_raster_stack` writing reprojected rasters back out on top of the original
   source files by name collision.

3. `reclassify_n_events_task_list` is only ever populated in the branch where
   `user_defined_rain_events_dir` is **not** set (`if 'user_defined_rain_events_dir' not in
   args or not args[...]:`). When it *is* set, the list stays empty and the next step crashes
   with `IndexError` the moment the monthly quickflow step indexes into it. This code path
   looks like it was never fully wired up for the non-`prealigned` case, only stubbed at the
   alignment stage.

**A fourth, separate issue in the same area**: that same `interpolate_list` line applies
`'near'` (nearest-neighbor) uniformly to *every* aligned input — correct for LULC/soil-group
(categorical), but wrong for precip/ET0/rain-events (continuous fields). When any of those is
coarser than the target grid (our rain-events/precip source is ~1km native against a 90m DEM;
CHIRPS at ~5.5km would be a more extreme case), this produces a real, regular checkerboard
artifact in the final QF/B output — confirmed by direct pixel inspection of the aligned
intermediate rasters, both before and after switching that specific interpolation to
`'bilinear'`. This isn't specific to the three bugs above; it's stock behavior for the whole
alignment call and would affect any run using a continuous input coarser than its DEM.

All four are patched and tested (multiple full end-to-end runs, checkerboard confirmed gone by
direct raster inspection). Happy to open a PR with the diff — it's small: two isolated fixes in
the `user_defined_rain_events_dir` block, one reordering of a list-population branch, and
splitting `interpolate_list`'s construction so precip/ET0/rain-events get `'bilinear'` instead
of `'near'`.

---

## Issue 3: `cn_a`/`cn_b`/`cn_c`/`cn_d`/`root_depth`/`kc_*` support a per-pixel raster override, undocumented

**Title:** `_reclassify_or_clip()` accepts a direct raster path via `args['{factor}_path']` — not mentioned anywhere in `execute()`'s docstring

**Body:**

Spent a while assuming there was no way to feed `seasonal_water_yield.execute()` a per-pixel
CN raster directly (only the lucode-indexed CSV table), based on reading `execute()`'s own
`Args:` documentation. Turns out that's wrong — `_reclassify_or_clip()` checks
`args[f'{key_field}_path']` for each factor in `_TABLE_BASED_BIOPHYSICAL_FACTORS` (`root_depth`,
`cn_a`, `cn_b`, `cn_c`, `cn_d`, `kc_1`-`kc_12`) *before* falling back to the table, and if that
key is set to a real path, it warps that raster directly onto the model grid via
`geoprocessing.warp_raster(..., 'bilinear', ...)` and uses it as-is.

This is a genuinely useful feature (lets you use a real per-pixel product like GCN250 instead
of collapsing it down to one value per land-cover class), but it's not mentioned anywhere in
`execute()`'s own docstring/`Args:` list, so it's very easy to miss — we did, for a while, and
built out unnecessary table-based tooling as a result.

**A real bug in this same path, found the first time we actually used it**: the
`geoprocessing.warp_raster(...)` call inside `_reclassify_or_clip()` doesn't handle nodata at
all — `warp_raster`'s own signature has no `nodata`/`src_nodata`/`dst_nodata` parameter, and the
call passes no `gdal_warp_options`/`gdal_warp_kwargs` that could supply one another way. Feeding
in a real-world raster with genuine nodata gaps (GCN250 has small coastal-coverage gaps that don't
quite match our own DEM/LULC-derived land mask) meant those nodata cells got warped through as
literal data — in our case, 1.17% of otherwise-valid model pixels ended up with a CN of 255
(GCN250's nodata sentinel, not a real curve number), concentrated in a fringe around every
coastline. Worked around by pre-filling the source raster's nodata gaps with a nearest-neighbor
fill before passing it in, rather than fixing the warp call itself.

Would a docstring PR adding these keys (`cn_a_path`, `cn_b_path`, `cn_c_path`, `cn_d_path`,
`root_depth_path`, `kc_1_path`...`kc_12_path`) to `execute()`'s `Args:` section, plus a nodata fix
for the warp call (or at minimum a documented caveat that callers need to pre-fill nodata
themselves), be welcome? Happy to write both — just want to confirm this is intentional/stable
behavior you'd want documented and hardened, rather than incidental.
