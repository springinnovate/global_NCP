# SWY Borneo test run environment.
#
# Built on top of this project's own working image
# (therealspring/global_ncp-computational-environment) rather than inspring's own Dockerfile,
# which is broken as of 2026-09-10: it references a requirements.txt that was deleted from the
# inspring repo in 2022 and never replaced, and pins an old therealspring/ecoshard fork commit of
# unconfirmed compatibility. This project's own image already has gcc/g++/cython/GDAL and a
# fresh (current upstream, unpinned) ecoshard install, which turned out to be everything inspring
# actually needs.
#
# Real bug worked around here, not fixed upstream: inspring's own setup.py omits
# 'inspring.seasonal_water_yield' from its `packages` list, so a normal `pip install .` builds
# the Cython extension but the Python module itself is not importable. Workaround: build the
# Cython extensions in place and import via PYTHONPATH against the source tree directly, rather
# than relying on the (broken) installed package. Real candidate for a small upstream PR — see
# docs/swy/research_notes.md's 2026-09-11 entry.
#
# Two real upstream bugs, both in `seasonal_water_yield.execute()`'s `user_defined_rain_events_dir`
# block, found 2026-09-14 (Philippines/Becky-inputs comparison run) and both patched below as of
# 2026-09-16 (the second was originally left unpatched — see below for why that changed):
#
# (1) `interpolate_list = ['near'] * len(input_align_list)` is computed BEFORE this block extends
# both `input_align_list` and `output_align_list` by the 12 rain-event rasters — so any call using
# `user_defined_rain_events_dir` (real, spatially-distributed rain events, as opposed to the flat
# placeholder table) crashes immediately at the alignment step with a length-mismatch ValueError.
#
# (2) `n_events_path_list = sorted(os.listdir(args['user_defined_rain_events_dir']))` builds bare
# filenames (no directory prefix), unlike the precip/ET0 lists two lines above which use
# `Path(...).glob('*.tif')` for full paths — and that same bare-filename list was reused as both
# the alignment *input* and *output* target, meaning `align_and_resize_raster_stack` would write
# reprojected rasters back out on top of the original source files by name collision. **Originally
# left unpatched** on the reasoning that a naive fix risked data loss — re-examined 2026-09-16 and
# that reasoning doesn't hold up: `_TMP_BASE_FILES` already reserves a completely separate, safe
# set of output paths for exactly this (`n_events_path_list`, in the cache dir — the same pattern
# already used correctly for precip/ET0 two lines above, and even used correctly by a *different*
# branch of this same function), which the buggy block was simply discarding and overwriting with
# the unsafe bare names instead of using. The fix below reads from real full source paths but
# writes aligned output to inspring's own pre-reserved safe paths, never touching the source files
# at all — not the "real feature-completion work" it looked like at first glance, once traced.
#
# A third real upstream bug, found 2026-09-16 immediately after fixing the first two above: the
# code that populates `reclassify_n_events_task_list` (each month's quickflow task depends on the
# matching entry) is written ONLY for the case where `user_defined_rain_events_dir` is NOT set —
# guarded by `if 'user_defined_rain_events_dir' not in args or not args[...]:`. When it IS set,
# that whole block is skipped and the list stays empty, crashing with IndexError the moment the
# monthly quickflow step indexes into it. `user_defined_rain_events_dir` was apparently never
# fully wired up for this (non-`prealigned`) code path — only stubbed at the alignment stage.
# Fix mirrors the `prealigned` branch's own precedent elsewhere in this same file
# (`reclassify_n_events_task_list = [align_task]*12`): the rain-event rasters are already in
# their final, aligned form once `align_task` completes, so no further per-month reclassification
# is needed — just register that same task as the dependency for all 12 months.
#
# A fourth bug, found 2026-09-16 by visually inspecting the Philippines rainfix run's own output
# rasters (a clear, regular checkerboard visible in both QF and B once viewed at native resolution
# — not visible in the whole-country overview, and not present in WWF-SIPA's own baseline output at
# the same location despite her using the exact same rain-events input file at an even coarser
# resampling ratio, 33:1 vs. this project's 11:1): patch (1) above hardcoded `'near'` (nearest-
# neighbor) resampling for the 12 rain-event rasters when extending `interpolate_list`. Nearest-
# neighbor is the wrong choice for a coarse count/rate field being resampled onto a much finer
# grid — it tiles each ~1km source pixel's exact value across an ~11x11 (Philippines, 90m) or
# ~33x33 (WWF-SIPA's own 30m run, unaffected since her own pipeline isn't this codebase) block of
# destination pixels, with a hard step at every source-cell edge. Fixed: rain-event rasters now use
# `'bilinear'`, distinct from the `'near'` still correctly used for the rest of `interpolate_list`
# (CN/LULC-type categorical inputs, where nearest-neighbor is the right choice).
#
# A fifth bug, found 2026-09-16 immediately after the fourth: the checkerboard did not go away
# after the `'bilinear'` fix above, because that fix only covered the rain-events rasters — the
# SAME `'near'`-for-everything line (`interpolate_list = ['near'] * len(input_align_list)`) also
# governs precip and ET0, both continuous fields, both ~1km native here (and CHIRPS precip, this
# project's own choice for Borneo/other runs, is ~5.5km — an even coarser mismatch). This is stock
# `inspring`/InVEST behavior, not introduced by any patch above — every run this project has done
# likely has this same checkerboard, just unnoticed before this session's pixel-level inspection.
# Fixed: precip and ET0 now use `'bilinear'` too; LULC, soil hydrologic group, and DEM correctly
# keep `'near'` (categorical data, or already at the target resolution).
#
# None of these five patches affect any run that doesn't use `user_defined_rain_events_dir` for
# the fourth fix specifically; the fifth fix (precip/ET0) applies to every run using this Dockerfile
# regardless of that flag.

FROM therealspring/global_ncp-computational-environment:latest

USER root
RUN git clone https://github.com/springinnovate/inspring.git /opt/inspring
WORKDIR /opt/inspring
RUN micromamba run -n geopy311 python -c "\
p = 'src/inspring/seasonal_water_yield/seasonal_water_yield.py'; \
src = open(p).read(); \
old = (\
    \"        n_events_path_list = sorted(os.listdir(args['user_defined_rain_events_dir']))\n\" \
    \"        input_align_list.extend(n_events_path_list)\n\" \
    \"        file_registry['n_events_path_list'] = n_events_path_list\n\" \
    \"        output_align_list.extend(n_events_path_list)\n\" \
); \
assert src.count(old) == 1, 'patch anchor not found or not unique'; \
new = (\
    \"        n_events_path_list = sorted(\n\" \
    \"            str(p) for p in Path(args['user_defined_rain_events_dir']).glob('*.tif'))\n\" \
    \"        input_align_list.extend(n_events_path_list)\n\" \
    \"        output_align_list.extend(file_registry['n_events_path_list'])\n\" \
    \"        interpolate_list.extend(['bilinear'] * (len(input_align_list) - len(interpolate_list)))\n\" \
); \
src = src.replace(old, new); \
old2 = (\
    \"                    reclassify_n_events_task_list.append(n_events_task)\n\" \
    \"\n\" \
    \"        curve_number_task = task_graph.add_task(\n\" \
); \
assert src.count(old2) == 1, 'second patch anchor not found or not unique'; \
new2 = (\
    \"                    reclassify_n_events_task_list.append(n_events_task)\n\" \
    \"        else:\n\" \
    \"            reclassify_n_events_task_list.extend([align_task] * N_MONTHS)\n\" \
    \"\n\" \
    \"        curve_number_task = task_graph.add_task(\n\" \
); \
src = src.replace(old2, new2); \
old3 = \"    interpolate_list = ['near'] * len(input_align_list)\n\"; \
assert src.count(old3) == 1, 'third patch anchor not found or not unique'; \
new3 = (\
    \"    if not args['user_defined_local_recharge']:\n\" \
    \"        interpolate_list = (\n\" \
    \"            ['bilinear'] * len(precip_path_list) + ['near'] +\n\" \
    \"            ['bilinear'] * len(et0_path_list) +\n\" \
    \"            ['near'] * (len(input_align_list) - len(precip_path_list) - 1 - len(et0_path_list)))\n\" \
    \"    else:\n\" \
    \"        interpolate_list = ['near'] * len(input_align_list)\n\" \
); \
open(p, 'w').write(src.replace(old3, new3))"
RUN micromamba run -n geopy311 python setup.py build_ext --inplace

# Confirmed 2026-09-11: inspring imports `ecoshard.geoprocessing`/`ecoshard.taskgraph`
# (bundled submodules of the current upstream ecoshard already in this image), not the
# standalone pygeoprocessing/taskgraph PyPI packages — no extra install needed for those.
ENV PYTHONPATH=/opt/inspring/src
USER mambauser
WORKDIR /workspace
CMD ["micromamba", "run", "-n", "geopy311", "python"]
