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
# Second real upstream bug, found 2026-09-14 (Philippines/Becky-inputs comparison run): in
# `seasonal_water_yield.execute()`, `interpolate_list = ['near'] * len(input_align_list)` is
# computed BEFORE the `if args['user_defined_rain_events_dir']:` block extends both
# `input_align_list` and `output_align_list` by the 12 rain-event rasters — so any call using
# `user_defined_rain_events_dir` (real, spatially-distributed rain events, as opposed to the flat
# placeholder table) crashes immediately at the alignment step with a length-mismatch ValueError.
# Patched below by extending `interpolate_list` to match right after that block. Doesn't affect
# any run that doesn't use `user_defined_rain_events_dir` (Borneo, and the plain Philippines run,
# both use the flat table instead) — safe to share this image across all of them.
#
# Third bug found in the same block, NOT patched (documented for a future proper fix): once the
# above is fixed, `n_events_path_list = sorted(os.listdir(args['user_defined_rain_events_dir']))`
# builds bare filenames (no directory prefix), unlike the precip/ET0 lists two lines above which
# use `Path(...).glob('*.tif')` for full paths — and that same list is reused as both the
# alignment *input* and *output* target, so a naive path fix would make `align_and_resize_
# raster_stack` write reprojected rasters back out on top of the original source files. Real fix
# needs genuinely separate aligned-output paths (see `_INTERMEDIATE_BASE_FILES`-style naming used
# for precip/et0) — real feature-completion work, not a one-line patch, and risks data loss if
# rushed. `09b_run_swy_ph_becky_inputs.py` falls back to the flat placeholder rain-events table
# instead of `user_defined_rain_events_dir` because of this.

FROM therealspring/global_ncp-computational-environment:latest

USER root
RUN git clone https://github.com/springinnovate/inspring.git /opt/inspring
WORKDIR /opt/inspring
RUN micromamba run -n geopy311 python -c "\
p = 'src/inspring/seasonal_water_yield/seasonal_water_yield.py'; \
src = open(p).read(); \
old = \"        output_align_list.extend(n_events_path_list)\n\"; \
assert src.count(old) == 1, 'patch anchor not found or not unique'; \
new = old + \"        interpolate_list.extend(['near'] * (len(input_align_list) - len(interpolate_list)))\n\"; \
open(p, 'w').write(src.replace(old, new))"
RUN micromamba run -n geopy311 python setup.py build_ext --inplace

# Confirmed 2026-09-11: inspring imports `ecoshard.geoprocessing`/`ecoshard.taskgraph`
# (bundled submodules of the current upstream ecoshard already in this image), not the
# standalone pygeoprocessing/taskgraph PyPI packages — no extra install needed for those.
ENV PYTHONPATH=/opt/inspring/src
USER mambauser
WORKDIR /workspace
CMD ["micromamba", "run", "-n", "geopy311", "python"]
