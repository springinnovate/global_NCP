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

FROM therealspring/global_ncp-computational-environment:latest

USER root
RUN git clone https://github.com/springinnovate/inspring.git /opt/inspring
WORKDIR /opt/inspring
RUN micromamba run -n geopy311 python setup.py build_ext --inplace

# Confirmed 2026-09-11: inspring imports `ecoshard.geoprocessing`/`ecoshard.taskgraph`
# (bundled submodules of the current upstream ecoshard already in this image), not the
# standalone pygeoprocessing/taskgraph PyPI packages — no extra install needed for those.
ENV PYTHONPATH=/opt/inspring/src
USER mambauser
WORKDIR /workspace
CMD ["micromamba", "run", "-n", "geopy311", "python"]
