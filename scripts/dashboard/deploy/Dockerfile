# Ecosystem service change explorer (scripts/dashboard/app.R) as a container.
# The image holds R, the packages and the app; the data (~8 GB, mostly map tiles) is mounted read-only
# at /data at run time. Build from scripts/dashboard/:  docker build -f deploy/Dockerfile -t gncp-explorer .

# Pinned by digest: the exact image the package was tested with (rocker/r-ver:4.5.1, Ubuntu 24.04)
FROM rocker/r-ver:4.5.1@sha256:03b023fbf7b1b24ac1bb8b2ac5fd7e15a767e67b40ff50c155e328110981c2aa

# Binary packages from a dated Posit Package Manager snapshot, so every build gets the same versions
ARG CRAN_SNAPSHOT=https://p3m.dev/cran/__linux__/noble/2026-10-01

# Runtime libraries of the binary packages: fs (libuv), and sf, terra and units, which leaflet pulls in
# (GDAL, PROJ, GEOS, udunits). Plus a font for the plots.
RUN apt-get update && apt-get install -y --no-install-recommends \
      libuv1t64 libgdal34t64 libproj25 libgeos-c1t64 libudunits2-0 fonts-dejavu-core \
    && rm -rf /var/lib/apt/lists/*

# Install, then load every installed package: a missing system library fails the build, not a viewer's page
RUN Rscript -e "pkgs <- c('shiny', 'bslib', 'leaflet', 'dplyr', 'ggplot2', 'patchwork', 'htmltools', \
                          'htmlwidgets'); \
                install.packages(pkgs, repos = '${CRAN_SNAPSHOT}'); \
                missing <- setdiff(pkgs, rownames(installed.packages())); \
                if (length(missing)) stop('not installed: ', paste(missing, collapse = ', ')); \
                for (p in rownames(installed.packages(lib.loc = .libPaths()[1]))) loadNamespace(p)"

# Unprivileged user; the app only reads /data and writes R's temp files to /tmp
RUN useradd --no-create-home --uid 10001 --home-dir /app --shell /usr/sbin/nologin explorer
COPY app.R /app/app.R

ENV EXPLORER_DATA_DIR=/data
# R's per-user cache (bslib/sass) goes to the tmpfs; the rest of the filesystem is read-only at run time
ENV XDG_CACHE_HOME=/tmp/.cache
USER explorer
WORKDIR /app
EXPOSE 3838
# plots through R's Cairo PNG device: ragg spaces DejaVu Sans glyphs unevenly at plot text sizes;
# errors are logged on the server but shown to viewers only as a generic message
CMD ["Rscript", "-e", "options(shiny.useragg = FALSE, shiny.sanitize.errors = TRUE); shiny::runApp('/app', host = '0.0.0.0', port = 3838, launch.browser = FALSE)"]
