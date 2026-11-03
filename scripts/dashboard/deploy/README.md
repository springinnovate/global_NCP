# Change explorer: deployment

The explorer (`scripts/dashboard/app.R`) is a single-file Shiny app. It shows the 1992-2020 change
maps on native pixels (pre-rendered PNG tiles) and recomputes hotspots on the 10 km grid for any
country, region, income group or biome. This folder runs it as a container.

## What runs where

| Part | Where | Size |
|---|---|---|
| Image: R 4.5.1 (`rocker/r-ver`), packages from a dated Posit Package Manager snapshot, `app.R` | built from this repo | 504 MB compressed, 1.8 GB unpacked |
| Data: `grid_dashboard.rds`, `coastal_points.rds`, `landcover_grid.rds`, `path_a/*.csv`, `tiles/` (224k PNGs) | a folder on the host, mounted read-only at `/data` | 8.3 GB |

The data stays out of the image: it holds unpublished results, and it changes independently of the code.

## Requirements

- Docker with Compose v2, linux/amd64.
- Disk: about 10 GB (data plus image).
- Memory: 0.55-0.7 GB measured with one or two viewers. Each session that selects the whole world holds
  its own copy of the grid (about 0.3 GB) while it is open. The container is capped at 4 GB.
- One CPU is enough. All sessions share one R process, so requests queue under concurrent use; this is
  sized for a small group (co-authors, a few demo viewers), not public traffic.

## Run

```bash
# 1. data folder (built on the analysis machine by build_data_bundle.sh), copied to the host; verify:
cd /srv/gncp-explorer-data && sha256sum -c --quiet SHA256SUMS

# 2. from scripts/dashboard/deploy/ in a clone of the repo:
EXPLORER_DATA=/srv/gncp-explorer-data docker compose up -d --build
docker compose ps        # "healthy" after about a minute
```

The app listens on `127.0.0.1:3838` only. Serve it through the host's reverse proxy with TLS; Shiny
needs WebSocket upgrades passed through (nginx: `proxy_http_version 1.1`, `Upgrade` and `Connection`
headers, `proxy_read_timeout` of a few minutes).

## Settings (environment)

| Variable | Default | Effect |
|---|---|---|
| `EXPLORER_ALLOW_DOWNLOAD` | `false` in `compose.yaml` | `true` shows the button that downloads the selected cells as CSV |
| `EXPLORER_TILE_URL` | unset: the app serves `/data/tiles` | URL where a web server serves the same `tiles/` folder (faster for many users) |
| `EXPLORER_DATA_DIR` | `/data` in the image | data folder inside the container |

## Security

Access: the app has no login, and the results are unpublished. Restrict access at the reverse proxy
(basic auth or the host's single sign-on) until the paper is out, and serve it over HTTPS only.

Container: base image pinned by digest; non-root user (uid 10001); read-only root filesystem with `/tmp`
as tmpfs; data mounted read-only; all capabilities dropped; `no-new-privileges`; memory (4 GB) and
process (256) limits; port bound to localhost.

App: browser inputs are checked against the values the interface offers before any computation, and the
one input shown as HTML (the selected unit) is escaped; errors reach viewers only as a generic message
(`shiny.sanitize.errors`); requests outside `tiles/` are rejected; no database, no outbound connections
from the server, no user input reaches a file path or a shell. Viewers' browsers load the basemaps from
Esri and OpenStreetMap.

Updates: rebuild the image every few months (or when a security update matters) to pick up patched
system libraries: bump the digest in the `FROM` line to the current `rocker/r-ver:4.5.1` and the
`CRAN_SNAPSHOT` date, rebuild, and rerun the checks below.

## Update

- Code: `git pull`, then `docker compose up -d --build`.
- Data: build a new folder with `build_data_bundle.sh`, verify it, point `EXPLORER_DATA` at it, `docker compose up -d`.

## Build the data folder

On a machine with the repo and its `data/` (see the data package README), from the repo root:

```bash
scripts/dashboard/deploy/build_data_bundle.sh /path/to/explorer_data
```

It copies the files above, writes `VERSION` (build time, commit) and `SHA256SUMS`. To move it to a
server, send it as one archive (`tar -cf explorer_data.tar -C <dir> .`) or with `rsync`: 224k small files
copy slowly one by one, and on exFAT or FAT drives they take far more space than their 8 GB.

## Known limitations

- Tiles over open ocean were never written (they would be empty), so the browser gets 404s for them.
  This is expected.
- Switching the change metric logs a Leaflet error (`clearRect`) in the browser console. Leaflet's canvas
  renderer redraws the coastal-points map after Shiny has replaced it; nothing visible breaks. The fix
  is to update those points through `leafletProxy` instead of re-rendering the map.
- Selecting the whole world copies the full grid (1.37 M cells) for that session; several viewers doing it
  at once is what drives memory use.
- The healthcheck also fails when `/data` stops being readable. On a laptop with the data on an external
  drive this happens after the drive is disconnected: the container keeps the old mount. Restart Docker
  Desktop, then `docker compose up -d`. A server with the data on its own disk is not affected.

Tested 2026-10-27, and again 2026-11-03 with the land cover tab, on macOS (Docker Desktop, linux/amd64) with the data folder built by
`build_data_bundle.sh`: all checksums verify; the container reports healthy; every tab renders in
headless Chromium with no server errors; tiles of every service and metric are served; the cell download
is hidden; the container runs as uid 10001 and cannot write to `/app` or `/data`; a request outside
`tiles/` is rejected.
