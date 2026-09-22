"""Shared helpers for NASA AppEEARS API access (DEM and NDVI acquisition).

Auth is entirely via `.netrc` (two blocks required — see the module docstring note below) —
never pass credentials as arguments or hardcode them here. `requests` reads `.netrc`
automatically when `trust_env=True` (the default), which is what makes this simpler and more
robust than the curl-based approach used interactively earlier in this project: shelling out to
`curl` from inside a Python `subprocess` on Windows can silently pick up a different, unrelated
`curl.exe` that can't read `.netrc` (a real gotcha hit and worked around in
docs/swy/research_notes.md's 2026-09-10 entry). Talking to the API directly via `requests`
avoids that whole class of problem.

Required `.netrc` setup (see docs/HANDOFF.md for the full walkthrough):
    machine urs.earthdata.nasa.gov
    login <username>
    password <password>

    machine appeears.earthdatacloud.nasa.gov
    login <username>
    password <password>

Both blocks are needed — AppEEARS authenticates against a different hostname than the one you
register with, and curl/requests won't match credentials across hostnames.
"""
import time

import geopandas as gpd
import requests

APPEEARS_BASE = "https://appeears.earthdatacloud.nasa.gov/api"


def aoi_to_geojson(aoi_path):
    """Load a vector AOI and return it as a GeoJSON FeatureCollection dict, reprojected to
    EPSG:4326 (required by AppEEARS)."""
    aoi = gpd.read_file(aoi_path).to_crs(4326)
    return aoi.__geo_interface__


def login():
    """Return a Bearer token. Real gotcha, worth keeping: this endpoint wants
    Content-Length: 0 and NO body — an earlier attempt with Content-Type: application/json
    and a `{}` body got a 400 "missing required parameter" (see research_notes.md)."""
    r = requests.post(f"{APPEEARS_BASE}/login", headers={"Content-Length": "0"})
    r.raise_for_status()
    return r.json()["token"]


def submit_area_task(token, task_name, layers, start_date, end_date, aoi_geojson):
    """Submit an AppEEARS 'area' task. `layers` is a list of {'product':..., 'layer':...}
    dicts. Dates are MM-DD-YYYY strings. Returns the task_id."""
    task = {
        "task_type": "area",
        "task_name": task_name,
        "params": {
            "dates": [{"startDate": start_date, "endDate": end_date}],
            "layers": layers,
            "geo": aoi_geojson,
            "output": {"format": {"type": "geotiff"}, "projection": "geographic"},
        },
    }
    r = requests.post(
        f"{APPEEARS_BASE}/task",
        headers={"Authorization": f"Bearer {token}", "Content-Type": "application/json"},
        json=task,
    )
    r.raise_for_status()
    return r.json()["task_id"]


def wait_for_task(token, task_id, poll_seconds=30, timeout_seconds=3600):
    """Poll task status until done. Raises TimeoutError if it doesn't finish in time."""
    elapsed = 0
    while elapsed < timeout_seconds:
        r = requests.get(f"{APPEEARS_BASE}/status/{task_id}", headers={"Authorization": f"Bearer {token}"})
        r.raise_for_status()
        status = r.json().get("status")
        print(f"  task {task_id}: {status} ({elapsed}s elapsed)")
        if status == "done":
            return
        time.sleep(poll_seconds)
        elapsed += poll_seconds
    raise TimeoutError(f"AppEEARS task {task_id} did not finish within {timeout_seconds}s")


def download_bundle(token, task_id, out_dir, name_filter=None):
    """Download every file in a completed task's bundle to out_dir.
    `name_filter(filename) -> bool` can restrict which files are pulled.
    Real gotcha: the bundle file endpoint 302-redirects to a pre-signed S3 URL —
    `requests` follows redirects by default (unlike a bare `curl` without `-L`), so this
    just works, but it's worth knowing why the equivalent curl command needs `-L` if anyone
    reimplements this by hand.
    """
    import os

    r = requests.get(f"{APPEEARS_BASE}/bundle/{task_id}", headers={"Authorization": f"Bearer {token}"})
    r.raise_for_status()
    files = r.json()["files"]
    os.makedirs(out_dir, exist_ok=True)
    downloaded = []
    for f in files:
        fname = os.path.basename(f["file_name"])
        if name_filter and not name_filter(fname):
            continue
        resp = requests.get(
            f"{APPEEARS_BASE}/bundle/{task_id}/{f['file_id']}",
            headers={"Authorization": f"Bearer {token}"},
        )
        resp.raise_for_status()
        out_path = os.path.join(out_dir, fname)
        with open(out_path, "wb") as fh:
            fh.write(resp.content)
        downloaded.append(out_path)
    print(f"  downloaded {len(downloaded)} files to {out_dir}")
    return downloaded
