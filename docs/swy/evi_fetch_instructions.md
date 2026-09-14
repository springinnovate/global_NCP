# Fetch EVI directly — run these yourself, not through Claude Code

Testing whether running from your own terminal (rather than Claude Code's Bash tool) bypasses
whatever is blocking AppEEARS API access. Two commands, one per region. Run from a Git Bash
prompt in the repo root (`/c/projects/global_NCP`).

```bash
.venv/Scripts/python.exe Python_scripts/swy_borneo_run/03b_fetch_evi.py
```

```bash
.venv/Scripts/python.exe Python_scripts/swy_philippines_run/03b_fetch_evi.py
```

Each submits an AppEEARS task, polls until it finishes (can take a while — same order of
magnitude as the DEM/NDVI fetches did), and downloads the result. You'll see progress printed as
it goes (`task <id>: <status> (<n>s elapsed)`).

**If you get a 403 error** immediately on submission (not a timeout, not a "done" status — an
actual error a few seconds in) — same block as before, still there. Fall back to the manual UI
route: upload `data/swy/borneo/inputs/borneo_evi_full_request.json` and
`data/swy/philippines/inputs/ph_evi_full_request.json` the same way you did for DEM/NDVI.

**If it works** — that confirms the block is specific to Claude Code's own execution path, not
your account or a permanent NASA-side restriction. Worth knowing either way; tell me which
happened.

**Once files land**: Borneo → `data/swy/borneo/inputs/mod13a3_evi_2020/`, Philippines →
`data/swy/philippines/inputs/mod13a3_evi_2020/` (both scripts write there automatically — no
manual sorting/renaming needed this time, unlike the DEM files).
