"""Build ONE embeddable Leaflet HTML fragment covering all of the Philippines SWY comparison's
interactive map content — replaces the earlier two-file split (`11b_build_report_map_html.py` +
`11c_build_comparison_map_html.py`, both removed 2026-09-15 night) after the user pointed out
there was no real reason for two separate maps/legends/opacity controls when one map with a
layer-toggle control does the same job with less duplicated boilerplate.

Reads two independently-generated data sources and merges them into one map:
- `data/swy/philippines/comparison_maps/{layers,lookup}.json` (from `10c_render_comparison_maps.py`)
  — QF/B, WWF-SIPA baseline vs. NCP Kc/CN run, shared color scale per pair, plus the coarse numeric
  grids behind the click-to-compare popup.
- `data/swy/philippines/workspace_becky_inputs/output_maps/layers.json` (from
  `10b_render_output_maps_becky_inputs.py`) — AET and L_sum, NCP-run-only (the WWF-SIPA baseline
  output never saved either, so there's no baseline counterpart to compare against — these just
  aren't part of the click-popup, only the layer-toggle list).

Click-anywhere popup, crisp-pixel rendering, and the opacity slider (all 2026-09-15 night fixes,
carried over unchanged from `11c`) apply across every layer here, not just the comparison pairs.

Output: docs/reports/swy/_output_map_ph.html, included from swy_status_report.qmd.
"""
import json
import os

COMPARISON_WORKSPACE = "data/swy/philippines/comparison_maps"
STANDALONE_LAYERS_JSON = (
    "data/swy/philippines/workspace_becky_inputs_90m_snapped/output_maps/layers.json"
)
OUT_PATH = "docs/reports/swy/_output_map_ph.html"

MAP_BOUNDS = [[4.51, 113.84], [21.13, 126.83]]


def main():
    with open(os.path.join(COMPARISON_WORKSPACE, "layers.json")) as f:
        comparison_layers = json.load(f)
    with open(os.path.join(COMPARISON_WORKSPACE, "lookup.json")) as f:
        lookups = json.load(f)
    with open(STANDALONE_LAYERS_JSON) as f:
        standalone_layers = json.load(f)

    all_layers = comparison_layers + standalone_layers
    lookups_by_var = {l["var"]: l for l in lookups}
    lookups_js = json.dumps(lookups_by_var)

    def legend_row(l):
        note = f" ({l['shared_scale_note']})" if "shared_scale_note" in l else ""
        return (
            f'<div class="swy-legend-row"><span class="swy-legend-swatch" '
            f'style="background:linear-gradient(to right, #440154, #3b528b, #21918c, #5ec962, #fde725);">'
            f'</span><span>{l["label"]}: {l["vmin"]:.1f} &ndash; {l["vmax"]:.1f} mm{note}</span></div>'
        )

    legend_rows = "".join(legend_row(l) for l in all_layers)

    layer_decls = "\n".join(
        f'  const layer_{l["id"]} = L.imageOverlay('
        f'"data:image/png;base64,{l["b64"]}", '
        f'{json.dumps(l["bounds"])}, {{opacity: 1.0}});'
        for l in all_layers
    )

    overlays_js = "\n".join(f'overlays["{l["label"]}"] = layer_{l["id"]};' for l in all_layers)
    all_layer_vars_js = ", ".join(f'layer_{l["id"]}' for l in all_layers)

    html = f"""<style>
  #swy-output-map {{ height: 600px; width: 100%; border-radius: 6px; }}
  .swy-legend {{ font-size: 0.85em; margin-top: 8px; }}
  .swy-legend-row {{ display: flex; align-items: center; gap: 8px; margin: 2px 0; }}
  .swy-legend-swatch {{ display: inline-block; width: 60px; height: 10px; border-radius: 2px; }}
  .swy-opacity-row {{ display: flex; align-items: center; gap: 8px; margin: 8px 0 2px; }}
  .swy-opacity-row input[type="range"] {{ width: 160px; }}
  .leaflet-image-layer {{
    image-rendering: -moz-crisp-edges;
    image-rendering: crisp-edges;
    image-rendering: pixelated;
  }}
  .swy-popup-section {{ margin-bottom: 4px; }}
  .swy-popup-table {{ border-collapse: collapse; margin: 2px 0; }}
  .swy-popup-table td {{ padding: 1px 6px 1px 0; }}
  .swy-popup-diff {{ font-size: 0.9em; }}
  .swy-popup-footnote {{ font-size: 0.75em; opacity: 0.7; margin-top: 4px; max-width: 240px; }}
</style>

<div id="swy-output-map"></div>
<div class="swy-legend">
  {legend_rows}
  <div class="swy-opacity-row">
    <label for="swy-opacity-slider">Layer opacity</label>
    <input type="range" id="swy-opacity-slider" min="0" max="1" step="0.05" value="1">
    <span id="swy-opacity-value">100%</span>
  </div>
  <div style="margin-top:6px; opacity:0.75;">QF and B each share one color scale between the
  WWF-SIPA baseline and NCP Kc/CN run, so brightness is directly comparable — not two
  independently auto-scaled images that happen to look similar. The WWF-SIPA raster is native 30m
  resolution (that run set TARGET_PIXEL_SIZE=30 explicitly); the NCP run is native ~90m (inherited
  from the SRTMGL3 DEM used there, since the WWF-SIPA inputs didn't include a DEM) — a real
  resolution difference visible as finer texture in the WWF-SIPA layers, not a rendering artifact.
  AET and L_sum are NCP-run-only (the WWF-SIPA baseline output never saved either) — L_sum is
  capped at its 99th percentile so the real spatial signal stays visible against the ~10% of
  pixels with runaway flow-accumulation values (see the write-up above); AET's legitimate
  zero-value ocean pixels are masked transparent rather than shown as dark purple. Use the layer
  control in the top right to toggle any layer on/off. Click anywhere on the map for both runs'
  raw QF/B values at that point.
  <div style="margin-top:6px;">Each image here is downsampled from the full-resolution raster for
  display (area-averaged, not just subsampled) — a speckled look at this zoomed-out scale reflects
  real fine-scale variation in soil group/land cover being averaged into view, not a data or model
  artifact. All numbers quoted in this report (ratios, correlations, per-class spreads) come from
  the full-resolution rasters directly, never from this display image.</div></div>
</div>

<link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css" />
<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
<script>
(function() {{
  const map = L.map('swy-output-map');

  const satellite = L.tileLayer(
    'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{{z}}/{{y}}/{{x}}',
    {{ attribution: 'Esri World Imagery', maxZoom: 17 }}
  ).addTo(map);
  const osm = L.tileLayer(
    'https://{{s}}.tile.openstreetmap.org/{{z}}/{{x}}/{{y}}.png',
    {{ attribution: 'OpenStreetMap contributors', maxZoom: 19 }}
  );

{layer_decls}
  layer_QF_baseline.addTo(map);

  const overlays = {{}};
  {overlays_js}

  L.control.layers(
    {{ 'Satellite (Esri)': satellite, 'Street (OSM)': osm }},
    overlays,
    {{ collapsed: false }}
  ).addTo(map);

  map.fitBounds({json.dumps(MAP_BOUNDS)});

  // --- opacity slider: applies to every overlay, on or off, so a later-toggled layer matches ---
  const allOverlayLayers = [{all_layer_vars_js}];
  const opacitySlider = document.getElementById('swy-opacity-slider');
  const opacityValueLabel = document.getElementById('swy-opacity-value');
  opacitySlider.addEventListener('input', function() {{
    const v = parseFloat(opacitySlider.value);
    allOverlayLayers.forEach(function(layer) {{ layer.setOpacity(v); }});
    opacityValueLabel.textContent = Math.round(v * 100) + '%';
  }});

  // --- click-to-compare: decode lookup.json's raw float32 grids, sample both runs, popup ---
  // Only covers QF/B (the pairs with a WWF-SIPA counterpart) -- AET/L_sum are NCP-only and
  // aren't part of this popup, only the layer-toggle list above.
  const LOOKUPS = {lookups_js};

  function b64ToFloat32Array(b64) {{
    const binary = atob(b64);
    const bytes = new Uint8Array(binary.length);
    for (let i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);
    return new Float32Array(bytes.buffer);
  }}

  // Decode once at load, not per click — atob'ing ~1MB strings on every click is wasteful
  // when there are only 4 grids total.
  for (const v of Object.keys(LOOKUPS)) {{
    for (const runId of ['baseline', 'ncp']) {{
      LOOKUPS[v][runId].data = b64ToFloat32Array(LOOKUPS[v][runId].b64);
    }}
  }}

  function sampleGrid(grid, lat, lng) {{
    const west = grid.bounds[0], south = grid.bounds[1], east = grid.bounds[2], north = grid.bounds[3];
    if (lng < west || lng > east || lat < south || lat > north) return null;
    const col = Math.min(Math.floor((lng - west) / (east - west) * grid.width), grid.width - 1);
    const row = Math.min(Math.floor((north - lat) / (north - south) * grid.height), grid.height - 1);
    const value = grid.data[row * grid.width + col];
    return Number.isNaN(value) ? null : value;
  }}

  function fmtVal(v) {{
    return v === null ? 'no data' : v.toFixed(1) + ' mm';
  }}

  function buildVarPopupHtml(varKey, lat, lng) {{
    const lookup = LOOKUPS[varKey];
    const baseline = sampleGrid(lookup.baseline, lat, lng);
    const ncp = sampleGrid(lookup.ncp, lat, lng);

    let diffRow;
    if (baseline === null || ncp === null) {{
      diffRow = '<div class="swy-popup-diff">Difference: n/a (no data at this point)</div>';
    }} else {{
      const absDiff = ncp - baseline;
      const sign = absDiff >= 0 ? '+' : '';
      let pctText;
      if (Math.abs(baseline) < 0.05) {{
        pctText = 'n/a (baseline is ~0 mm)';
      }} else {{
        pctText = (ncp / baseline * 100).toFixed(0) + '% of the WWF-SIPA value';
      }}
      diffRow = '<div class="swy-popup-diff">Difference: ' + sign + absDiff.toFixed(1) +
        ' mm &nbsp;(NCP is ' + pctText + ')</div>';
    }}

    return '<div class="swy-popup-section"><strong>' + lookup.label + '</strong>' +
      '<table class="swy-popup-table">' +
      '<tr><td>WWF-SIPA baseline</td><td>' + fmtVal(baseline) + '</td></tr>' +
      '<tr><td>NCP Kc/CN run</td><td>' + fmtVal(ncp) + '</td></tr>' +
      '</table>' + diffRow + '</div>';
  }}

  map.on('click', function(e) {{
    const html = '<div class="swy-popup">' +
      buildVarPopupHtml('QF', e.latlng.lat, e.latlng.lng) +
      '<hr/>' +
      buildVarPopupHtml('B', e.latlng.lat, e.latlng.lng) +
      '<div class="swy-popup-footnote">Baseline (~30m native) and NCP (~90m native) grids are ' +
      'each sampled independently at their own resolution and are not co-registered pixels ' +
      '&mdash; treat this as "nearest data point in each dataset," not a pixel-for-pixel match.' +
      '</div></div>';
    L.popup({{ maxWidth: 280 }}).setLatLng(e.latlng).setContent(html).openOn(map);
  }});
}})();
</script>
"""

    with open(OUT_PATH, "w", encoding="utf-8") as f:
        f.write(html)
    print(f"Written to {OUT_PATH} ({len(html)} chars)")


if __name__ == "__main__":
    main()
