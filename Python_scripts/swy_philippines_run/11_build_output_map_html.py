"""Build ONE embeddable Leaflet HTML fragment covering all of the Philippines SWY comparison's
interactive map content — replaces the earlier two-file split (`11b_build_report_map_html.py` +
`11c_build_comparison_map_html.py`, both removed 2026-09-15 night) after the user pointed out
there was no real reason for two separate maps/legends/opacity controls when one map with a
layer-toggle control does the same job with less duplicated boilerplate.

Reads `data/swy/philippines/comparison_maps/{layers,lookup}.json` (from
`10c_render_comparison_maps.py`): QF/B, her CN + Kc through our 90m pipeline (09h) vs. the current NCP run (our CN + our
per-pixel Kc, 09l), shared color scale per pair, plus the coarse numeric grids behind the
click-to-compare popup. The standalone AET/L_sum layers (old 09e run) and the Negron Juarez-only
layers were dropped 2026-09-25: both came from superseded intermediate runs.

Click-anywhere popup, crisp-pixel rendering, and the opacity slider (all 2026-09-15 night fixes,
carried over unchanged from `11c`) apply across every layer here, not just the comparison pairs.

Output: docs/reports/swy/_output_map_ph.html, included from swy_status_report.qmd.
"""
import json
import os

COMPARISON_WORKSPACE = "data/swy/philippines/comparison_maps"
OUT_PATH = "docs/reports/swy/_output_map_ph.html"

MAP_BOUNDS = [[4.51, 113.84], [21.13, 126.83]]


def main():
    with open(os.path.join(COMPARISON_WORKSPACE, "layers.json")) as f:
        comparison_layers = json.load(f)
    with open(os.path.join(COMPARISON_WORKSPACE, "lookup.json")) as f:
        lookups = json.load(f)
    # context layers (10e_render_context_layers.py): land cover + shaded-relief DEM, each with
    # its own legend; the popup also reports class and elevation at the clicked point
    with open(os.path.join(COMPARISON_WORKSPACE, "context_layers.json")) as f:
        context_layers = json.load(f)
    with open(os.path.join(COMPARISON_WORKSPACE, "context_lookup.json")) as f:
        context_lookup = json.load(f)
    context_lookup_js = json.dumps(context_lookup)

    # optional test layers (10f_render_tree_crop_test_layers.py); skipped if the file is absent
    test_path = os.path.join(COMPARISON_WORKSPACE, "test_layers.json")
    test_layers = []
    if os.path.exists(test_path):
        with open(test_path) as f:
            test_layers = json.load(f)

    all_layers = comparison_layers + test_layers + context_layers
    lookups_by_var = {l["var"]: l for l in lookups}
    lookups_js = json.dumps(lookups_by_var)

    def legend_row(l):
        if "legend_html" in l:
            return l["legend_html"]
        note = f" ({l['shared_scale_note']})" if "shared_scale_note" in l else ""
        return (
            f'<div class="swy-legend-row"><span class="swy-legend-swatch" '
            f'style="background:linear-gradient(to right, #440154, #3b528b, #21918c, #5ec962, #fde725);">'
            f'</span><span>{l["label"]}: {l["vmin"]:.1f} &ndash; {l["vmax"]:.1f} mm{note}</span></div>'
        )

    # one legend block per layer, only the active layer's is shown (see showLayer below)
    legend_rows = "".join(
        f'<div class="swy-legend-block" data-layer="{l["id"]}" style="display:none;">{legend_row(l)}</div>'
        for l in all_layers
    )

    layer_decls = "\n".join(
        f'  const layer_{l["id"]} = L.imageOverlay('
        f'"data:image/png;base64,{l["b64"]}", '
        f'{json.dumps(l["bounds"])}, {{opacity: 1.0}});'
        for l in all_layers
    )

    all_layer_vars_js = ", ".join(f'layer_{l["id"]}' for l in all_layers)

    # Radio-style layer picker, not Leaflet's default checkbox overlay control -- selecting one
    # data layer removes whichever was showing before and shows only the new one, so comparing two
    # layers no longer means manually unchecking the first (2026-09-23, user-requested).
    #
    # No leading whitespace on these lines: this fragment is pulled into the report via Quarto's
    # {{< include >}}, which runs it through the Markdown parser first -- a 4+-space-indented line
    # is Markdown's own "this is a code block" rule, so the tags rendered as literal escaped text
    # instead of real HTML on the first attempt (confirmed directly against the rendered output,
    # not assumed). legend_row() below sidesteps this by staying on one unbroken line; this does
    # the same by joining with no line-leading indentation at all.
    radio_rows = "\n".join(
        f'<label class="swy-layer-radio"><input type="radio" name="swy-layer" '
        f'value="{l["id"]}"{" checked" if i == 0 else ""}> {l["label"]}</label>'
        for i, l in enumerate(all_layers)
    )
    layer_by_id_js = ", ".join(f'"{l["id"]}": layer_{l["id"]}' for l in all_layers)

    html = f"""<style>
  #swy-output-map {{ height: 600px; width: 100%; border-radius: 6px; }}
  .swy-legend {{ font-size: 0.85em; margin-top: 8px; }}
  .swy-legend-row {{ display: flex; align-items: center; gap: 8px; margin: 2px 0; }}
  .swy-legend-swatch {{ display: inline-block; width: 60px; height: 10px; border-radius: 2px; }}
  .swy-cat-grid {{ display: flex; flex-wrap: wrap; gap: 4px 14px; margin: 4px 0; }}
  .swy-cat {{ display: inline-flex; align-items: center; gap: 5px; }}
  .swy-cat-swatch {{ display: inline-block; width: 12px; height: 12px; border-radius: 2px; }}
  .swy-opacity-row {{ display: flex; align-items: center; gap: 8px; margin: 8px 0 2px; }}
  .swy-opacity-row input[type="range"] {{ width: 160px; }}
  .swy-layer-picker {{
    display: flex; flex-direction: column; gap: 3px; margin: 8px 0;
    max-height: 220px; overflow-y: auto; border: 1px solid #ddd; border-radius: 6px;
    padding: 8px 10px;
  }}
  .swy-layer-radio {{ display: flex; align-items: center; gap: 6px; font-size: 0.92em; cursor: pointer; }}
  .swy-layer-radio input {{ margin: 0; cursor: pointer; }}
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
<div style="margin-bottom:4px;"><strong>Layer</strong> (pick one — selecting a new layer replaces whichever was showing)</div>
<div class="swy-layer-picker">
{radio_rows}
</div>
{legend_rows}
<div class="swy-opacity-row">
<label for="swy-opacity-slider">Layer opacity</label>
<input type="range" id="swy-opacity-slider" min="0" max="1" step="0.05" value="1">
<span id="swy-opacity-value">100%</span>
</div>
<div style="margin-top:6px; opacity:0.75;">Both runs go through the same pipeline on the same 90m grid (SRTMGL3 DEM) with WWF-SIPA's inputs; they differ only in CN and Kc. Her CN + Kc through this pipeline reproduces WWF-SIPA's own 30m run closely (r=0.92 for QF and B), so it stands in for her run here. QF and B each share one color scale across both runs. Land cover and elevation are context layers (the model inputs both runs share). Click anywhere for land cover, elevation, and both runs' QF/B at that point.
<div style="margin-top:6px;">Each image here is downsampled from the full-resolution raster for display (area-averaged, not just subsampled) — a speckled look at this zoomed-out scale reflects real fine-scale variation in soil group/land cover being averaged into view, not a data or model artifact. All numbers quoted in this report (ratios, correlations, per-class spreads) come from the full-resolution rasters directly, never from this display image.</div></div>
</div>

<link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css" />
<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
<div id="swy-map-error" style="display:none; color:#b00; background:#fee; border:1px solid #b00; padding:8px 12px; border-radius:4px; margin-bottom:8px; font-family:monospace; font-size:12px; white-space:pre-wrap;"></div>
<script>
try {{
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

  L.control.layers(
    {{ 'Satellite (Esri)': satellite, 'Street (OSM)': osm }},
    {{}},
    {{ collapsed: false }}
  ).addTo(map);

  map.fitBounds({json.dumps(MAP_BOUNDS)});

  // --- radio-style exclusive data layer: only one shown at a time, picking a new one removes
  // whatever was on before instead of stacking (2026-09-23, replaces the old checkbox overlay
  // control, which let every layer accumulate and made comparing two of them a manual toggle). ---
  const allOverlayLayers = [{all_layer_vars_js}];
  const layerById = {{{layer_by_id_js}}};
  const opacitySlider = document.getElementById('swy-opacity-slider');
  const opacityValueLabel = document.getElementById('swy-opacity-value');
  let activeLayer = null;

  function showLayer(id) {{
    if (activeLayer) map.removeLayer(activeLayer);
    activeLayer = layerById[id];
    activeLayer.setOpacity(parseFloat(opacitySlider.value));
    activeLayer.addTo(map);
    document.querySelectorAll('.swy-legend-block').forEach(function(el) {{
      el.style.display = el.dataset.layer === id ? 'block' : 'none';
    }});
  }}

  document.querySelectorAll('input[name="swy-layer"]').forEach(function(radio) {{
    radio.addEventListener('change', function() {{ if (radio.checked) showLayer(radio.value); }});
  }});
  const initialChecked = document.querySelector('input[name="swy-layer"]:checked');
  if (initialChecked) showLayer(initialChecked.value);

  opacitySlider.addEventListener('input', function() {{
    const v = parseFloat(opacitySlider.value);
    if (activeLayer) activeLayer.setOpacity(v);
    opacityValueLabel.textContent = Math.round(v * 100) + '%';
  }});

  // --- click-to-compare: decode lookup.json's raw float32 grids, sample both runs, popup ---
  // Only covers QF/B (the pairs with a WWF-SIPA counterpart) -- AET/L_sum are NCP-only and
  // aren't part of this popup, only the layer-toggle list above.
  const LOOKUPS = {lookups_js};
  const CONTEXT = {context_lookup_js};
  CONTEXT.LULC.data = null; CONTEXT.DEM.data = null;

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

  CONTEXT.LULC.data = b64ToFloat32Array(CONTEXT.LULC.b64);
  CONTEXT.DEM.data = b64ToFloat32Array(CONTEXT.DEM.b64);

  function contextPopupHtml(lat, lng) {{
    const cls = sampleGrid(CONTEXT.LULC, lat, lng);
    const elev = sampleGrid(CONTEXT.DEM, lat, lng);
    const clsName = cls === null ? 'no data' : (CONTEXT.lulc_names[String(Math.round(cls))] || 'no data');
    const elevText = elev === null ? 'no data' : Math.round(elev) + ' m';
    return '<div class="swy-popup-section"><table class="swy-popup-table">' +
      '<tr><td>Land cover</td><td><strong>' + clsName + '</strong></td></tr>' +
      '<tr><td>Elevation</td><td>' + elevText + '</td></tr></table></div><hr/>';
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
        pctText = 'n/a (hers is ~0 mm)';
      }} else {{
        pctText = (ncp / baseline * 100).toFixed(0) + '% of hers';
      }}
      diffRow = '<div class="swy-popup-diff">Difference: ' + sign + absDiff.toFixed(1) +
        ' mm &nbsp;(ours is ' + pctText + ')</div>';
    }}

    return '<div class="swy-popup-section"><strong>' + lookup.label + '</strong>' +
      '<table class="swy-popup-table">' +
      '<tr><td>Her CN + her Kc</td><td>' + fmtVal(baseline) + '</td></tr>' +
      '<tr><td>Our CN + our Kc</td><td>' + fmtVal(ncp) + '</td></tr>' +
      '</table>' + diffRow + '</div>';
  }}

  map.on('click', function(e) {{
    const html = '<div class="swy-popup">' +
      contextPopupHtml(e.latlng.lat, e.latlng.lng) +
      buildVarPopupHtml('QF', e.latlng.lat, e.latlng.lng) +
      '<hr/>' +
      buildVarPopupHtml('B', e.latlng.lat, e.latlng.lng) +
      '<div class="swy-popup-footnote">Both runs share the same 90m grid; values come from a downsampled lookup grid.' +
      '</div></div>';
    L.popup({{ maxWidth: 280 }}).setLatLng(e.latlng).setContent(html).openOn(map);
  }});
}})();
}} catch (err) {{
  const box = document.getElementById('swy-map-error');
  box.style.display = 'block';
  box.textContent = 'Map script error (this box is a temporary diagnostic, safe to remove once fixed): ' + err.message + '\\n' + (err.stack || '');
  console.error(err);
}}
</script>
"""

    with open(OUT_PATH, "w", encoding="utf-8") as f:
        f.write(html)
    print(f"Written to {OUT_PATH} ({len(html)} chars)")


if __name__ == "__main__":
    main()
