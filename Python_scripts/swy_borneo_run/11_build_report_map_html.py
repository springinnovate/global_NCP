"""Build the self-contained interactive Leaflet map HTML fragment for the SWY status report.

Reads data/borneo_lulc/output_maps/layers.json (built by 10_render_output_maps.py) and writes
docs/reports/_output_map.html, included into swy_status_report.qmd via
`{{< include _output_map.html >}}`. Kept as a separate file rather than inlined directly in the
.qmd because the embedded base64 image data is large (a few MB of text) — better kept out of the
primary, human-edited source file.

Leaflet's ImageOverlay draws each raster as a simple rectangle between two lat/lon corners
(no per-pixel reprojection) — fine here since Borneo sits close to the equator, where Web
Mercator's cos(latitude) distortion is negligible.
"""
import json

LAYERS_JSON = "data/borneo_lulc/output_maps/layers.json"
OUT_PATH = "docs/reports/_output_map.html"


def main():
    with open(LAYERS_JSON) as f:
        layers = json.load(f)

    overlay_js = []
    checkbox_js = []
    legend_html = []
    for i, layer in enumerate(layers):
        var = f"layer_{layer['id']}"
        overlay_js.append(
            f'const {var} = L.imageOverlay("data:image/png;base64,{layer["b64"]}", '
            f'{json.dumps(layer["bounds"])}, {{opacity: 0.75}});'
        )
        checked = "checked" if i == 0 else ""
        checkbox_js.append(f'overlays["{layer["label"]}"] = {var};')
        legend_html.append(
            f'<div class="swy-legend-row"><span class="swy-legend-swatch" '
            f'style="background:linear-gradient(to right, #440154, #3b528b, #21918c, #5ec962, #fde725);">'
            f'</span><span>{layer["label"]}: {layer["vmin"]:.1f} &ndash; {layer["vmax"]:.1f} mm</span></div>'
        )
        if i == 0:
            first_var = var

    html = f"""
<style>
  #swy-output-map {{ height: 600px; width: 100%; border-radius: 6px; }}
  .swy-legend {{ font-size: 0.85em; margin-top: 8px; }}
  .swy-legend-row {{ display: flex; align-items: center; gap: 8px; margin: 2px 0; }}
  .swy-legend-swatch {{ display: inline-block; width: 60px; height: 10px; border-radius: 2px; }}
</style>

<div id="swy-output-map"></div>
<div class="swy-legend">
  {''.join(legend_html)}
  <div style="margin-top:6px; opacity:0.75;">Viridis colormap. L_sum is capped at its 99th
  percentile so the real spatial signal stays visible against the ~4.6% of pixels with runaway
  flow-accumulation values (see the write-up above) — those pixels render as the brightest yellow.
  Toggle layers with the control in the top right; drag/scroll to pan and zoom.</div>
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

  {chr(10).join(overlay_js)}
  {first_var}.addTo(map);

  const overlays = {{}};
  {chr(10).join(checkbox_js)}

  L.control.layers(
    {{ 'Satellite (Esri)': satellite, 'Street (OSM)': osm }},
    overlays,
    {{ collapsed: false }}
  ).addTo(map);

  map.fitBounds({json.dumps(layers[0]['bounds'])});
}})();
</script>
"""
    with open(OUT_PATH, "w", encoding="utf-8") as f:
        f.write(html)
    print(f"written to {OUT_PATH}, {len(html)/1e6:.1f}MB")


if __name__ == "__main__":
    main()
