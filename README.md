# qua-view
Web app written in Haskell (GHCJS) for visualizing building geometry.
This is a part of [qua-kit](https://github.com/achirkin/qua-kit) project - **Quick Urban Analysis Kit** -
that is being developed under the scope of [ADvISE](http://www.ia.arch.ethz.ch/advise/) reseach project.
Qua-view can be compiled into a standalone javascript application, independent of the main project.
Other parts are responsible for a server side.

Note that qua-view depends on
[ghcjs-webgl](https://github.com/achirkin/ghcjs-webgl), 
[fastvec](https://github.com/achirkin/fastvec), and
[ghcjs-base-alt](https://github.com/achirkin/ghcjs-base-alt),
which are available only via github.

Compiled application currently is available on http://qua-kit.ethz.ch/

Supports mouse and finger control. Works best on chrome (desktop or mobile), but most other browsers working too.

### Scenario properties

We gradually add new functionality to qua-kit visualization.
As a consequence, some of the features are not representable via standard GeoJSON.
To overcome this problem, we have extended the format we use to import GeoJSON objects.
Though, the changes do not affect normal GeoJSON files.
The extended format adds one more layer of JSON to GeoJSON feature collections.
Here is a current example structure of a scenario file:
```yaml
[root]
  - name # [String] name of the scenario
  - lon  # [Number] longitude of the scenario center in degrees
  - lat  # [Number] latitude of the scenario center in degrees
  - alt  # [Number] altitude of the scenario center in meters (default is 0 if omitted)
  - srid # [Int]   (e.g. 4326 in case of WGS'84) - georeference system id
  - geometry:
      [Feature Collection Object] # content of scenario
  - properties: # key-value collection of scenario-wise properties
      - defaultHeight       # [Number] default building height in meters (if building is kept in 2D, it is extruded)
      - evaluationCellSize  # [Number] used for rendering service result - resolution of heatmaps
      - defaultScale        # [Number] multiplier to used to fix default zoom level of the camera in qua-
      - defaultBlockColor   # [#RRGGBB] set visualization colors
      - defaultActiveColor  # [#RRGGBB] set visualization colors
      - defaultLineColor    # [#RRGGBB] set visualization colors
      - defaultStaticColor  # [#RRGGBB] set visualization colors
      - mapZoomLevel        # [Int] tile server zoom level, typically something like 13-17
      - useMapLayer         # [Bool] whether to use map or not
      - mapUrl              # [String] url pattern, like "http://a.tile.stamen.com/toner/${z}/${x}/${y}.png"
                            # look at http://wiki.openstreetmap.org/wiki/Tile_servers for more information
      - forcedArea          # linear ring specifying working area of the scenario
      - hiddenProperties    # [[String]] list of object property names to not show in the viewer
```
