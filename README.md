# qua-view
Web app written in Haskell (GHCJS) for visualizing building geometry.
This is a part of [qua-kit](https://github.com/achirkin/qua-kit) project - **Quick Urban Analysis Kit** -
that is being developed under the scope of [ADvISE](http://www.ia.arch.ethz.ch/advise/) reseach project.
Qua-view can be compiled into a standalone javascript application, independent of the main project.
Other parts are responsible for a server side.

Compiled application currently is available on http://qua-kit.ethz.ch/

Supports mouse and finger control. Works best on chrome (desktop or mobile), but most other browsers working too.

## Qua-kit scenario representation

We gradually add new functionality to qua-kit visualization.
As a consequence, some of the features are not representable via standard GeoJSON. To overcome this problem, we have extended the format we use to import GeoJSON objects.
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
                # look at Model.Scenario module for these properties
      - defaultObjectHeight  # [Number] default building height in meters (default: 3.5)
      - selectedDynamicColor # [#RRGGBB] set visualization colors (default: "#FF6060FF")
      - selectedGroupColor   # [#RRGGBB] set visualization colors (default: "#CC8888E0")
      - selectedStaticColor  # [#RRGGBB] set visualization colors (default: "#BB8888FF")
      - defaultStaticColor   # [#RRGGBB] set visualization colors (default: "#808088E0")
      - defaultBlockColor    # [#RRGGBB] set visualization colors (default: "#C0C082FF")
      - defaultLineColor     # [#RRGGBB] set visualization colors (default: "#CC6666FF")
      - defaultPointColor    # [#RRGGBB] set visualization colors (default: "#006666FF")
      - viewDistance         # [Number] distance in meters where objects fade in white (default: 2000)
      - mapZoomLevel         # [Int] tile server zoom level, typically something like 13-17 (default: 15)
      - useMapLayer          # [Bool] whether to use map or not (default: false)
      - mapOpacity           # [Number] make a map semi-transparent (default: 0.8)
                             # (min (transparent): 0.0; max (opaque): 1.0)
      - mapUrl               # [String] url pattern, like "http://a.tile.stamen.com/toner/${z}/${x}/${y}.png"
                             # look at http://wiki.openstreetmap.org/wiki/Tile_servers for more information
                             # (default: "https://a.tile.openstreetmap.org/${z}/${x}/${y}.png")
      - hiddenProperties     # [[String]] list of object property names to not show in the viewer
      - evaluationCellSize   # [Number] used for rendering service result - resolution of heatmaps
      - previewImgUrl        # [String] url to show an image above the info panel
```
Special object properties:
```yaml
[Feature]
  - type: "Feature"
  - geometry: {..}     # GeoJSON Geometry object (any object except GeometryCollection)
  - properties: # key-value collection of object-specific properties
                # look at Model.Scenario.Object module for these properties
      - geomID         # [Int] luci-compatible id of an object
                       # (assigned by qua-view if missing in a scenario file)
                       # must be in a range [0x00000001 .. 0xFFFFFFFE] to work in qua-view properly.
      - groupID        # [Int] An identifier grouping multiple objects to move together
      - height         # [Number] height of a building to be extruded if it is given in 2D
      - previewImgUrl  # [String] url to show an image above the info panel
      - viewColor      # [#RRGGBB] set the color of an object explicitly
      - static         # [Bool] cannot move object if true (default: false)
      - selectable     # [Bool] if we can click on object to select it
                       # (default: true)
                       # (selectable = false implies static = true)
      - visible        # [Bool] if object is renderable at all
                       # (default: true)
                       # (visible = false implies selectable = false and static = true)
      - special        # [String] :: [SpecialObjectType] defines this object as a special control object
 ```
Special object types are used to control `qua-view` behavior. We use `special :: String` property of object to define a special object. Below is the list of possible values of `special` property and their meaning.

  * `"special": "camera"` means we define a default camera position in `qua-view` for this scenario.
    * Must be at most one for a scenario.
    * Geometry type must be a valid `"MultiPoint"`.
    * Geometry must contain exactly two 3D points [camera position, look at point].
    * Default property values:
       ```yaml
       - static: true
       - selectable: false
       - visible: false
       ```
    * Example:
      ```json
      { "type": "Feature"
      , "geometry":
        { "type": "MultiPoint"
        , "coordinates": [[camera_x,camera_y,camera_z],[lookat_x,lookat_y,lookat_z]]
        }
      , "properties": { "special": "camera"}
      }
      ```
  * `"special": "forcedArea"` is a polygon specifying working area of the scenario; used e.g. by the luci services manager to determine the area to be evaluated.
    * Must be at most one for a scenario.
    * Geometry type must be a valid `"Polygon"`.
    * Default property values:
       ```yaml
       - static: true
       - selectable: false
       - visible: true
       - viewColor: "#FFFFFF99"
       ```
    * Example:
      ```json
      { "type": "Feature"
      , "geometry":
        { "type": "MultiPoint"
        , "coordinates": [[[...]]]
        }
      , "properties": { "special": "forcedArea"}
      }
      ```
  * `"special": "template"` states that an object can be used as a template to create new objects.
    It appears on the geometry pane and allows to drag-&-drop it to a scene to create a copy.
    * There may be any number of template objects
    * A new (cloned) object retains all the properties of the template except `static`, `selectable`,`template`, and `visible`,
      which are reset to their defaults (removed from the property list).
      This allows to hide a template object in a scene if necessary.
    * If a template object is a part of a group, the whole group is considered to be a template.


## Development

### Setup

First, we need to install GHCJS. We install GHCJS via stack, but before that some dependencies need to be installed.
Refer to [GHCJS documentation](https://github.com/ghcjs/ghcjs/tree/ghc-8.0) to check, which are needed.
In particular, the following command installs everything needed on Ubuntu:

    sudo apt-get install libtinfo-dev nodejs nodejs-legacy npm

(`nodejs-legacy is only needed for the alias from `node` to `nodejs`.)

Next step is to install haskell dependencies. At this moment we use stack snapshot `lts-8.21`:

    stack install alex happy haddock-2.17.4 haddock-api-2.17.4 hscolour hoogle --resolver=lts-8.21

Then, clone this repository and setup GHCJS using stack:

```
git clone -b reflex https://github.com/achirkin/qua-view
cd qua-view
ln -s `stack path --compiler-bin`/ghcjs-pkg `stack path --compiler-bin`/ghc-pkg
stack setup
```

### Build documentation

Another important step is to correctly build haddocks, because our crucial dependencies are not in hackage or stackage.
Run haddock in the `qua-view` project folder (do this before running `stack build` the first time).

```
stack haddock
```

The last thing is to run hoogle to make it easier to lookup function names.

```
stack hoogle generate -- --local
stack hoogle -- server --port=8080 --local
```

Also have a look at the following file for `GHCJS` and `JavaScript` module docs

    ~/.ghcjs/XXX/ghcjs/doc/html/index.html

and this [Reflex tutorial](https://github.com/reflex-frp/reflex-platform#tutorial)
and [function reference](https://github.com/reflex-frp/reflex/blob/develop/Quickref.md).

#### Build documentation for executables

Unfortunately, at this moment, `stack haddock` does not allow building documentation for executables.
However, we still can use `cabal haddock --executables` with `stack` environment to build documentation.
```
# make sure cabal is compiled with host compiler for the same snapshot
stack build cabal-install --compiler=ghc-8.0.2
# configure cabal
env PATH=$(stack path --bin-path):$(stack path --bin-path --compiler=ghc-8.0.2):$PATH \
  cabal configure --ghcjs \
  --package-db=clear \
  --package-db=global \
  --package-db=$(stack path --global-pkg-db) \
  --package-db=$(stack path --snapshot-pkg-db) \
  --package-db=$(stack path --local-pkg-db)
# build documentation
env PATH=$(stack path --bin-path):$(stack path --bin-path --compiler=ghc-8.0.2):$PATH \
  cabal haddock --executables
```


#### Hoogle for both qua-server and qua-view

```
cd [root qua-kit folder]
stack install hoogle haddock-2.17.4 haddock-api-2.17.4
./config/haddock-hoogle-all.sh
stack exec hoogle -- server --port=8080 --local
```

### Build the project standalone

Finally, you can generate the website into the `/web` directory:

    stack build --file-watch

To view the result, you need to run a small web server due to browsers' same-origin policy, e.g.

    cd qua-view/web
    python -m SimpleHTTPServer
