{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Main (
    main
) where


-- Various thins I use
import Control.Arrow (second)
import Data.Geometry
import JsHs.JSString (JSString, append, unpack')
import Control.Monad (void, when)
import GHCJS.Useful
import Text.Read (readMaybe)
import Data.Coerce
import qualified JsHs.Array as JS
import JsHs.WebGL.Types (GLfloat)

-- Program Logic
--import Reactive
import Program
--import Program.Model.Camera (CState(..))

-- Events
--import Controllers.Pointer
--import Controllers.ElementResizing
import Controllers.GUIEvents
import qualified Controllers.GeoJSONFileImport as JFI

-- Get EventSense instances so that we can pass events into processing cycle
--import Program.Reactions ()

-- functional reactive programming
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators
import Reactive.Banana.JsHs
import Program.Model.Camera
import Program.Model.City
--import Program.View
import Program.Settings

import JsHs.Debug

main :: IO ()
main = do
    -- whole document
--    body <- documentBody

    -- drawing area
    canvas <- getElementById "glcanvas"
    -- "import geometry" button converts GeoJSON into internal representation
    importButton <- getElementById "jsonfileinput"
    clearGeomHandler <- getElementById "cleargeombutton" >>= clickHandler
    -- geoJSON updates
    geoJSONImportsHandler <- JFI.geoJSONImports
    JFI.registerButton geoJSONImportsHandler importButton

    -- get default scaling level
    let geomScale = readMaybe . unpack' $ getHtmlArg "scale"

    -- get request processing
    let userProfile = case getHtmlArg "role" of
                    "edit" -> ExternalEditor
                    "view" -> ExternalViewer
                    _      -> Full


    canv <- getCanvasById "glcanvas"
--    view <- initView program canv

    -- reactive-banana event network
    heh <- elementHandler $ coerce canvas
    network <- compile $ mdo

      -- initial state of various params
      isize <- viewPortSize heh >>= valueB
      let icamera = initCamera (realToFrac $ coordX isize)
                               (realToFrac $ coordY isize)
                               CState { viewPoint  = vector3 3 0 0
                                      , viewAngles = (-pi/5, pi/12)
                                      , viewDist   = 30 }

      -- GeoJSON updates
      geoJSONImportE <- fromAddHandler $ JFI.addHandler geoJSONImportsHandler
      clearGeometryE <- fmap (const ClearingGeometry) <$> clickEvents clearGeomHandler

      -- canvas events
      pointerE <- pointerEvents heh
      wheelE   <- wheelEvents heh
      resizeE  <- resizeEvents heh
      curPointersB <- curPointers heh
      oldPointersB <- downPointers heh
      buttonsB' <- buttons heh
      ctrlKeyB <- ctrlKey heh
      shiftKeyB <- shiftKey heh
      let modButtons True True 1 = 4
          modButtons True False 1 = 2
          modButtons False True 1 = 2
          modButtons _ _ b = b
          buttonsB = modButtons <$> shiftKeyB <*> ctrlKeyB <*> buttonsB'
          coordsB = combinePointers <$> oldPointersB <*> curPointersB

      -- program components
      cameraB <- cameraBehavior icamera
                                pointerE
                                wheelE
                                resizeE
                                buttonsB
                                coordsB

      let settingsB = pure defaultSettings { objectScale = geomScale}

      (cityChanges, cityB) <- cityBehavior settingsB geoJSONImportE clearGeometryE

      let programB = initProgram userProfile settingsB cameraB cityB

      iprogram <- valueB programB
      viewB <- viewBehavior canv iprogram isize resizeE cityChanges

      -- render scene
      updateE <- updateEvents heh
      reactimate $ (\p v t -> renderScene t p v) <$> programB <*> viewB <@> updateE

    actuate network
    play heh
    putStrLn "Hello world!"
    programIdle




combinePointers :: JS.Array Coords2D -> JS.Array Coords2D -> [(Vector2 GLfloat, Vector2 GLfloat)]
combinePointers a b = zipWith (\p1 p2 -> ( asVector p1, asVector p2)
                              ) (JS.toList a) (JS.toList b)
