-----------------------------------------------------------------------------
-- |
-- Module      :  Program
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Program where

---- import GHCJS.Foreign
--import GHCJS.Marshal
--import Program.Model.GeoJSON
--import Controllers.GUIEvents

--import JavaScript.Web.Canvas (Canvas)
import GHCJS.Concurrent
import JsHs.WebGL
--import JsHs.Types
import qualified JsHs.Array as JS
import JsHs.Useful
import Data.Geometry
import Data.Monoid ((<>))
-- import Control.Concurrent

import Program.VisualService

import qualified Program.Controllers.GUI as GUI
import Program.Model.Camera
import Program.Model.City
--import Program.Model.CityObject
import Program.Model.WiredGeometry
import Program.View.CityView (groundView, CityView (CityView))
import Program.View.WiredGeometryView ()
import Program.View.CityGroundView
import Program.View
import Program.Settings
import Program.Types
import Program.Model.CityGround

import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Banana.JsHs

--import JsHs.Debug



-- | Data type representing the whole program state
data Program = Program
    { camera   :: !Camera
    , decGrid  :: !WiredGeometry
    , city     :: !City
    , settings :: !Settings
    }


initProgram :: Behavior Settings
            -> Behavior Camera
            -> Behavior City
            -> Behavior Program
initProgram ctrlsB camB ciB = (\cls cam ci -> Program
    { camera = cam
    , decGrid = dg
    , city = ci
    , settings = cls
    }) <$> ctrlsB <*> camB <*> ciB
  where
    dg = createDecorativeGrid 500 100 (vector4 0.6 0.6 0.8 1)



-- | Statefull view of the program; used in IO actions for displaying and interaction
data PView = PView
    { context      :: !ViewContext
    , dgView       :: !(View WiredGeometry)
    , cityView     :: !(View City)
--    , luciClient   :: !(Maybe LuciClient)
--    , luciScenario :: !(Maybe LuciScenario)
    , scUpToDate   :: !Bool
    }



renderScene :: IO (Maybe PictureVal) -> Program -> PView -> Time -> IO (PView, Maybe PictureVal)
renderScene getPicture program view ctime = withoutPreemption $ do
    -- selector rendering
    ctx' <- applySelector (context view) (camera program) (city program) (cityView view)
    -- prepare rendering
    ctx <- prepareRenderState ctx' (camera program) ctime
    -- render
    clearScreen ctx
    draw ctx (decGrid program) (dgView view)
    draw ctx (city program) (cityView view)
    -- render to picture if needed
    mpic <- getPicture
    -- done!
    return (view{ context = ctx }, mpic)



viewBehavior :: WebGLCanvas
             -> Event WantPicture -- ^ resize
             -> Event ResizeEvent -- ^ resize
             -> Event (RequireViewUpdate City)
             -> Event Time -- ^ renderings
             -> Event VisualServiceResult
             -> Behavior Program
             -> MomentIO (Behavior PView, Event PictureVal)
viewBehavior canvas wantPicE resEvents cityUpdates renderings vsResultsE programB = mdo
    reactimate $ renderScenarioServiceResult <$> vsResultsE

    -- initial values
    itime <- liftIO getTime
    iprog <- valueB programB
    -- init GL
    gl <- liftIO $ getWebGLContext canvas
    -- init Context
    (ctxB, ctxE) <- viewContextBehavior gl itime (viewportSize $ camera iprog)
                                         (vector3 (-0.5) (-0.6) (-1)) resEvents
    ictx <- valueB ctxB
    -- init object views
    dgview <- liftIO . withoutPreemption $ createView gl (decGrid iprog)
    cview <- liftIO . withoutPreemption$ createView gl (city iprog)
    cUpdatesDelayed <- mapEventIO (\x -> return x) cityUpdates -- threadDelay 1000000 >>
    cviewE <- mapEventIO (\(pv, RequireViewUpdate c) -> withoutPreemption $ updateView gl c (cityView pv)
                         ) $ (,) <$> pviewB <@> cUpdatesDelayed

    -- done!
    let ipview = PView
          { context      = ictx
          , dgView       = dgview
          , cityView     = cview
          , scUpToDate   = False
          }
        pviewE1 = (\cv pview -> pview
          { dgView       = dgview
          , cityView     = cv
          , scUpToDate   = False
          }) <$> cviewE
        pviewE0 = (\ctx pview -> pview{context = ctx}) <$> ctxE
        pviewE3 = (\grv pview@PView{cityView = cv} -> pview { cityView = cv{groundView = grv}}) <$> grvE
    wantPictureB <- stepper (return Nothing) ( unionWith (const id)
                                   (return Nothing <$ renderings)
                                   ((Just <$> js_CanvToDataUrl canvas) <$ wantPicE))
    pviewE2pictureE <- mapEventIO id $ renderScene <$> wantPictureB <*> programB <*> pviewB <@> renderings
    let pviewE2 = const . fst <$> pviewE2pictureE
        pictureE = filterJust $ snd <$> pviewE2pictureE
--    grvB <- groundViewBehavior programB (glctx . context <$> pviewB) vsResultsE
    grvE <- groundViewEvents programB pviewB vsResultsE
    let pviewEAll :: Event (PView -> PView)
        pviewEAll = unions [pviewE0, pviewE1, pviewE2, pviewE3]

--        unionWith (const id) (unionWith (const id) pviewE2 pviewE1) (injectGrview <$> pviewB <@> grvE)
    pviewB <- accumB ipview pviewEAll :: MomentIO (Behavior PView)

    -- let pviewB = (\p c -> p {cityView = c}) <$> pviewB' <*> cviewB
    return (pviewB, pictureE)



-- | Render scenario service results on side menu on the left
renderScenarioServiceResult :: VisualServiceResult -> IO ()
renderScenarioServiceResult (VisualServiceResultScenario (Left str))
  = GUI.showScenarioServiceResultString str
renderScenarioServiceResult (VisualServiceResultScenario (Right buf))
  = GUI.showScenarioServiceResultPng buf
renderScenarioServiceResult _ = return ()

--- Selecting object on click

getSelectId :: Vector2 GLfloat -> Program -> PView -> IO SelectionEvent
getSelectId v program view = getSelection (context view) (camera program) v


selection :: Behavior (Program, PView) -> Event PointerEvent -> MomentIO (Event SelectionEvent)
selection beh ev = mapEventIO spawnEvent $ (,) <$> beh <@> filterE clickEvent ev
  where
    clickEvent (PointerUp v) = JS.length (pointers v) == 1
    clickEvent _ = False
    spawnEvent ((p,v), PointerUp e) = getSelectId (getCoord e) p v
    spawnEvent _ = undefined
    getCoord e = asVector (pointers e JS.! 0)



selectionConfirm :: Behavior (Program, PView) -> Event PointerEvent
                 -> MomentIO (Event SelectionConfirmEvent)
selectionConfirm beh ev = fmap filterJust
      . mapEventIO spawnEvent
      $ (,) <$> beh <@> filterE clickEvent ev
  where
    clickEvent (PointerDown _) = True
    clickEvent _ = False
    spawnEvent ((p,v), PointerDown e) = do
      SelectionEvent i <- getSelectId (getCoord e) p v
      return $ if i /= 0 then Just $ SelectionConfirmEvent i
                         else Nothing
    spawnEvent _ = undefined
    getCoord e = asVector (pointers e JS.! 0)




groundViewEvents :: Behavior Program
                 -> Behavior PView
                 -> Event VisualServiceResult
                 -> MomentIO (Event CityGroundView)
groundViewEvents programB pviewB vsResultE = mapEventIO groundViewUpdateF $ (,,) <$> programB <*> pviewB <@> vsResultE
  where
   groundViewUpdateF
      ( Program { city = City {ground = gr, csettings = set }}
      , PView
          { context      = ViewContext {glctx = gl}
          , cityView     = CityView { groundView = grv}
          }
      , VisualServiceResultPoints _ values
      ) = case groundGridToTexArray gr (evalCellSize set) colors of
            (_, Nothing) ->
                updateGroundView gl gr Nothing grv
            (_, Just (texbuf, texsize)) ->
                updateGroundView gl
                                 gr
                                 (Just (Right (texbuf, texsize)))
                                 grv
        where colors =  makeColors palette (Just $ groundStencil gr (evalCellSize set)) values
   groundViewUpdateF (_,PView { cityView     = CityView { groundView = grv} }, VisualServiceResultUnknown _ err) =
      logText' ("Failed to render a service result: " <> err) >> return grv
   groundViewUpdateF (_,PView { cityView     = CityView { groundView = grv} },_) = return grv
   palette = Bezier3Palette (vector4 0 0 255 240)
                            (vector4 50 255 0 240)
                            (vector4 155 255 0 240)
                            (vector4 255 0 0 240)

foreign import javascript unsafe "h$makePreview($1)"
  js_CanvToDataUrl :: WebGLCanvas -> IO PictureVal
