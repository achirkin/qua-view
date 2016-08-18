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
import JsHs.WebGL
import qualified JsHs.Array as JS
import JsHs.Useful
import Data.Geometry

import Program.VisualService

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

data Profile = Full | ExternalEditor | ExternalViewer deriving (Show, Eq)



-- | Data type representing the whole program state
data Program = Program
    { camera   :: !Camera
    , decGrid  :: !WiredGeometry
    , city     :: !City
    , userRole :: !Profile
    , settings :: !Settings
    }


initProgram :: Profile -- ^ determine the functionality of the program
            -> Behavior Settings
            -> Behavior Camera
            -> Behavior City
            -> Behavior Program
initProgram userProfile ctrlsB camB ciB = (\cls cam ci -> Program
    { camera = cam
    , decGrid = dg
    , city = ci
    , settings = cls
    , userRole = userProfile
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



renderScene :: Program -> PView -> Time -> IO PView
renderScene program view ctime = do
    -- selector rendering
    ctx' <- applySelector (context view) (camera program) (city program) (cityView view)
    -- prepare rendering
    ctx <- prepareRenderState ctx' (camera program) ctime
    -- render
    clearScreen ctx
    draw ctx (decGrid program) (dgView view)
    draw ctx (city program) (cityView view)
    -- done!
    return view{ context = ctx }


viewBehavior :: WebGLCanvas
             -> Event ResizeEvent -- ^ resize
             -> Event (RequireViewUpdate City)
             -> Event Time -- ^ renderings
             -> Event VisualServiceResult
             -> Behavior Program
             -> MomentIO (Behavior PView)
viewBehavior canvas resEvents cityUpdates renderings vsResultsE programB = mdo
    -- initial values
    itime <- liftIO getTime
    iprog <- valueB programB
    -- init GL
    gl <- liftIO $ getWebGLContext canvas
    -- init Context
    ctxB <- viewContextBehavior gl itime (viewportSize $ camera iprog)
                                         (vector3 (-0.5) (-0.6) (-1)) resEvents
    ictx <- valueB ctxB
    -- init object views
    dgview <- liftIO $ createView gl (decGrid iprog)
    cview <- liftIO $ createView gl (city iprog)
    cviewE <- mapEventIO (\(RequireViewUpdate c) -> createView gl c) cityUpdates
--    cviewB <- stepper cview cviewE

    -- done!
    let ipview = PView
          { context      = ictx
          , dgView       = dgview
          , cityView     = cview
  --        , luciClient   = Nothing
  --        , luciScenario = Nothing
          , scUpToDate   = False
          }
        pviewE1 = (\ctx cv -> PView
          { context      = ctx
          , dgView       = dgview
          , cityView     = cv
  --        , luciClient   = Nothing
  --        , luciScenario = Nothing
          , scUpToDate   = False
          }) <$> ctxB <@> cviewE
    pviewE2 <- mapEventIO id $ renderScene <$> programB <*> pviewB <@> renderings
--    grvB <- groundViewBehavior programB (glctx . context <$> pviewB) vsResultsE
    grvE <- groundViewEvents programB pviewB vsResultsE
    let pviewEAll :: Event PView
        pviewEAll = unionWith (const id) (unionWith (const id) pviewE2 pviewE1) (injectGrview <$> pviewB <@> grvE)
    pviewB <- stepper ipview pviewEAll :: MomentIO (Behavior PView)
    return pviewB
  where
    injectGrview pview@PView{cityView = cv} grv = pview { cityView = cv{groundView = grv}}

--    -- done!
--    let ipview = PView
--          { context      = ictx
--          , dgView       = dgview
--          , cityView     = cview
--  --        , luciClient   = Nothing
--  --        , luciScenario = Nothing
--          , scUpToDate   = False
--          }
--        pviewE1 = (\ctx cv pview -> pview
--          { context      = ctx
--          , dgView       = dgview
--          , cityView     = cv
--  --        , luciClient   = Nothing
--  --        , luciScenario = Nothing
--          , scUpToDate   = False
--          }) <$> ctxB <@> cviewE
--        pviewE3 = (\grv pview -> pview
--          { cityView     = (cityView pview)
--            { groundView = grv
--            }
--          }) <$> grvE
--    pviewE2 <-fmap (const) . mapEventIO id $ renderScene <$> programB <*> pviewB <@> renderings
--    grvE <- groundViewEvents programB pviewB vsResultsE
----    grvB <- groundViewBehavior programB (glctx . context <$> pviewB) vsResultsE
--    let pviewEAll :: Event (PView -> PView)
--        pviewEAll = unions [pviewE2, pviewE1]
--    pviewB <- stepper ipview pviewEAll
----    pviewB <- (injectGrview <$> grvB <*>) <$> stepper ipview pviewEAll :: MomentIO (Behavior PView)
--    return pviewB
--  where
--    injectGrview grv pview@PView{cityView = cv} = pview { cityView = cv{groundView = grv}}



--- Marking area for selection and firing an event with selection Id

selectOnScene :: Event a -> Event ((Program, IO PView) -> (Program,  IO PView))
selectOnScene = fmap xxx
  where
    xxx _ (program, ioview) = ( program
                              ,ioview >>= \view -> do
      ctx <- applySelector (context view) (camera program) (city program) (cityView view)
      return view{context = ctx}
      )


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
      ( Program { city = City {ground = gr, csettings = set}}
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
        where colors =  makeColors palette values
   groundViewUpdateF (_,PView { cityView     = CityView { groundView = grv} },_) = return grv
   palette = Bezier3Palette (vector4 0 0 255 240)
                            (vector4 50 255 0 240)
                            (vector4 155 255 0 240)
                            (vector4 255 0 0 240)



