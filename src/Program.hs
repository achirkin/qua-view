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
import GHCJS.Useful
import Data.Geometry

--import Controllers.LuciClient

import Program.Model.Camera
import Program.Model.City
--import Program.Model.CityObject
import Program.Model.WiredGeometry
import Program.View.CityView ()
import Program.View.WiredGeometryView ()
import Program.View
import Program.Settings
import Program.Types


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


--
--renderScene :: Time -> Program -> PView -> IO ()
--renderScene ctime program view = do
--    -- prepare rendering
--    ctx <- prepareRenderState (context view) (camera program) ctime
--    -- render
--    -- setup WebGL
--    clearScreen ctx
--    draw ctx (decGrid program) (dgView view)
--    draw ctx (city program) (cityView view)
--    -- done!

renderScene' :: Program -> PView -> Time -> IO PView
renderScene' program view ctime = do
    -- prepare rendering
    ctx <- prepareRenderState (context view) (camera program) ctime
    -- render
    clearScreen ctx
    draw ctx (decGrid program) (dgView view)
    draw ctx (city program) (cityView view)
    -- done!
    return view{ context = ctx}


viewBehavior :: WebGLCanvas
             -> Event ResizeEvent -- ^ resize
             -> Event (RequireViewUpdate City)
             -> Event Time -- ^ renderings
             -> Behavior Program
             -> MomentIO (Behavior PView)
viewBehavior canvas resEvents cityUpdates renderings programB = mdo
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
    pviewE2 <- mapEventIO id $ renderScene' <$> programB <*> pviewB <@> renderings
    let pviewEAll :: Event PView
        pviewEAll = unionWith (const id) pviewE2 pviewE1
    pviewB <- stepper ipview pviewEAll :: MomentIO (Behavior PView)
    return pviewB


--  iview <- liftIO $ setupViewContext gl vpsize t sd
--  viewE <- mapEventIO (\(v, ResizeEvent c) -> updateViewPortSize c v)
--                      $ fmap (,) viewB <@> resEv
--  viewB <- stepper iview viewE


--updateProgramView :: String -> Program -> PView -> IO PView
--updateProgramView msg program pview = do
--        getElementById "clearbutton" >>= elementParent >>= hideElement
--        cityView' <- updateView (glctx $ context pview) (city program) (cityView pview)
--        logText msg
--        return pview{cityView = cityView', scUpToDate = False}




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


--instance Reaction Program PView PointerClickEvent "Get selection" 1 where
--    response _ hole (PClick LeftButton (p:_)) prog pview = getSelectId p prog pview >>= reqEvent hole . EBox >> return pview
--    response _ hole (PClick Touches [p])      prog pview = getSelectId p prog pview >>= reqEvent hole . EBox >> return pview
--    response _ _ _ _ pview = return pview

--- Selecting onbject on mouse down to allow moving objects

--instance Reaction Program PView PointerDownEvent "Fire selection action" 0 where
--    react _ _ prog@Program{controls = c} = prog{controls = c{selectedObject = 0}}
--    response _ hole (PDown _ (p:_)) program view = do
--        SelectionEvent i <- getSelectId p program view
--        when (i /= 0) . reqEvent hole . EBox $ SelectionConfirmEvent i
--        return view
--    response _ _ _ _ view = return view

