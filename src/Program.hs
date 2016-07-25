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

module Program where

---- import GHCJS.Foreign
--import GHCJS.Marshal
--import Program.Model.GeoJSON
import Controllers.GUIEvents

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

import Services
import qualified Services.Isovist as Services
import qualified Services.Radius as Services

import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Banana.JsHs

data Profile = Full | ExternalEditor | ExternalViewer deriving (Show, Eq)

-- | Data type representing the whole program state
data Program = Program
    { camera   :: !Camera
    , decGrid  :: !WiredGeometry
    , city     :: !City
    , userRole :: !Profile
    , controls :: !Controls
    }

data Controls = Controls
    { selectedObject    :: !Int
    , activeService     :: !ServiceBox
    , availableServices :: ![ServiceBox]
    , objectScale       :: !(Maybe GLfloat)
    }

initProgram :: GLfloat -- ^ width of the viewport
            -> GLfloat -- ^ height of the viewport
            -> CState -- ^ initial camera state
            -> Profile -- ^ determine the functionality of the program
            -> Maybe GLfloat -- ^ default scaling of the objects
            -> Program
initProgram vw vh cstate userProfile objScale = Program
    { camera = initCamera vw vh cstate
    , decGrid = createDecorativeGrid 500 100 (vector4 0.6 0.6 0.8 1)
    , city = emptyCity -- buildCity [] [] [] []
    , controls = Controls
        { selectedObject = 0
        , activeService = isovistService -- radService
        , availableServices = [radService, isovistService]
        , objectScale = objScale
        }
    , userRole = userProfile
    } where radService = ServiceBox . Services.Radius $ vector3 0 3 5
            isovistService = ServiceBox (Services.Isovist Services.Area)




-- | Statefull view of the program; used in IO actions for displaying and interaction
data PView = PView
    { context      :: !ViewContext
    , dgView       :: !(View WiredGeometry)
    , cityView     :: !(View City)
--    , luciClient   :: !(Maybe LuciClient)
--    , luciScenario :: !(Maybe LuciScenario)
    , scUpToDate   :: !Bool
    }


initView :: Program -> WebGLCanvas -> IO PView
initView prog@Program
    { camera = cam
    } canvas = do
    -- current time
    ctime <- getTime
    -- init GL
    gl <- getWebGLContext canvas
    -- init Context
    ctx <- setupViewContext gl cam ctime (vector3 (-0.5) (-0.6) (-1))
    -- init object views
    dgview <- createView gl (decGrid prog)
    cview <- createView gl (city prog)
    -- done!
    return PView
        { context      = ctx
        , dgView       = dgview
        , cityView     = cview
--        , luciClient   = Nothing
--        , luciScenario = Nothing
        , scUpToDate   = False
        }


cityUpdates :: Event (CitySettings -> City -> City)
            -> Event ((Program, IO PView) -> (Program, IO PView ))
cityUpdates = fmap c
  where
    c f (program, ioview) =
      let np = program
            { city = f (defaultCitySettings { defScale = objectScale . controls $ program})
                       (city program)
            }
          ionv = ioview >>= \view -> ((\cv -> view{cityView = cv})
                            <$> createView (glctx $ context view) (city np))
      in ( np, ionv )

cityClears :: Event ClearingGeometry
           -> Event ((Program, IO PView) -> (Program, IO PView ))
cityClears = fmap c
  where
    c _ (program, ioview) =
      let np = program { city = clearCity (city program) }
          ionv = ioview >>= updateProgramView "Cleared geometry." np
      in (np, ionv)


renderings :: Event (Double, Program, PView) -> MomentIO (Event PView)
renderings = mapEventIO (\(t,p,v) -> renderScene t p v )

renderScene :: Double -> Program -> PView -> IO PView
renderScene ctime program view = do
    -- prepare rendering
    ctx <- prepareRenderState (context view) (camera program) ctime
    -- render
    clearScreen ctx
    draw ctx (decGrid program) (dgView view)
    draw ctx (city program) (cityView view)
    -- done!
    return view{context = ctx}

updateProgramView :: String -> Program -> PView -> IO PView
updateProgramView msg program pview = do
        getElementById "clearbutton" >>= elementParent >>= hideElement
        cityView' <- updateView (glctx $ context pview) (city program) (cityView pview)
        logText msg
        return pview{cityView = cityView', scUpToDate = False}




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
    getCoord e = let c = pointers e JS.! 0 in vector2 (realToFrac $ posX c) (realToFrac $ posY c)



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
    getCoord e = let c = pointers e JS.! 0 in vector2 (realToFrac $ posX c) (realToFrac $ posY c)


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

