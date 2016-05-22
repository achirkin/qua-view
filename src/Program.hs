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

module Program where

---- import GHCJS.Foreign
--import GHCJS.Marshal
--import Program.Model.GeoJSON


--import JavaScript.Web.Canvas (Canvas)
import JsHs.WebGL
import GHCJS.Useful
import Data.Geometry

import Controllers.LuciClient

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
        , activeService = radService
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
    , luciClient   :: !(Maybe LuciClient)
    , luciScenario :: !(Maybe LuciScenario)
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
        , luciClient   = Nothing
        , luciScenario = Nothing
        , scUpToDate   = False
        }

--
--smallCity = buildCity [ building Dynamic $ SimplePolygon
--                             [ Vector3 0 1 0
--                             , Vector3 1 1 0
--                             , Vector3 1 2 2
--                             , Vector3 0 1.5 1
--                             ]
--                       , building Static $ SimplePolygon
--                             [ Vector3 (-1) 2   (-1)
--                             , Vector3   1  1.5 (-1)
--                             , Vector3   1  2     1
--                             , Vector3 (-1) 1.5   1
--                             ]
--                       , building Static $ SimplePolygon
--                             [ Vector3 (-1) 1   (-1)
--                             , Vector3   1  1.5 (-1)
--                             , Vector3   1  1     1
--                             , Vector3 (-1) 1.5   1
--                             ]
--                       , building Dynamic $ SimplePolygon
--                             [ Vector3 (-1) 2   (-1)
--                             , Vector3   0  1.5   2
--                             , Vector3   1  2   (-1)
--                             ]
--                       , building Dynamic $ SimplePolygon
--                             [ Vector3 (-1) 1   (-1)
--                             , Vector3   1  1.5 (-1)
--                             , Vector3   1  1     1
--                             ]]
--                       [ Vector3 0 0 1
--                       , Vector3 10 0 0
--                       , Vector3 0 0 5
--                       , Vector3 0 0 (-5)
--                       , Vector3 0 0 (-8)]
--                       [ 0.4
--                       , pi/4
--                       , 0
--                       , pi - 0.00000001
--                       , pi/2]
--                       [ [ Vector3 (-4) 0.1 (-12), Vector3 (-4) 0.1 10
--                         , Vector3 5 0.1 11, Vector3 7 0.1 8
--                         , Vector3 7 0.1 (-12), Vector3 (-4) 0.1 (-12)]
--                       , [ Vector3 11 0.1 10, Vector3 15 0.1 10
--                         , Vector3 15 0.1 14, Vector3 11 0.1 13, Vector3 11 0.1 10]
--                       ]
