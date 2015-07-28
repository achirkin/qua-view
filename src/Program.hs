-----------------------------------------------------------------------------
-- |
-- Module      :  Program
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program where


import GHCJS.WebGL hiding (Program)
import GHCJS.Useful
import Geometry.Space
import Geometry.Structure (Polygon(..))

import Controllers.LuciClient

import Program.Model.Camera
import Program.Model.City
import Program.Model.CityObject
import Program.Model.DecorativeGrid
import Program.View.CityView
import Program.View.DecorativeGridView
import Program.View

import Services
import Services.RadianceService



-- | Data type representing the whole program state; pure functional
data Program = Program
    { camera   :: !Camera
    , decGrid  :: !DecorativeGrid
    , city     :: !City
    , controls :: !Controls
    }

data Controls = Controls
    { selectedObject     :: !Int
    , activeService      :: !ServiceBox
    , availableServices  :: ![ServiceBox]
    }


initProgram :: GLfloat -- ^ width of the viewport
            -> GLfloat -- ^ height of the viewport
            -> CState -- ^ initial camera state
            -> Program
initProgram vw vh cstate = Program
    { camera = initCamera vw vh cstate
    , decGrid = createDGrid 500 100 (Vector4 0.6 0.6 0.8 1)
    , city = buildCity [ building Dynamic $ SimplePolygon
                             [ Vector3 0 1 0
                             , Vector3 1 1 0
                             , Vector3 1 2 2
                             , Vector3 0 1.5 1
                             ]
                       , building Static $ SimplePolygon
                             [ Vector3 (-1) 2   (-1)
                             , Vector3   1  1.5 (-1)
                             , Vector3   1  2     1
                             , Vector3 (-1) 1.5   1
                             ]
                       , building Static $ SimplePolygon
                             [ Vector3 (-1) 1   (-1)
                             , Vector3   1  1.5 (-1)
                             , Vector3   1  1     1
                             , Vector3 (-1) 1.5   1
                             ]
                       , building Dynamic $ SimplePolygon
                             [ Vector3 (-1) 2   (-1)
                             , Vector3   0  1.5   2
                             , Vector3   1  2   (-1)
                             ]
                       , building Dynamic $ SimplePolygon
                             [ Vector3 (-1) 1   (-1)
                             , Vector3   1  1.5 (-1)
                             , Vector3   1  1     1
                             ]]
                       [ Vector3 0 0 1
                       , Vector3 10 0 0
                       , Vector3 0 0 5
                       , Vector3 0 0 (-5)
                       , Vector3 0 0 (-8)]
                       [ 0.4
                       , pi/4
                       , 0
                       , pi - 0.00000001
                       , pi/2]
    , controls = Controls
        { selectedObject = 0
        , activeService = radService
        , availableServices = [radService]
        }
    } where radService = ServiceBox . RadianceService $ Vector3 0 3 5




-- | Statefull view of the program; used in IO actions for displaying and interaction
data PView = PView
    { context    :: !ViewContext
    , dgView     :: !DecorativeGridView
    , cityView   :: !CityView
    , luciClient :: !(Maybe LuciClient)
    }


initView :: Program -> JSElement -> IO PView
initView prog@Program
    { camera = cam
    } canvas = do
    -- current time
    ctime <- getTime
    -- init GL
    gl <- getCtx canvas
    -- init Context
    ctx <- setupViewContext gl cam ctime (Vector3 (-0.5) (-1) 0.6)
    -- init object views
    dgview <- createView gl (decGrid prog)
    cview <- createView gl (city prog)
    -- done!
    return PView
        { context    = ctx
        , dgView     = dgview
        , cityView   = cview
        , luciClient = Nothing
        }
