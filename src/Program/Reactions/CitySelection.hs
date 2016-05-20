{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Reactions.CitySelection
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.Reactions.CitySelection () where


import Reactive
import Controllers.Pointer

import Program
import Program.Model.Camera
import Program.Model.City
import Program.Model.CityObject
import Program.View


-- remember state on click/mousedown to transform buildings later

instance Reaction Program PView SelectionEvent "Select in City" 1 where
    react _ (SelectionEvent i) program = program{city = (city program) {activeObjId = i}}

instance Reaction Program PView SelectionConfirmEvent "Remember selection state" 1 where
    react _ (SelectionConfirmEvent i) program@Program{city = ci} =
        if activeObjId ci /= i
        then program
        else program
            { controls = (controls program){selectedObject = i}
            , city = ci{activeObjSnapshot = getObject i ci }
            }


-- transform buildings

transformCity :: (Camera -> LocatedCityObject -> LocatedCityObject)
              -> Program -> Program
transformCity _ program@Program{controls = Controls{ selectedObject = 0 }} = program
transformCity f program@Program
    { city = c@City{activeObjSnapshot = Just obj}
    , controls = Controls{ selectedObject = i }
    } = program
    { city = setObject i (f (camera program) obj) c
    }
transformCity _ program = program

geometryChanged :: Bool -> PView -> IO PView
geometryChanged True pview@PView{scUpToDate = True} = return pview{scUpToDate = False}
geometryChanged _ pview = return pview

instance Reaction Program PView PointerMoveEvent "Move City Object" 0 where
    react _ (PMove _            _ []             )   = id
    react _ (PMove LeftButton   _ ((npos,opos):_))   = transformCity (dragObject opos npos)
    react _ (PMove RightButton  _ ((npos,opos):_))   = transformCity (rotateObject opos npos)
    react _ (PMove Touches      _ [(npos,opos)]  )   = transformCity (dragObject opos npos)
    react _ (PMove Touches      _ [(n1,o1),(n2,o2)]) = transformCity (twoFingerObject (o1,o2) (n1,n2))
    react _ (PMove Touches      _ (_:_:_:_))         = id
    react _ (PMove MiddleButton _ _)                 = id
    response _ _ (PMove _            _ []             )       _ = geometryChanged False
    response _ _ (PMove LeftButton   _ ((_npos,_opos):_))     _ = geometryChanged True
    response _ _ (PMove RightButton  _ ((_npos,_opos):_))     _ = geometryChanged True
    response _ _ (PMove Touches      _ [(_npos,_opos)]  )     _ = geometryChanged True
    response _ _ (PMove Touches      _ [(_n1,_o1),(_n2,_o2)]) _ = geometryChanged True
    response _ _ (PMove Touches      _ (_:_:_:_))             _ = geometryChanged False
    response _ _ (PMove MiddleButton _ _)                     _ = geometryChanged False
