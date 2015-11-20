{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Reactions.ServiceFinish
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.Reactions.ServiceFinish where

--import GHCJS.WebGL
--import Geometry.Space

--import GHCJS.Marshal
import Data.Geometry

import GHCJS.Useful

import Reactive
import Controllers.GUIEvents

import Program
import Program.Model.City
import Program.Model.CityGround
import Program.Model.ScalarField
import Program.View
import Program.View.CityView
import Program.View.CityGroundView

-- | fire this event when service execution is finished
newtype ServiceRunFinish = ServiceRunFinish ScalarField


instance Reaction Program PView ServiceRunFinish "Finish service" 0 where
    response _ _ (ServiceRunFinish sf) Program
            { city = City {ground = gr, settings = set}
            } view@PView{cityView = cv} = do
        ngr <- case groundGridToTexArray gr (evalCellSize set) colors of
            (_, Nothing) -> do
                getElementById "clearbutton" >>= elementParent >>= hideElement
                updateGroundView (glctx $ context view) gr Nothing (groundView cv)
            (_, Just (texbuf, texsize)) -> do
                getElementById "clearbutton" >>= elementParent >>= showElement
                updateGroundView (glctx $ context view)
                                 gr
                                 (Just (Right (texbuf, texsize)))
                                 (groundView cv)
        programIdle
        return view{cityView = cv{groundView = ngr}}
        where colors = makeColors palette sf
              palette = Bezier3Palette (vector4 0 0 255 255)
                                       (vector4 0 255 100 255)
                                       (vector4 100 255 0 255)
                                       (vector4 255 0 0 255)

instance Reaction Program PView ClearServiceResults "Clear service results" 0 where
    response _ _ _ program pview = do
        cityView' <- updateView (glctx $ context pview) (city program) (cityView pview)
        getElementById "clearbutton" >>= elementParent >>= hideElement
        return pview{cityView = cityView'}
