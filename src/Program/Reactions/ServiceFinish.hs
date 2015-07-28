{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
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

import GHCJS.WebGL hiding (Program)
import Geometry.Space

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
    response _ (ServiceRunFinish sf) Program
            { city = City {ground = gr}
            } view@PView{cityView = cv} = do
        texarr <- typedArrayViewS texbuf
        ngr <- updateGroundView (glctx $ context view) gr (Just (Right (texarr, texsize))) (groundView cv)
        getElementById "clearbutton" >>= elementParent >>= showElement
        programIdle
        return (Left view{cityView = cv{groundView = ngr}})
        where colors = makeColors palette sf
              palette = Bezier3Palette (Vector4 255 0 0 255)
                                       (Vector4 100 255 0 255)
                                       (Vector4 0 255 100 255)
                                       (Vector4 0 0 255 255)
              (_, Just (texbuf, texsize)) = groundGridToTexArray gr 1 colors

instance Reaction Program PView ClearServiceResults "Clear service results" 0 where
    response _ _ program pview = do
        cityView' <- updateView (glctx $ context pview) (city program) (cityView pview)
        getElementById "clearbutton" >>= elementParent >>= hideElement
        return $ Left pview{cityView = cityView'}
