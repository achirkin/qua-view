{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
-- | Dynamics and events related to actions on scenario objects
module Program.Scenario.Object
    ( QEventTag (..)
    , objectSelectionsDyn
    , colorObjectsOnSelection
    ) where


--import qualified Data.Map.Strict as Map
import           Reflex
import           Reflex.Dom.Widget.Animation (AnimationHandler)
import qualified Reflex.Dom.Widget.Animation as Animation
import           Control.Lens
--import           Numeric.DataFrame (Mat44f, (%*))
import           Commons

import           Model.Scenario (Scenario)
import qualified Model.Scenario as Scenario
--import qualified Model.Scenario.Object.Geometry as Geometry
import           Model.Scenario.Object (ObjectId (..))
import qualified Model.Scenario.Object as Object
import           Model.Scenario.Properties
--import           Workers.LoadGeometry (LGWMessage (..), QEventTag (..))


import qualified SmallGL
--import qualified SmallGL.Types as SmallGL



-- | Selected object id events.
--   They happen when user clicks on a canvas;
--   Either some object is selected, or nothing (clicked on empty or non-selectable space).
--   The dynamic returned is guaranteed to change on every update.
objectSelectionsDyn :: ( Reflex t, MonadIO (Performable m)
                       , PerformEvent t m
                       , MonadHold t m
                       , MonadLogger m
                       , MonadFix m
                       )
                    => AnimationHandler t
                    -> SmallGL.RenderingApi
                    -> m (Dynamic t (Maybe ObjectId))
objectSelectionsDyn aHandler renderingApi = do
    selectorClicks <- performEvent $ getClicked
                                  <$> Animation.downPointersB aHandler
                                  <@ select (Animation.pointerEvents aHandler) PClickEvent
    rec selIdD <- holdDyn Nothing selIdE
        let selIdE = fmapMaybe (\(i,j) -> if i == j then Nothing else Just j)
                               $ (,) <$> current selIdD <@> selectorClicks
    logDebugEvents' @JSString "Program.Scenario.Object" $ (,) "selectedObjId" . Just <$> updated selIdD
    return selIdD
  where
    getClicked []        = pure Nothing
    getClicked ((x,y):_) = fmap f . liftIO $ SmallGL.getHoveredSelId renderingApi (round x, round y)
    f oid = if oid == 0xFFFFFFFF then Nothing else Just (ObjectId oid)


-- | Color objects when they are selected or unselected.
--   This function does not generate any Reflex objects and does not affect any existing Reflex objects.
--   It just updates SmallGL color buffers based on scenario settings.
colorObjectsOnSelection :: ( Reflex t, MonadIO (Performable m)
                           , PerformEvent t m
                           )
                        => SmallGL.RenderingApi
                        -> Behavior t Scenario
                        -> Dynamic t (Maybe ObjectId)
                        -> m ()
colorObjectsOnSelection renderingApi scB selObjD
    = performEvent_ $ f <$> scB <*> current selObjD <@> updated selObjD
  where
    f scenario oldObjId newObjId = liftIO $ do
        case mnewObj of
          Nothing -> return ()
          Just o  -> SmallGL.setObjectColor renderingApi
                                            (o^.Object.renderingId)
                                            (selectedColor $ o^.Object.objectBehavior)
        case moldObj of
          Nothing -> return ()
          Just o  -> SmallGL.setObjectColor renderingApi
                                            (o^.Object.renderingId)
                                            (Scenario.resolvedObjectColor scenario o ^. colorVeci)

      where
        selectedColor Object.Dynamic = scenario^.Scenario.selectedDynamicColor.colorVeci
        selectedColor Object.Static  = scenario^.Scenario.selectedStaticColor.colorVeci
        moldObj = oldObjId >>= \i -> scenario ^. Scenario.objects . at i
        mnewObj = newObjId >>= \i -> scenario ^. Scenario.objects . at i





