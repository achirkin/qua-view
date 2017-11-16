{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
#ifndef ISWORKER
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
module Workers.Types
    ( LGWMessage (..)
#ifndef ISWORKER
    , QEventTag (..)
#endif
    ) where

import GHC.Generics
#ifdef ISWORKER
import Commons.NoReflex
#else
import Commons
#endif
import Model.Scenario
import Model.Scenario.Object (ObjectRenderable(..))
import Model.Scenario.Statistics

-- | LoadGeometryWorker messages
data LGWMessage
  = LGWResult (Scenario' 'Prepared)
    -- ^ Send parsed Scenario
  | LGWSCStat ScenarioStatistics
    -- ^ Send general info about scenario object
  | LGWSError JSError
    -- ^ Something went wrong!
  deriving Generic

instance FromJSVal LGWMessage
instance ToJSVal   LGWMessage


#ifndef ISWORKER
-- | Here we need to aggregate all possible message types from all workers.
data instance QEventTag WorkerMessage evArg where
  -- | Messages that come from LoadGeometryWorker
  LGWMessage :: QEventTag WorkerMessage LGWMessage

deriveEvent ''WorkerMessage
#endif
