{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module CommonTypes
    ( -- * Re-exported from Reflex
      Reflex, Event, Behavior, Dynamic, Widget, Element
    , AnimationTime (..), AnimationHandler
    , PointerEvent, AEventType (..), PEventType (..), PointerEventType (..)
    , WheelEvent (..), ModKey (..)
      -- * Re-exported from GHC.TypeLits
    , Symbol, Nat
      -- * Some commonly used types re-exported
    , Text, JSString, JSVal
      -- * Local types
    , IsBusy (..)
    ) where

import Data.Text (Text)
import GHCJS.Types (JSString, JSVal)
import GHC.TypeLits
import Reflex.Class
import Reflex.Dom
import Reflex.Dom.Widget.Animation

-- | Whether the program or component is busy doing some extensive work.
data IsBusy (s :: Symbol) = Busy | Idle
    deriving (Eq, Show)
