{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    , IsBusy (..), ComponentState (..), ElementClick (..)
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

-- | Click event tagged by the name of a program component that was clicked.
data ElementClick (s :: Symbol) = ElementClick
    deriving Eq
instance KnownSymbol s => Show (ElementClick s) where
    show _ = "ElementClick :: " ++ symbolVal (ElementClick @s)

-- | Represents a state of a component (e.g. DOM element or any other component)
data ComponentState (s :: Symbol) = Active | Inactive
    deriving (Eq, Show)
