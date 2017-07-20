{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Types
    ( -- * Re-exported from Reflex
      Reflex, Event, Behavior, Dynamic, Widget
      -- * Re-exported from GHC.TypeLits
    , Symbol, Nat
      -- * Local types
    , IsBusy (..)
    ) where

import GHC.TypeLits
import Reflex.Class
import Reflex.Dom.Main

-- | Whether the program or component is busy doing some extensive work.
data IsBusy (s :: Symbol) = Busy | Idle
    deriving (Eq, Show)
