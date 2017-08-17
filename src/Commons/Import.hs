module Commons.Import
    ( -- * Commonly used standard types
      first, second, (***), (&&&)
    , when, unless, void, for
    , Proxy (..)
    , Semigroup (..), Monoid (..)
    , Coercible, coerce
      -- * Re-exported from Reflex
    , Reflex, Event, Behavior, Dynamic, Widget, Element, EventSelector
    , DomBuilder, DomBuilderSpace
    , AnimationTime (..), AnimationHandler
    , PointerEvent, AEventType (..), PEventType (..), PointerEventType (..)
    , WheelEvent (..), ModKey (..)
    , (=:)
      -- * Re-exported from GHC.TypeLits
    , Symbol, Nat, KnownNat, KnownSymbol, natVal, symbolVal
      -- * Dependent maps
    , DMap, DSum (..)
      -- * Some commonly used types re-exported
    , Text, JSString, JSVal, Map
    , Nullable (..), nullableToMaybe, maybeToNullable
    , PFromJSVal (..), PToJSVal (..), ToJSVal (..), ToJSString, toJSString
      -- * Other useful libraries
    , MonadIO (..), Default (..), MonadFix (..)
    , ReaderT (..), MonadTrans (..)
    ) where



import Control.Arrow (first, second, (***), (&&&))
import Control.Monad (when, unless, void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Fix
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Coerce
import Data.Default
import Data.Proxy
import Data.Semigroup
import Data.Map.Lazy (Map)
import Data.Dependent.Map (DMap, DSum (..))
import Data.JSString (JSString)
import Data.Text (Text)
import Data.Traversable (for)
import GHCJS.Types (JSVal)
import GHCJS.DOM.Types ( Nullable (..), nullableToMaybe, maybeToNullable, toJSString
                       , PFromJSVal (..), PToJSVal (..), ToJSString, ToJSVal (..))
import GHC.TypeLits
import Reflex.Class
import Reflex.Dom
import Reflex.Dom.Widget.Animation
