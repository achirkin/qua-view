{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | This module should not depend on anything in qua-view.
--   It can be imported by any other modules to use certain handy functions or types.
module Commons
    ( -- * Local types
      IsBusy (..), ComponentState (..), ElementClick (..)
      -- * Local functions
    , jsstring
      -- * Commonly used standard types
    , first, second, (***), (&&&)
    , when, unless, void
    , Proxy (..)
    , Semigroup (..), Monoid (..)
    , Coercible, coerce
      -- * Re-exported from Reflex
    , Reflex, Event, Behavior, Dynamic, Widget, Element
    , AnimationTime (..), AnimationHandler
    , PointerEvent, AEventType (..), PEventType (..), PointerEventType (..)
    , WheelEvent (..), ModKey (..)
    , (=:)
      -- * Re-exported from GHC.TypeLits
    , Symbol, Nat, KnownNat, KnownSymbol, natVal, symbolVal
      -- * Some commonly used types re-exported
    , Text, JSString, JSVal
      -- * Other useful libraries
    , MonadIO (..), Default (..)
    ) where


import Control.Arrow (first, second, (***), (&&&))
import Control.Monad (when, unless, void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Coerce
import Data.Default
import Data.Proxy
import Data.Semigroup
import Data.Text (Text)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (qAddTopDecls)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
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
    show s = "ElementClick :: " ++ symbolVal s

-- | Represents a state of a component (e.g. DOM element or any other component)
data ComponentState (s :: Symbol) = Active | Inactive
    deriving Eq
instance KnownSymbol s => Show (ComponentState s) where
    show s = "ComponentState :: " ++ symbolVal s





-- | Create a multiline JavaScript string using a splice.
--   Presumably, it performs faster than any other way of creating JSString, because it avoids
--   conversion between HS string and JSString.
--
--   Use #{varName} syntax to embed variable JSString values at runtime.
jsstring :: QuasiQuoter
jsstring = QuasiQuoter
    { quoteExp = \s -> do
        let ss = unlines $ stripLines s
            (varNames, chunks) = first (fmap $ VarE . mkName) $ getWithVars ss
            (funCode, funType) = mkFunAndArgs chunks
        loc <- location
        fName <- newName ("jsstring"
                         ++ '_' : (loc_module loc >>= \c -> if c == '.' then "zi" else [c])
                         ++ '_' : showLoc (loc_start loc)
                         ++ '_' : showLoc (loc_end loc))
        qAddTopDecls [ForeignD (ImportF JavaScript Unsafe funCode fName funType )]
        return $ foldl AppE (VarE fName) varNames
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }
  where
    showLoc (i,j) = show i ++ "_" ++ show j
    -- get list of JSString lines, with removed indentation
    stripLines s = let ls = lines s
                       countSp "" = maxBound
                       countSp l  | all (' ' ==) l = maxBound
                                  | otherwise = length $ takeWhile (' ' ==) l
                       indent = minimum $ fmap countSp ls
                   in fmap (drop indent) ls
    -- find unbound variables and insert js vars on their places
    fetchVarName ('}':s) = ("",s)
    fetchVarName (c:s)   = first (c:) $ fetchVarName s
    fetchVarName []      = error "could not get variable name - end of input."
    getWithVars []          = ([], [[]])
    getWithVars ('#':'{':s) = let (v, s') = fetchVarName s
                              in (v:) *** ([]:) $ getWithVars s'
    getWithVars (c:s)       = second (overHead (c:)) $ getWithVars s
    overHead _ [] = []
    overHead f (c:cs) = f c : cs
    -- construct function
    mkFunAndArgs [] = mkFunAndArgs' 0 [];
    mkFunAndArgs chunks = first ("$r = [" ++) $ mkFunAndArgs' 1 chunks
    mkFunAndArgs' :: Int -> [String] -> (String, Type)
    mkFunAndArgs' _ [] = ("$r = [].join('');", ConT ''JSString)
    mkFunAndArgs' _ [c] = (show c ++ "].join('');", ConT ''JSString)
    mkFunAndArgs' i (c:chunks) = let (e, a) = mkFunAndArgs' (i+1) chunks
                                 in (show c ++ ",$" ++ show i ++ ',':e, AppT ArrowT (ConT ''JSString) `AppT` a)
