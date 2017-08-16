{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Commons.Local
    ( -- * Local types
      IsBusy (..), ComponentState (..), ElementClick (..)
    , CompState (..), byCompName
    , JSError (..)
    , LoadedTextContent (..)
      -- * Local functions
    , jsstring
    , castToJSString
    ) where




import Data.String (IsString (..))
import Data.Type.Equality
import qualified Data.GADT.Compare as GADT
import GHC.TypeLits
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (qAddTopDecls)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Unsafe.Coerce (unsafeCoerce)

import Commons.Import



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

-- | Let us select event by a corresponding type-level component name
data CompState a
    = forall (s :: Symbol)
    . (KnownSymbol s, a ~ ComponentState s) => CompState

-- | Convenient function to select component state
byCompName :: forall (s :: Symbol) . KnownSymbol s => CompState (ComponentState s)
byCompName = CompState

instance GADT.GEq CompState where
  geq s@CompState
      t@CompState = case sameSymbol (toCSProxy s) (toCSProxy t) of
        Nothing -> Nothing
        Just Refl -> Just GADT.Refl

instance GADT.GCompare CompState where
  gcompare s@CompState
           t@CompState = case symbolVal (toCSProxy s) `compare` symbolVal (toCSProxy t) of
                LT -> unsafeCoerce GADT.GLT
                EQ -> unsafeCoerce GADT.GEQ
                GT -> unsafeCoerce GADT.GGT

toCSProxy :: forall (s :: Symbol) . CompState (ComponentState s) -> Proxy s
toCSProxy _ = Proxy

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


-- | Error messages coming from various widgets, etc.
newtype JSError = JSError { getJSError :: JSString }
  deriving (PToJSVal, ToJSVal, ToJSString, IsString, Show, Eq)


-- | Try to cast JavaScript object to a string.
--   If the object is not a valid string, return Nothing.
castToJSString :: PToJSVal a => a -> Maybe JSString
castToJSString = nullableToMaybe . js_castToJSString . pToJSVal

foreign import javascript unsafe "((typeof $1 === 'string') ? $1 : null)"
    js_castToJSString :: JSVal -> Nullable JSString


-- | Usually we get geometry in a form of GeoJSON string,
--   but we can also load some other text files from various places.
--   Hence, this data type is used to depict some transefrable chunk of JavaScript string data.
newtype LoadedTextContent = LoadedTextContent { getTextContent :: JSString }

