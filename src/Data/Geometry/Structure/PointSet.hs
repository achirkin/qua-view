{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds, KindSignatures, GHCForeignImportPrim #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Structure.PointSet
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Structure.PointSet
    ( PointSet (..)
    , PointArray ()
    , length, pointArray, toList, index, unflatten
    , pcaVectors
    , project1D, projectND
    ) where


import Prelude hiding (length)

import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import GHC.TypeLits
import Data.Typeable (Proxy (..))

import GHCJS.Types
import GHCJS.Marshal.Pure (PFromJSVal(..))

import Data.Geometry



-- | Array of points
newtype PointArray (n::Nat) x = PointArray JSVal
instance IsJSVal (PointArray n x)
instance PFromJSVal (PointArray n x) where
    pFromJSVal = PointArray

-- | Create a PointArray
pointArray :: [Vector n x]
           -> PointArray n x
pointArray xs = js_fromList  . unsafeCoerce . seqList $ xs

-- | Get list of points from LinearRing (without repeatative last point)
toList :: PointArray n x -> [Vector n x]
toList = unsafeCoerce . js_toList


{-# INLINE length #-}
foreign import javascript unsafe "$1.length"
    length :: PointArray n x -> Int

{-# INLINE index #-}
foreign import javascript unsafe "$2[$1]"
    index :: Int -> PointArray n x -> Vector n x


{-# INLINE js_fromList #-}
foreign import javascript unsafe "h$listToArray($1)"
    js_fromList :: Any -> PointArray n x

{-# INLINE js_toList #-}
foreign import javascript unsafe "h$toHsListJSVal($1)"
    js_toList:: PointArray n x -> Any

seqList :: [a] -> [a]
seqList xs = foldr seq () xs `seq` xs


-- | convert an array to the point set
{-# INLINE unflatten #-}
unflatten :: (KnownNat n) => JSVal -> PointArray n x
unflatten val = arr
    where arr = js_unflatten (dim' arr Proxy) val

{-# INLINE dim' #-}
dim' :: (KnownNat n) => PointArray n x -> Proxy n -> Int
dim' _ = fromInteger . natVal


class PointSet s n x | s -> n, s -> x where
    -- | make a flat JSArrayout of the point set
    flatten :: s -> JSVal
    -- | make a JSArray of coordinates out of the point set
    toPointArray :: s -> PointArray (n::Nat) x
    -- | convert an array to the point set
    fromPointArray :: PointArray (n::Nat) x -> s
    -- | mean of the set
    mean :: s -> Vector n x
    -- | variance of the set
    var :: s -> Matrix n x


instance PointSet (PointArray n x) n x where
    {-# INLINE flatten #-}
    flatten = js_flatten
    {-# INLINE toPointArray #-}
    toPointArray = id
    {-# INLINE fromPointArray #-}
    fromPointArray = id
    {-# INLINE mean #-}
    mean = js_mean
    {-# INLINE var #-}
    var = js_var


{-# INLINE js_flatten #-}
foreign import javascript unsafe "[].concat.apply([], $1)"
    js_flatten :: PointArray n x -> JSVal


{-# INLINE js_unflatten #-}
foreign import javascript unsafe "$r = []; for (var i=0; i < $2.length; i+=$1){$r.push($2.slice(i,i+chunkSize));}"
    js_unflatten :: Int -> JSVal -> PointArray n x

{-# INLINE js_mean #-}
foreign import javascript unsafe "gm$mean($1)"
    js_mean :: PointArray n x -> Vector n x

{-# INLINE js_var #-}
foreign import javascript unsafe "gm$cov($1)"
    js_var :: PointArray n x -> Matrix n x




-- | Calculate principal components in order of reducing eigenvalues
pcaVectors :: (KnownNat n, PointSet s n x) => s -> [Vector n x]
pcaVectors s = unsafeCoerce $ js_eigs m (dim m)
    where m = var s
{-# INLINE js_eigs #-}
foreign import javascript unsafe "h$toHsListJSVal(gm$principalEigenvectors($1,$2))"
    js_eigs :: Matrix n x -> Int -> Any

-- | Get 1D javaScript array by projecting set on vector
project1D :: (KnownNat n, PointSet s n x, Fractional x, JSNum x) => Vector n x -> s -> JSVal
project1D v = js_project1D (v / (v .*. v) ) . toPointArray

{-# INLINE js_project1D #-}
foreign import javascript unsafe "$2.map(function (v){ return dotJSVec($1,v); })"
    js_project1D :: Vector n x -> PointArray n x -> JSVal


-- | Get ND javaScript array by projecting set on vector
projectND :: (KnownNat n, KnownNat m, PointSet s m x, Fractional x, JSNum x)
          => [Vector m x] -> s -> PointArray n x
projectND vs s = rez
    where rez = js_projectND (unsafeCoerce . seqList $ map (\v -> v / (v .*. v)) vs ) (dim' rez Proxy) (toPointArray s)

{-# INLINE js_projectND #-}
foreign import javascript unsafe "var vs = h$listToArray($1).slice(0,$2); $r = $3.map(function (v){ return vs.map( function(e){ return dotJSVec(v,e); }); });"
    js_projectND :: Any -> Int -> PointArray m x -> PointArray n x












