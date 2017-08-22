{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Coordinate representation
--
--   I use @Float32Array@s for representing coordinates.
--
--   All points and normals are represented as homogenious 4D vectors.
--
--   If 2D coordinates are given, they are padded with zeroes
--     (`PaddedZeros` flag is added during parsing if at least one vector was padded).
--
--   All normals are inferred from coordinates.
--
--   PaddedZeros flag can be used to infer if I should try to extrude 2D objects into 3D
--     (by inspecting @height@ feature property or using default extrusion height).
--
module Model.GeoJSON.Coordinates
    ( PaddedZeros (..)
    , PointSeq (..), LinearRing (..), LinearRingWithNormals (..)
    , bestFittingPlaneN, bestFittingLineN
    , varX, meanX
    , ObjectCentres (..)
    , getScenarioStatistics
    ) where

import Numeric.DataFrame
import Numeric.Dimensions
import Numeric.TypeLits
import Commons
import JavaScript.JSON.Types.Internal
import JavaScript.JSON.Types.Instances
import JavaScript.Array (JSArray)
import qualified JavaScript.Array as JSArray
import Unsafe.Coerce
import Model.Scenario.Statistics

newtype PaddedZeros = PaddedZeros Bool
  deriving (Eq, Show)

-- | 4xB where N >= 2
newtype PointSeq = PointSeq (DataFrame Float '[N 4, XN 2])
  deriving Eq

-- | 4xN where N >= 3 (last point removed)
newtype LinearRing = LinearRing (DataFrame Float '[N 4, XN 3])
  deriving Eq

-- | 4x2xN where N >= 3 (last point removed)
newtype LinearRingWithNormals = LinearRingWithNormals (DataFrame Float '[N 4, N 2, XN 3])
  deriving Eq

-- https://math.stackexchange.com/q/2306029
bestFittingPlaneN :: DataFrame Float '[N 4, XN 3] -> Vec3f
bestFittingPlaneN (SomeDataFrame (df' :: DataFrame Float ns))
    | (Evidence :: Evidence ([4,n] ~ ns, 2 <= n)) <- unsafeCoerce (Evidence :: Evidence ())
      -- take mean
    , m  <- meanX df'
      -- normalized frame
    , df <- ewmap   @_ @'[4] (flip (-) m) df'
      -- solve Ax = B
    , b  <- ewmap @Float @'[] (3:!Z !. ) df :: DataFrame Float '[n]
    , a  <- ewmap @Float @'[3] @'[n] @'[3,n]
                  @Float @'[4]       @'[4,n]
                  (\v -> vec3 (unScalar $ 1:!Z !. v) (unScalar $ 2:!Z !. v) 1 ) df
    , aT <- transpose a :: DataFrame Float '[n, 3]
    , aX <- inverse $ a %* aT :: DataFrame Float '[3, 3]
    , r3 <- aX %* a %* b
    , df2d <- ewmap (\v -> vec2 (unScalar $ 1:!Z !. v) (unScalar $ 2:!Z !. v) ) df
    = if abs (det aX) > 0.001
      then let v = vec3 (unScalar $ 1 !. r3) (unScalar $ 2 !. r3) 1 in v / fromScalar (normL2 v)
      else bestFittingLineN df2d <+:> 0

-- Here we assume it is already normalized
bestFittingLineN :: forall n . (KnownDim n, 2 <= n) => DataFrame Float '[2, n] -> Vec2f
bestFittingLineN df
    | -- solve Ax = B
      b  <- ewmap @Float @'[]  @'[n] @'[n]    (2:!Z !. ) df
    , a  <- ewmap @Float @'[2] @'[n] @'[2,n]
                  @Float @'[2]       @'[2,n]
                  (\v -> vec2 (unScalar $ 1:!Z !. v) 1 ) df
    , aT <- transpose a :: DataFrame Float '[n, 2]
    , aX <- inverse $ a %* aT :: DataFrame Float '[2, 2]
    , r2  <- aX %* a %* b
    , (vx, vy) <- unpackV2 $ varX df
    = if vx < 0.001 || vx / vy < 0.00001
      then vec2 0 1
      else if vy < 0.001 || vy / vx < 0.00001
           then vec2 1 0
           else let v = vec2 (unScalar $ 1 !. r2) 1 in v / fromScalar (normL2 v)

meanX :: forall n m . (KnownDim n, KnownDim m) => DataFrame Float '[n, m] -> Vector Float n
meanX x = ewfoldl (+) 0 x / fromIntegral (dimVal' @m)

varX :: forall n m . (KnownDim n, KnownDim m) => DataFrame Float '[n, m] -> Vector Float n
varX x = ewfoldl (\a v -> let v' = (v - m) in a + v' * v' ) 0 x / fromIntegral (dimVal' @m - 1)
  where
    m = meanX x



instance FromJSON (Vec3f, PaddedZeros) where
    parseJSON val = flip (withArray "Vec3f") val $ \coords ->
       pure $ unsafeCoerce *** PaddedZeros $ js_parseVec3f coords

instance FromJSON (Vec4f, PaddedZeros) where
    parseJSON val = flip (withArray "Vec4f") val $ \coords ->
       pure $ unsafeCoerce *** PaddedZeros $ js_parseVec4f coords

instance FromJSON (PointSeq, PaddedZeros) where
    parseJSON val = flip (withArray "Point sequence") val $ \coords ->
       let lvl = js_arrayNestingLvl coords
       in if lvl /= 2
          then fail $ "Array nesting level must be 2, but encountered " ++ show lvl ++ "."
          else let (jsv, len, pz) = js_parsePointSeq coords
               in if len >= 2
                  then case someIntNatVal len of
                    Nothing -> fail $
                       "Could not set DataFrame length: " ++ show len ++ "."
                    Just (SomeIntNat (_ :: Proxy n)) ->
                       pure (PointSeq $ SomeDataFrame (unsafeCoerce jsv :: DataFrame Float '[4,n])
                                                      , PaddedZeros pz)
                  else fail $
                    "Length of a point array must be greater than 1, but the array length is "
                      ++ show len ++ "."

instance FromJSON (LinearRing, PaddedZeros) where
    parseJSON val = flip (withArray "Linear ring") val $ \coords ->
       let lvl = js_arrayNestingLvl coords
       in if lvl /= 2
          then fail $ "Array nesting level must be 2, but encountered " ++ show lvl ++ "."
          else let (jsv, len, pz) = js_parseLinearRing coords
               in if len >= 4
                  then case someIntNatVal (len-1) of
                    Nothing -> fail $
                       "Could not set DataFrame length: " ++ show len ++ "."
                    Just (SomeIntNat (_ :: Proxy n)) ->
                       pure (LinearRing $ SomeDataFrame (unsafeCoerce jsv :: DataFrame Float '[4,n])
                                                        , PaddedZeros pz)
                  else fail $
                    "Length of a point array must be greater than 3, but the array length is "
                      ++ show len ++ "."


-- | Go over a 2D point set and derive neccessary statistics
getScenarioStatistics :: ObjectCentres -> ScenarioStatistics
getScenarioStatistics (ObjectCentres (SomeDataFrame centres))
    = ewfoldMap (\v -> ScenarioStatistics v v 1 v) centres

-- | Try hard to parse JSON containing feature collection or geometry
--   and return collection of object centres in 2D
newtype ObjectCentres = ObjectCentres (DataFrame Float '[N 2, XN 0])

instance FromJSON ObjectCentres where
    parseJSON (SomeValue jsv) = case someIntNatVal n of
        Nothing -> fail $ "Could not set DataFrame length: " ++ show n ++ "."
        Just (SomeIntNat (_ :: Proxy n)) ->
          pure (ObjectCentres $ SomeDataFrame
                     (unsafeCoerce (js_wrapFloat32ArrayVec xs) :: DataFrame Float '[2,n])
               )
      where
        xs = js_getObjectCentres jsv
        n  = JSArray.length xs


foreign import javascript unsafe
    "var a = h$geojson_parseVec3($1); $r1 = a[0]; $r2 = a[1];"
    js_parseVec3f :: JSArray -> (JSVal, Bool)

foreign import javascript unsafe
    "var a = h$geojson_parseVec4($1); $r1 = a[0]; $r2 = a[1];"
    js_parseVec4f :: JSArray -> (JSVal, Bool)

foreign import javascript unsafe
    "var a = h$geojson_parsePointSeq($1); $r1 = a[0]; $r2 = a[1]; $r3 = a[2];"
    js_parsePointSeq :: JSArray -> (JSVal, Int, Bool)

foreign import javascript unsafe
    "var a = h$geojson_parseLinearRing($1); $r1 = a[0]; $r2 = a[1]; $r3 = a[2];"
    js_parseLinearRing :: JSArray -> (JSVal, Int, Bool)

foreign import javascript unsafe
    "h$geojson_nestingLvl($1)"
    js_arrayNestingLvl :: JSArray -> Int

foreign import javascript unsafe
    "h$geojson_getObjectCentres($1)"
    js_getObjectCentres :: JSVal -> JSArray

foreign import javascript unsafe
    "new Float32Array([].concat.apply([], $1))"
    js_wrapFloat32ArrayVec :: JSArray -> JSVal



