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
    , bestFittingPlaneN, bestFittingLineN
    , varX, meanX
    , ObjectCentres (..)
    , getScenarioStatistics
    , setNormalsAndComputeIndices
    ) where


import Control.Monad (zipWithM, foldM)
import Data.Word
--import Data.List (fromListN)
import Numeric.DataFrame
import Numeric.DataFrame.IO
import Numeric.Dimensions
import Numeric.Dimensions.Traverse.IO
import Numeric.TypeLits
import Commons
import JavaScript.JSON.Types.Internal
import JavaScript.JSON.Types.Instances
import JavaScript.Array (JSArray)
import qualified JavaScript.Array as JSArray
import Unsafe.Coerce
import Model.Scenario.Statistics
import Model.Scenario.Object.Geometry

-- | Flag telling if fromJSON parsing function appened zeros as third coordinates
--   to all vertices of a Geometry.
newtype PaddedZeros = PaddedZeros Bool
  deriving (Eq, Show)

----------------------------------------------------------------------------------------------------
-- * Getting normals and indices
----------------------------------------------------------------------------------------------------


setNormalsAndComputeIndices :: Geometry
                            -> IO (Maybe (SomeIODataFrame Word16 '[XN 0]))
setNormalsAndComputeIndices (Points _) = pure Nothing
setNormalsAndComputeIndices (Lines xs) = pure $ case fromList $ map scalar iss of
      SomeDataFrame (df :: DataFrame Word16 ns) ->
        case unsafeCoerce (Evidence :: Evidence (ns ~ ns)) of
          (Evidence :: Evidence ('[n] ~ ns)) ->
            Just $ SomeIODataFrame
              (unsafeCoerce df :: IODataFrame Word16 '[n])
    where
      (_, iss) = foldl f (0,[]) xs
      f :: (Word16, [Word16]) -> SomeIODataFrame Float '[N 4,XN 2] -> (Word16, [Word16])
      f (n0,is) (SomeIODataFrame (_ :: IODataFrame Float ns))
        | (Evidence :: Evidence ('[4,n] ~ ns, 2 <= n)) <- unsafeCoerce (Evidence :: Evidence ())
        , n <- fromIntegral $ dimVal' @n
        = (n + n0, is ++ ([0..n-2] >>= \i -> [n0+i,n0+i+1]) )
setNormalsAndComputeIndices (Polygons ns) = do
    l0 <- js_emptyList
    l1 <- foldM (\l p -> triangulateAndSetNormal p >>= js_concat l) l0 ns
    (n, df) <- js_wratIds l1
    return $ case someIntNatVal n of
      Nothing -> Nothing
      Just (SomeIntNat (_::Proxy n)) -> Just $ SomeIODataFrame
              (unsafeCoerce df :: IODataFrame Word16 '[n])


triangulateAndSetNormal :: (SomeIODataFrame Float '[N 4, N 2, XN 3], [Int])
                        -> IO JSVal
triangulateAndSetNormal (SomeIODataFrame (sdf :: IODataFrame Float ns), holes)
    = case unsafeCoerce (Evidence :: Evidence (ns ~ ns, 2 <= 3)) of
        (Evidence :: Evidence ('[4,2,n] ~ ns, 2 <= n)) -> do
          df <- unsafeFreezeDataFrame sdf
          let onlyPoints = ewmap @_ @'[4] @'[n] (1!.) df
              n = bestFittingPlaneN onlyPoints
              n' = n <+:> 0
              projected = project2D onlyPoints n
          jprojected <- toJSVal projected
          jholes <- toJSVal holes
          overDimIdx_ (dim @'[n]) $ \i -> copyDataFrame n' (1:!2:!i) sdf
          js_earcut (unsafeCoerce sdf) jprojected jholes >>= fromJSValUnchecked

foreign import javascript unsafe
    "var off = Math.floor($1.byteOffset / 32); $r = (earcut($2, $3)).map(function(e){return off + e;});"
    js_earcut :: JSVal -> JSVal -> JSVal -> IO JSVal

foreign import javascript unsafe
    "$1.concat($2)"
    js_concat :: JSVal -> JSVal -> IO JSVal

foreign import javascript unsafe
    "$r = [];"
    js_emptyList :: IO JSVal

foreign import javascript unsafe
    "$r1 = $1.length; $r2 = new Uint16Array($1);"
    js_wratIds :: JSVal -> IO (Int, JSVal)


project2D :: forall n . (KnownDim n, 2 <= n)
          => DataFrame Float '[4, n]
          -> Vec3f
          -> DataFrame Float '[2, n]
project2D df' norm = ewmap proj df
    where
      m  = meanX df'
      proj x = vec2 (unScalar $ dot x nx) (unScalar $ dot x ny) / fromScalar (4 !. x)
      nx' = let x' = norm `cross` vec3 1 0 0
                x'' = if dot x' x' < 0.01
                      then norm `cross` vec3 0 1 0
                      else x'
            in x'' / (fromScalar (normL2 x'') <+:> 0)
      nx = nx' <+:> 0
      ny = let y' = norm `cross` nx'
           in y' / fromScalar (normL2 y') <+:> 0
      df = ewmap (flip (-) m) df'


-- https://math.stackexchange.com/q/2306029
bestFittingPlaneN :: forall n . (KnownDim n, 2 <= n) => DataFrame Float '[4, n] -> Vec3f
bestFittingPlaneN df'
    | -- take mean
      m  <- meanX df'
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


----------------------------------------------------------------------------------------------------
-- * Converting from JSON
----------------------------------------------------------------------------------------------------


instance FromJSON (Geometry, PaddedZeros) where
    parseJSON v = flip (withObject "GeoJSON Geometry object") v $ \obj -> do
      gType <- obj .: "type"
      obj .: "coordinates" >>= \a -> flip (withArray "GeoJSON Geometry coordinates") a $ \arr ->
        case (js_arrayNestingLvl arr, gType) of

          (1, "Point") ->
              pure $ (Points . SomeIODataFrame) *** PaddedZeros
                   $ js_parsePoint arr

          (2, "MultiPoint") ->
            case js_parsePointSeq arr of
              (df, n, padded) -> flip (,) (PaddedZeros padded) . Points <$> converDF df n

          (2, "LineString") ->
            case js_parsePointSeq arr of
              (df, n, padded) -> flip (,) (PaddedZeros padded) . Lines . (:|[]) <$> converDF df n

          (3, "MultiLineString") ->
            case js_parseMultiLineString arr of
              (dfs', ns', padded) -> do
                dfs <- parseJSON dfs'
                ns  <- parseJSON ns'
                rs  <- zipWithM converDF dfs ns
                case rs of
                  [] -> fail "MultiLineString seems to be empty"
                  (x:xs) -> pure ( Lines $ x :| xs
                                 , PaddedZeros padded
                                 )

          (3, "Polygon") ->
            case js_parsePolygon arr of
              (df, n, holes', padded) -> do
                 poly <- converDF df n
                 holes <- parseJSON $ arrayValue holes'
                 return ( Polygons ( (poly, holes) :| [])
                        , PaddedZeros padded
                        )

          (4, "MultiPolygon") ->
            case js_parseMultiPolygon arr of
              (dfs', ns', holes', padded) -> do
                dfs <- parseJSON dfs'
                ns  <- parseJSON ns'
                holes  <- parseJSON holes'
                rs  <- zipWithM converDF dfs ns
                case zip rs holes of
                  [] -> fail "MultiLineString seems to be empty"
                  (x:xs) -> pure ( Polygons $ x :| xs
                                 , PaddedZeros padded
                                 )

          (lvl, s) -> fail $
              "Wrong geometry type (" <> s <> ") or array nesting level (" <> show lvl <> ")."


converDF :: forall ns k
          . ( Dimensions ns, ArraySizeInference ns)
         => JSVal -> Int -> Parser (SomeIODataFrame Float (AsXDims ns +: XN k))
converDF jsv n = case someIntNatVal n of
  Nothing -> fail "Could not read dataframe length"
  Just (SomeIntNat (_::Proxy n)) ->
    case  ( unsafeCoerce (Evidence :: Evidence ()) :: Evidence
               (FixedDim (AsXDims ns +: XN k) (ns +: n) ~ (ns +: n)) )
      +!+ ( unsafeCoerce (Evidence :: Evidence ()) :: Evidence
               (FixedXDim (AsXDims ns +: XN k) (ns +: n) ~ (AsXDims ns +: XN k)) )
      +!+ inferSnocDimensions @ns @n
      +!+ inferSnocArrayInstance (undefined :: IODataFrame Float ns) (Proxy @n)
         of
      Evidence -> case inferNumericFrame @Float @(ns +: n) of
        Evidence -> pure $ SomeIODataFrame (coerce jsv :: IODataFrame Float (ns +: n))


foreign import javascript unsafe
    "h$geojson_nestingLvl($1)"
    js_arrayNestingLvl :: JSArray -> Int

foreign import javascript unsafe
    "var a = h$geojson_parseVec4($1); $r1 = a[0]; $r2 = a[1];"
    js_parsePoint :: JSArray -> (IODataFrame Float '[4,1], Bool)

foreign import javascript unsafe
    "var a = h$geojson_parsePointSeq($1); $r1 = new Float32Array(a[0]); $r2 = a[1]; $r3 = a[2];"
    js_parsePointSeq :: JSArray -> (JSVal, Int, Bool)

foreign import javascript unsafe
    "var a = h$geojson_parseMultiLineString($1); $r1 = a[0]; $r2 = a[1]; $r3 = a[2];"
    js_parseMultiLineString :: JSArray -> (Value, Value, Bool)

foreign import javascript unsafe
    "var a = h$geojson_parsePolygon($1);$r1 = new Float32Array(a[0]);$r2 = a[1];$r3 = a[2];$r4 = a[3];"
    js_parsePolygon :: JSArray -> (JSVal, Int, JSArray, Bool)

foreign import javascript unsafe
    "var a = h$geojson_parseMultiPolygon($1); $r1 = a[0];$r2 = a[1];$r3 = a[2];$r4 = a[3];"
    js_parseMultiPolygon :: JSArray -> (Value, Value, Value, Bool)


----------------------------------------------------------------------------------------------------
-- * Converting to JSON
----------------------------------------------------------------------------------------------------



instance ToJSON Geometry where
    toJSON (Points (SomeIODataFrame (sdf :: IODataFrame Float ns)))
        | (Evidence :: Evidence ([4,n] ~ ns, 1 <= n)) <- unsafeCoerce (Evidence :: Evidence (ns ~ ns, 1 <= 1))
        , n <- dimVal' @n
        = if n == 1
          then objectValue $ object
            [ ("type", toJSON ("Point" :: JSString))
            , ("coordinates", js_vecToJSArray 3 $ unsafeCoerce sdf )
            ]
          else objectValue $ object
            [ ("type", toJSON ("MultiPoint" :: JSString))
            , ("coordinates", js_vecToJSArray2Stride 4 3 $ unsafeCoerce sdf )
            ]
    toJSON (Lines (SomeIODataFrame sdf :| []))
        = objectValue $ object
            [ ("type", toJSON ("LineString" :: JSString))
            , ("coordinates", js_vecToJSArray2Stride 4 3 $ unsafeCoerce sdf )
            ]
    toJSON (Lines (x:|xs))
        = objectValue $ object
            [ ("type", toJSON ("MultiLineString" :: JSString))
            , ("coordinates", toJSON $ map f (x:xs) )
            ]
      where
        f :: SomeIODataFrame Float '[N 4, XN 2] -> Value
        f (SomeIODataFrame sdf) = js_vecToJSArray2Stride 4 3 (unsafeCoerce sdf)
    toJSON (Polygons ((SomeIODataFrame sdf, holes) :| []))
        = objectValue $ object
            [ ("type", toJSON ("Polygon" :: JSString))
            , ("coordinates", js_vecToJSArray3StrideNRings 8 3
                                   (toJSON (0: map (8*) holes)) (unsafeCoerce sdf))
            ]
    toJSON (Polygons (x:|xs))
        = objectValue $ object
            [ ("type", toJSON ("MultiPolygon" :: JSString))
            , ("coordinates", toJSON $ map f (x:xs))
            ]
      where
        f :: (SomeIODataFrame Float '[N 4, N 2, XN 3], [Int]) -> Value
        f (SomeIODataFrame sdf, holes)
           = js_vecToJSArray3StrideNRings 8 3 (toJSON (0: map (8*) holes)) (unsafeCoerce sdf)



foreign import javascript unsafe "Array.prototype.slice.call($2, 0, $1)"
    js_vecToJSArray :: Int -> JSVal -> Value

foreign import javascript unsafe
    "var a = [];\
    \ for(var i = 0; i < $3.length; i+=$1){\
    \   a.push(Array.prototype.slice.call($3, i, i + $2));\
    \ } $r = a;"
    js_vecToJSArray2Stride :: Int -> Int -> JSVal -> Value

foreign import javascript unsafe
    "var a = [], r, is = $3.concat([$4.length]);\
    \ for(var i = 0; i < is.length - 1; i++){\
    \   r = [];\
    \   for(var j = is[i]; j < is[i+1]; i+=$1){\
    \     r.push(Array.prototype.slice.call($3, j, j + $2));\
    \   }\
    \   r.push(r[0]);\
    \   a.push(r);\
    \ } $r = a;"
    js_vecToJSArray3StrideNRings :: Int -> Int -> Value -> JSVal -> Value








----------------------------------------------------------------------------------------------------
-- * Gathering object statistics
----------------------------------------------------------------------------------------------------

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
    "h$geojson_getObjectCentres($1)"
    js_getObjectCentres :: JSVal -> JSArray

foreign import javascript unsafe
    "new Float32Array([].concat.apply([], $1))"
    js_wrapFloat32ArrayVec :: JSArray -> JSVal


