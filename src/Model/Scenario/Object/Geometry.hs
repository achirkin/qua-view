{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
module Model.Scenario.Object.Geometry
    ( Geometry (..), getTransferable, allData
    , applyTransform, applyGeomCoords
    ) where


import JavaScript.Object
import GHCJS.Types (jsval)
import Unsafe.Coerce (unsafeCoerce)
import Numeric.DataFrame
import Numeric.DataFrame.IO
import Numeric.Dimensions
import Numeric.Dimensions.Traverse.IO
import Numeric.TypeLits
import Commons.NoReflex



-- | All geometry types possible.
--
--   For performance considerations we preserve two properties:
--
--    * All dataframes in a single geometry share a single Transferable ArrayBuffer.
--    * Underlying ArrayBuffer has exactly the same size as all geometry frames together
--    * We never replace mutable dataframes inside a geometry - only update them.
--
--   These properties simplify zero-copy vertex data transfer, make sure matrix transforms are fast,
--   and let us share the geometry with rendering engine as-is.
data Geometry
  = Points   !(SomeIODataFrame Float '[N 4, XN 1])
    -- ^ One or more points in homogenous coordinates
  | Lines    !(NonEmpty (SomeIODataFrame Float '[N 4, XN 2]))
    -- ^ One or more lines. Each line is a set of at least two points in homogenous coordinates.
  | Polygons !(NonEmpty (SomeIODataFrame Float '[N 4, N 2, XN 3], [Int]))
    -- ^ One or more polygons.
    --   Each polygon is a set of points and normals in homogenous coordinates,
    --    together with indices of holes.

getTransferable :: Geometry -> IO Transferable
getTransferable (Points    (SomeIODataFrame sd))          = dataFrameToTransferable sd
getTransferable (Lines     (SomeIODataFrame sd :| _))     = dataFrameToTransferable sd
getTransferable (Polygons ((SomeIODataFrame sd, _) :| _)) = dataFrameToTransferable sd

-- | apply some transformation function on all points
applyGeomCoords :: (KnownDim k, k <= 4)
                => Geometry
                -> (Vector Float k -> Vector Float k) -> IO ()
applyGeomCoords (Points (SomeIODataFrame (iodf :: IODataFrame Float ns))) f
    | (Evidence :: Evidence ('[4,n] ~ ns)) <- unsafeCoerce (Evidence @(ns ~ ns))
    = overDimIdx_ dim $ \i -> let j = 1 :! i in do
        x <- unsafeSubArrayFreeze iodf j
        copyDataFrame (f x) j iodf
applyGeomCoords (Lines lns) f = forM_ @_ @_ @_ @() lns $
    \(SomeIODataFrame (iodf :: IODataFrame Float ns)) ->
      case unsafeCoerce (Evidence @(ns ~ ns)) of
        (Evidence :: Evidence ('[4,n] ~ ns)) ->
          overDimIdx_ dim $ \i -> let j = 1 :! i in do
            x <- unsafeSubArrayFreeze iodf j
            copyDataFrame (f x) j iodf
applyGeomCoords (Polygons pls) f = forM_ @_ @_ @_ @() pls $
    \(SomeIODataFrame (iodf :: IODataFrame Float ns), _) ->
      case unsafeCoerce (Evidence @(ns ~ ns)) of
        (Evidence :: Evidence ('[4,2,n] ~ ns)) ->
          overDimIdx_ dim $ \i -> let j = 1 :! 1 :! i in do
            x <- unsafeSubArrayFreeze iodf j
            copyDataFrame (f x) j iodf


instance ToJSVal Geometry where
    toJSVal (Points sdf) = do
        o <- create
        unsafeSetProp "type" (pToJSVal ("points" :: JSString)) o
        toJSVal sdf >>= \j -> unsafeSetProp "data" j o
        return $ jsval o
    toJSVal (Lines (ln :| lns)) = do
        o <- create
        unsafeSetProp "type" (pToJSVal ("lines" :: JSString)) o
        toJSVal ln  >>= \j -> unsafeSetProp "data1" j o
        toJSVal lns >>= \j -> unsafeSetProp "datas" j o
        return $ jsval o
    toJSVal (Polygons (pn :| pns)) = do
        o <- create
        unsafeSetProp "type" (pToJSVal ("polygons" :: JSString)) o
        toJSVal pn  >>= \j -> unsafeSetProp "data1" j o
        toJSVal pns >>= \j -> unsafeSetProp "datas" j o
        return $ jsval o

instance FromJSVal Geometry where
    fromJSVal jsv = fromObj (unsafeCoerce jsv)

fromObj :: JavaScript.Object.Object -> IO (Maybe Geometry)
fromObj o = unsafeGetProp "type" o >>= fromJSVal >>= \mt -> case mt :: Maybe JSString of
    Just "points" -> fmap (fmap Points) $ unsafeGetProp "data" o >>= fromJSVal
    Just "lines"  -> do
      sdf  <- unsafeGetProp "data1" o >>= fromJSVal
      sdfs <- unsafeGetProp "datas" o >>= fromJSVal
      return . fmap Lines $ (:|) <$> sdf <*> sdfs
    Just "polygons"  -> do
      sdf  <- unsafeGetProp "data1" o >>= fromJSVal
      sdfs <- unsafeGetProp "datas" o >>= fromJSVal
      return . fmap Polygons $ (:|) <$> sdf <*> sdfs
    _ -> pure Nothing


-- | Multiply geometry vectors with 4D hom matrix on the left in a fast way.
applyTransform :: Geometry -> Mat44f -> IO ()
applyTransform !g !m = allData g >>= \(SomeIODataFrame !df) -> applyTransformDF m df df




-- | Get all vertices and normals from a geometry as a single DataFrame of 4D vectors.
--   This is constant-time operation with no data transfer or copying.
--   It works only if all components in Geometry share a single ArrayBuffer however.
allData :: Geometry -> IO (SomeIODataFrame Float '[N 4, XN 1])
allData g = getTransferable g >>= js_allVertices >>= \(arr, n) ->
    case someIntNatVal n of
      Just (SomeIntNat (_::Proxy n)) -> return $
        SomeIODataFrame (coerce arr :: IODataFrame Float '[4,n])
      Nothing -> error "Impossible happend in allVertices: could not get length as Dim."

foreign import javascript unsafe
    "$r1 = new Float32Array($1); $r2 = Math.floor($r1.length / 4);"
    js_allVertices :: Transferable -> IO (JSVal, Int)
