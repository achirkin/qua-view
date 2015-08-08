{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Model.GeoJSON
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.Model.GeoJSON where

import Data.Primitive
import qualified Data.IntMap.Strict as IM

import Control.Monad ((>=>), liftM, join)
import Control.Arrow (first, second)

import GHCJS.WebGL

import Geometry.Space
import Geometry.Space.Transform
import Geometry.Math
import qualified Geometry.Structure as S
import Program.Model.City
import Program.Model.CityObject
import Program.Model.WiredGeometry

import Data.Geospatial
import Data.LinearRing
import Data.LineString
--import Debug.Trace (traceShow)



approxTolerance :: GLfloat
approxTolerance = 0.1



----------------------------------------------------------------------------------------------------
-- 2D GeoJSON
----------------------------------------------------------------------------------------------------

defaultHeightStatic :: GLfloat
defaultHeightStatic = 2

defaultHeightDynamic :: GLfloat
defaultHeightDynamic = 3

defaultLinesAlt :: GLfloat
defaultLinesAlt = 0.1

defaultAreaSize :: GLfloat
defaultAreaSize = 400

featureCollectionToObjects :: ObjectBehavior
                             -> GLfloat -- ^ Scale
                             -> Vector2 GLfloat -- ^ Shift
                             -> GeoFeatureCollection a
                             -> (([LocatedCityObject], [[Vector3 GLfloat]]), [String])
featureCollectionToObjects b scal shift@(Vector2 shx shz) gfc = organize $ gms >>= f
    where gms = map _geometry $ _geofeatures gfc :: [GeospatialGeometry]
          f NoGeometry = [Left "No geometry"]
          f (Point p) = [Left $ "Point " ++ show p]
          f (MultiPoint mp) = [Left $ "MultiPoint " ++ show mp]
          f (Polygon poly) = [liftM Right $ if isPolygon3d poly
            then polygon3DtoCityObject b scal shift poly
            else polygon2DtoCityObject b scal shift poly]
          f (MultiPolygon mpoly) = map Polygon (splitGeoMultiPolygon mpoly) >>= f
          f (Line l) = [Right . Left $ lineToPoints l]
          f (MultiLine ml) = map Line (splitGeoMultiLine ml) >>= f
          f (Collection xs) = xs >>= f
          organize [] = (([],[]),[]) :: (([LocatedCityObject], [[Vector3 GLfloat]]), [String])
          organize (Left m : xs) = second (m:) $ organize xs
          organize (Right (Left lns) : xs) = first (second (lns:)) $ organize xs
          organize (Right (Right x) : xs) = first (first (x:)) $ organize xs
          shift3 = Vector3 shx 0 shz
          lineToPoints :: GeoLine -> [Vector3 GLfloat]
          lineToPoints (GeoLine lineString) = pnts >>= toVector
            where pnts = fromLineString lineString
                  toVector (x:z:0:_) = toVector [x,z]
                  toVector (x:z:y:_) = [shift3 .+
                    (fmap ((*scal) . realToFrac) $ Vector3 x y (-z))]
                  toVector [x,z] = [shift3 .+
                    Vector3 (scal * realToFrac x) defaultLinesAlt (scal * realToFrac (-z))]
                  toVector [_] = []
                  toVector [] = []

featureCollectionToObjectsAndScale :: ObjectBehavior
                                   -> GeoFeatureCollection a
                                   -> ( ([LocatedCityObject], [[Vector3 GLfloat]])
                                      , [String]
                                      , GLfloat
                                      , Vector2 GLfloat)
featureCollectionToObjectsAndScale b gfc = (r1,r2, scal, shift)
    where bb = S.minBBox gfc :: S.BoundingBox 2 GLfloat
          lb = S.lowBound bb
          hb = S.highBound bb
          diagr = normL2 $ lb .- hb
          center = (lb .+ hb) /.. 2
          scal = defaultAreaSize / diagr
          shift = center*.. (-scal)
          (r1,r2) = featureCollectionToObjects b scal shift gfc

--featureCollectionToObjectsAndScale :: ObjectBehavior
--                                   -> GeoFeatureCollection a
--                                   -> (([LocatedCityObject], [[Vector3 GLfloat]]), [String])
--featureCollectionToObjectsAndScale b scal gfc = organize $ gms >>= f
--    where gms = map _geometry $ _geofeatures gfc :: [GeospatialGeometry]
--          f NoGeometry = [Left "No geometry"]
--          f (Point p) = [Left $ "Point " ++ show p]
--          f (MultiPoint mp) = [Left $ "MultiPoint " ++ show mp]
--          f (Polygon poly) = [liftM Right $ if isPolygon3d poly
--            then polygon3DtoCityObject b scal poly
--            else polygon2DtoCityObject b scal poly]
--          f (MultiPolygon mpoly) = map Polygon (splitGeoMultiPolygon mpoly) >>= f
--          f (Line l) = [Right . Left $ lineToPoints l]
--          f (MultiLine ml) = map Line (splitGeoMultiLine ml) >>= f
--          f (Collection xs) = xs >>= f
--          organize [] = (([],[]),[]) :: (([LocatedCityObject], [[Vector3 GLfloat]]), [String])
--          organize (Left m : xs) = second (m:) $ organize xs
--          organize (Right (Left lns) : xs) = first (second (lns:)) $ organize xs
--          organize (Right (Right x) : xs) = first (first (x:)) $ organize xs
--          lineToPoints :: GeoLine -> [Vector3 GLfloat]
--          lineToPoints (GeoLine lineString) = pnts >>= toVector
--            where pnts = fromLineString lineString
--                  toVector (x:z:0:_) = [Vector3 (realToFrac x) defaultLinesAlt (realToFrac (-z))]
--                  toVector (x:z:y:_) = [Vector3 (realToFrac x) (realToFrac y) (realToFrac (-z))]
--                  toVector (x:z:_) = [Vector3 (realToFrac x) defaultLinesAlt (realToFrac (-z))]

instance S.Boundable (GeoFeatureCollection a) 2 GLfloat where
    minBBox gfc = S.boundSet (map _geometry (_geofeatures gfc) >>= f)
      where f :: GeospatialGeometry -> [S.BoundingBox 2 GLfloat]
            f NoGeometry = []
            f (Point p) = toVec (_unGeoPoint p) >>= \x -> [S.boundingBox x x]
            f (MultiPoint mp) = [S.boundSet $ map _unGeoPoint (splitGeoMultiPoint mp) >>= toVec]
            f (Polygon poly) = [S.boundSet $ _unGeoPolygon poly >>= fromLinearRing >>= toVec]
            f (MultiPolygon mpoly) = map Polygon (splitGeoMultiPolygon mpoly) >>= f
            f (Line l) = [S.boundSet $ (fromLineString $ _unGeoLine l) >>= toVec]
            f (MultiLine ml) = map Line (splitGeoMultiLine ml) >>= f
            f (Collection xs) = xs >>= f
            toVec (x:z:_) = [Vector2 (realToFrac x) (realToFrac (-z))]
            toVec [_] = []
            toVec [] = []

----------------------------------------------------------------------------------------------------
-- 2D GeoJSON
----------------------------------------------------------------------------------------------------


-- | Loses all rings except first one
polygon2DtoCityObject :: ObjectBehavior
                      -> GLfloat -- ^ Scale
                      -> Vector2 GLfloat -- ^ Shift
                      -> GeoPolygon
                      -> Either String LocatedCityObject
polygon2DtoCityObject beh scal shift (GeoPolygon (pts:_)) =
    f . map (shift .+)
      . map (\(x:z:_) -> ((scal*) . realToFrac) <$> Vector2 x (-z)) . fromLinearRing $ pts
    where f :: [Vector 2 GLfloat] -> Either String (STransform "Quaternion" GLfloat CityObject)
          f (_:xs@(p1:p2:_:_)) | Vector2 ax ay <- p2.-p1
                         , (s,si) <- geojsonTransform2d (mean xs) (atan2 (-ay) ax)
                         = Right . s $ bds xs si
          f _ = Left "polygon2DtoCityObject: not enough points!"
          bds xs si = building beh . S.SimplePolygon $
            map (\(Vector2 x z) -> transform . si $ Vector3 x (
                case beh of
                    Static -> defaultHeightStatic
                    Dynamic -> defaultHeightDynamic
                ) z) xs
polygon2DtoCityObject _ _ _ (GeoPolygon []) = Left "polygon2DtoCityObject: No polygons at all!"


geojsonTransform2d :: Vector2 GLfloat
                   -> GLfloat
                   -> ( a -> STransform "Quaternion" GLfloat a
                      , b -> STransform "Quaternion" GLfloat b)
geojsonTransform2d (Vector2 sx sy) angle = (s, flip wrap (inverseTransform $ s ()))
    where s = translate (Vector3 sx 0 sy) >=> rotateY angle



----------------------------------------------------------------------------------------------------
-- 3D GeoJSON
----------------------------------------------------------------------------------------------------

isPolygon3d :: GeoPolygon -> Bool
isPolygon3d (GeoPolygon (pts:_)) = case fromLinearRing pts of
    x:_ -> length x >= 3
    []  -> False
isPolygon3d _ = False

-- | Loses all rings except first one.
--   Removes all vertical walls.
polygon3DtoCityObject :: ObjectBehavior
                      -> GLfloat -- ^ Scale
                      -> Vector2 GLfloat -- ^ Shift
                      -> GeoPolygon
                      -> Either String LocatedCityObject
polygon3DtoCityObject beh scal (Vector2 shx shz) (GeoPolygon (pts:_)) =
    g . map (shift .+)
      . map (\(x:z:y:_) -> ((scal*) . realToFrac) <$> Vector3 x y (-z)) . fromLinearRing $ pts
    where f :: [Vector 3 GLfloat] -> Either String (STransform "Quaternion" GLfloat CityObject)
          f (_:xs@(p1:p2:_:_)) | Vector3 ax _ az <- p2.-p1
                         , (s,si) <- geojsonTransform3d (mean xs) (atan2 (-az) ax)
                         = Right . s $ bds xs si
          f _ = Left "polygon3DtoCityObject: not enough points!"
          isVertical xs = all (\(a,b,c) -> flip runApprox approxTolerance
                               . areOrthogonal (Vector3 0 1 0) $ (b .- a) `cross` (c .- a))
                          . map (\(i,j,k) -> (xs !! i, xs !! j, xs !! k))
                          . S.triangulate3 $ S.SimplePolygon xs
          g xs = if isVertical xs then Left "Ignoring wall"
                                  else f xs
          bds xs si = building beh . S.SimplePolygon $
            map (transform . si) xs
          shift = Vector3 shx 0 shz
polygon3DtoCityObject _ _ _ (GeoPolygon []) = Left "polygon3DtoCityObject: No polygons at all!"




geojsonTransform3d :: Vector3 GLfloat
                   -> GLfloat
                   -> ( a -> STransform "Quaternion" GLfloat a
                      , b -> STransform "Quaternion" GLfloat b)
geojsonTransform3d shift angle = (s, flip wrap (inverseTransform $ s ()))
    where s = translate shift >=> rotateY angle


----------------------------------------------------------------------------------------------------
-- Convert everything back to GeoJSON
----------------------------------------------------------------------------------------------------

geometries2features :: GeospatialGeometry -> GeoFeatureCollection ()
geometries2features (Collection geoms) = GeoFeatureCollection Nothing (geoms >>= f)
    where f (Collection gs) = gs >>= f
          f g = [GeoFeature Nothing g () Nothing]
geometries2features g = GeoFeatureCollection Nothing [GeoFeature Nothing g () Nothing]

-- | Gives roofs of buildings in 2D
cityGeometryRoofs :: City -> GeospatialGeometry
cityGeometryRoofs city = Collection polygons
    where polygons = case mapM cityObjectToPolygon2DRoof . IM.elems $ objectsIn city of
            Nothing -> []
            Just xs -> map Polygon xs

-- | Gives full geometry "as is" in 3D (both, lines and polygons)
cityGeometryFull3D :: City -> GeospatialGeometry
cityGeometryFull3D city = Collection $ lnes ++ polygons
    where Collection lnes = wiredGeometryToLines $ clutter city
          polygons = case mapM cityObjectToPolygon3D . IM.elems $ objectsIn city of
            Nothing -> []
            Just xs -> map MultiPolygon xs

wiredGeometryToLines :: WiredGeometry -> GeospatialGeometry
wiredGeometryToLines (WiredGeometry _ n' byteArray) = Collection $ parse 0
    where n = fromIntegral n'
          parse i | i >= n = []
                  | otherwise = case parseLine [] i of
            (Nothing, j) -> parse j
            (Just gl, j) -> Line gl : parse j
          parseLine :: [Vector3 GLfloat] -> Int -> (Maybe GeoLine, Int)
          parseLine [] i | i >= n = (Nothing, i+1)
                         | otherwise = parseLine [indexByteArray byteArray i] (i+1)
          parseLine [x] i | i >= n = (Nothing, i+1)
                          | otherwise = parseLine [indexByteArray byteArray i, x] (i+1)
          parseLine (x:y:xs) i | i >= n = (Just . GeoLine $ makeLineString (f x) (f y) (map f xs), i+1)
                               | otherwise = let cur = indexByteArray byteArray i
            in if x /= cur || i == n - 1
               then (Just . GeoLine $ makeLineString (f x) (f y) (map f xs), i+1)
               else parseLine (indexByteArray byteArray (i+1):x:y:xs) (i+2)
          f (Vector3 x y z) = [realToFrac x, realToFrac (-z), realToFrac y]


cityObjectToPolygon2DRoof :: LocatedCityObject -> Maybe GeoPolygon
cityObjectToPolygon2DRoof lobj = polygon2geo f . transform $ liftM objPolygon lobj
    where f (Vector3 x _ z) = [realToFrac x, realToFrac (-z)]


cityObjectToPolygon3D :: LocatedCityObject -> Maybe GeoMultiPolygon
cityObjectToPolygon3D lobj = liftM mergeGeoPolygons
                             . mapM (polygon2geo f)
                             . makeWalls
                             . transform
                             $ liftM objPolygon lobj
    where f (Vector3 x y z) = [realToFrac x, realToFrac (-z), realToFrac y]
          makeWalls :: S.Polygon 3 GLfloat -> [S.Polygon 3 GLfloat]
          makeWalls poly = (poly:) . map mkwall $ g poly
          g :: S.Polygon 3 GLfloat -> [(Vector 3 GLfloat, Vector 3 GLfloat)]
          g (S.SimpleConvexPolygon []) = []
          g (S.SimpleConvexPolygon xs) = zip xs (last xs : xs)
          g (S.SimplePolygon []) = []
          g (S.SimplePolygon xs) = zip xs (last xs : xs)
          g (S.GenericPolygon xxs) = xxs >>= g
          mkwall :: (Vector 3 GLfloat, Vector 3 GLfloat) -> S.Polygon 3 GLfloat
          mkwall (Vector3 x1 y1 z1, Vector3 x2 y2 z2) = S.SimpleConvexPolygon
            [ Vector3 x1 y1 z1, Vector3 x2 y2 z2
            , Vector3 x2 0  z2, Vector3 x1 0  z1 ]

list2ring :: [a] -> Maybe (LinearRing a)
list2ring [] = Nothing
list2ring [_] = Nothing
list2ring [_,_] = Nothing
list2ring (x:y:z:xs) = Just $ makeLinearRing x y z xs

polygon2geo :: (Vector 3 GLfloat -> GeoPositionWithoutCRS)
            -> S.Polygon 3 GLfloat
            -> Maybe GeoPolygon
polygon2geo f (S.SimpleConvexPolygon xs) = liftM (GeoPolygon . (:[])) . list2ring $ map f xs
polygon2geo f (S.SimplePolygon xs) = liftM (GeoPolygon . (:[])) . list2ring $ map f xs
polygon2geo f (S.GenericPolygon xxs) = liftM (GeoPolygon . join . map _unGeoPolygon) $ mapM (polygon2geo f) xxs


--SimpleConvexPolygon [Point n x] -- ^ The simplest type of polygon
--                 | SimplePolygon [Point n x] -- ^ Polygon without holes
--                 | GenericPolygon [Polygon n x] -- ^ Polygon with holes

--cityToFeatureCollection :: City -> GeoFeatureCollection ()
--cityToFeatureCollection city =


