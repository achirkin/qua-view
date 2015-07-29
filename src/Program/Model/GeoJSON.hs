{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
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


import Control.Monad ((>=>), liftM)
import Control.Arrow (first, second)

import GHCJS.WebGL

import Geometry.Space
import Geometry.Space.Transform
import Geometry.Math
import qualified Geometry.Structure as S
import Program.Model.CityObject

import Data.Geospatial
import Data.LinearRing
import Data.LineString
--import Debug.Trace (traceShow)



approxTolerance :: GLfloat
approxTolerance = 0.1



----------------------------------------------------------------------------------------------------
-- 2D GeoJSON
----------------------------------------------------------------------------------------------------

defaultHeight :: GLfloat
defaultHeight = 3

defaultLinesAlt :: GLfloat
defaultLinesAlt = 0.1



featureCollectionToObjects :: ObjectBehavior
                             -> GLfloat -- ^ Scale
                             -> GeoFeatureCollection a
                             -> (([LocatedCityObject], [[Vector3 GLfloat]]), [String])
featureCollectionToObjects b scal gfc = organize $ gms >>= f
    where gms = map _geometry $ _geofeatures gfc :: [GeospatialGeometry]
          f NoGeometry = [Left "No geometry"]
          f (Point p) = [Left $ "Point " ++ show p]
          f (MultiPoint mp) = [Left $ "MultiPoint " ++ show mp]
          f (Polygon poly) = [liftM Right $ if isPolygon3d poly
            then polygon3DtoCityObject b scal poly
            else polygon2DtoCityObject b scal poly]
          f (MultiPolygon mpoly) = map Polygon (splitGeoMultiPolygon mpoly) >>= f
          f (Line l) = [Right . Left $ lineToPoints l]
          f (MultiLine ml) = map Line (splitGeoMultiLine ml) >>= f
          f (Collection xs) = xs >>= f
          organize [] = (([],[]),[]) :: (([LocatedCityObject], [[Vector3 GLfloat]]), [String])
          organize (Left m : xs) = second (m:) $ organize xs
          organize (Right (Left lns) : xs) = first (second (lns:)) $ organize xs
          organize (Right (Right x) : xs) = first (first (x:)) $ organize xs
          lineToPoints :: GeoLine -> [Vector3 GLfloat]
          lineToPoints (GeoLine lineString) = pnts >>= toVector
            where pnts = fromLineString lineString
                  toVector (x:z:0:_) = [Vector3 (realToFrac x) defaultLinesAlt (realToFrac (-z))]
                  toVector (x:z:y:_) = [Vector3 (realToFrac x) (realToFrac y) (realToFrac (-z))]
                  toVector (x:z:_) = [Vector3 (realToFrac x) defaultLinesAlt (realToFrac (-z))]
                  toVector _ = []

----------------------------------------------------------------------------------------------------
-- 2D GeoJSON
----------------------------------------------------------------------------------------------------


-- | Loses all rings except first one
polygon2DtoCityObject :: ObjectBehavior
                      -> GLfloat -- ^ Scale
                      -> GeoPolygon
                      -> Either String LocatedCityObject
polygon2DtoCityObject beh scal (GeoPolygon (pts:_)) =
    f . map (\(x:z:_) -> ((scal*) . realToFrac) <$> Vector2 x (-z)) . fromLinearRing $ pts
    where f :: [Vector 2 GLfloat] -> Either String (STransform "Quaternion" GLfloat CityObject)
          f (_:xs@(p1:p2:_:_)) | Vector2 ax ay <- p2.-p1
                         , (s,si) <- geojsonTransform2d (mean xs) (atan2 (-ay) ax)
                         = Right . s $ bds xs si
          f _ = Left "polygon2DtoCityObject: not enough points!"
          bds xs si = building beh . S.SimplePolygon $
            map (\(Vector2 x z) -> transform . si $ Vector3 x defaultHeight z) xs
polygon2DtoCityObject _ _ (GeoPolygon []) = Left "polygon2DtoCityObject: No polygons at all!"


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
                      -> GeoPolygon
                      -> Either String LocatedCityObject
polygon3DtoCityObject beh scal (GeoPolygon (pts:_)) =
    g . map (\(x:z:y:_) -> ((scal*) . realToFrac) <$> Vector3 x y (-z)) . fromLinearRing $ pts
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
polygon3DtoCityObject _ _ (GeoPolygon []) = Left "polygon3DtoCityObject: No polygons at all!"




geojsonTransform3d :: Vector3 GLfloat
                   -> GLfloat
                   -> ( a -> STransform "Quaternion" GLfloat a
                      , b -> STransform "Quaternion" GLfloat b)
geojsonTransform3d shift angle = (s, flip wrap (inverseTransform $ s ()))
    where s = translate shift >=> rotateY angle




