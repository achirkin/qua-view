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


import Control.Monad ((>=>))
import Control.Arrow (first, second)

import GHCJS.WebGL

import Geometry.Space
import Geometry.Space.Transform
import Geometry.Math
import qualified Geometry.Structure as S
import Program.Model.CityObject

import Data.Geospatial
import Data.LinearRing
--import Debug.Trace (traceShow)



--approxTolerance :: GLfloat
--approxTolerance = 0.1



----------------------------------------------------------------------------------------------------
-- 2D GeoJSON
----------------------------------------------------------------------------------------------------

defaultHeight :: GLfloat
defaultHeight = 3

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



featureCollection2DtoObjects :: ObjectBehavior
                             -> GLfloat -- ^ Scale
                             -> GeoFeatureCollection a
                             -> ([LocatedCityObject], [String])
featureCollection2DtoObjects b scal gfc = organize $ gms >>= f
    where gms = map _geometry $ _geofeatures gfc :: [GeospatialGeometry]
          f NoGeometry = [Left "No geometry"]
          f (Point p) = [Left $ "Point " ++ show p]
          f (MultiPoint mp) = [Left $ "MultiPoint " ++ show mp]
          f (Polygon poly) = [polygon2DtoCityObject b scal poly]
          f (MultiPolygon mpoly) = map Polygon (splitGeoMultiPolygon mpoly) >>= f
          f (Line l) = [Left $ "Line " ++ show l]
          f (MultiLine ml) = [Left $ "MultiLine " ++ show ml]
          f (Collection xs) = xs >>= f
          organize [] = ([],[])
          organize (Left m : xs) = second (m:) $ organize xs
          organize (Right x : xs) = first (x:) $ organize xs


geojsonTransform2d :: Vector2 GLfloat
                   -> GLfloat
                   -> ( a -> STransform "Quaternion" GLfloat a
                      , b -> STransform "Quaternion" GLfloat b)
geojsonTransform2d (Vector2 sx sy) angle = (s, flip wrap (inverseTransform $ s ()))
    where s = translate (Vector3 sx 0 sy) >=> rotateY angle



----------------------------------------------------------------------------------------------------
-- 3D GeoJSON
----------------------------------------------------------------------------------------------------


-- | Loses all rings except first one
polygon3DtoCityObject :: ObjectBehavior
                      -> GLfloat -- ^ Scale
                      -> GeoPolygon
                      -> Either String LocatedCityObject
polygon3DtoCityObject beh scal (GeoPolygon (pts:_)) =
    f . map (\(x:z:y:_) -> ((scal*) . realToFrac) <$> Vector3 x y (-z)) . fromLinearRing $ pts
    where f :: [Vector 3 GLfloat] -> Either String (STransform "Quaternion" GLfloat CityObject)
          f (_:xs@(p1:p2:_:_)) | Vector3 ax _ az <- p2.-p1
                         , (s,si) <- geojsonTransform3d (mean xs) (atan2 (-az) ax)
                         = Right . s $ bds xs si
          f _ = Left "polygon3DtoCityObject: not enough points!"
          bds xs si = building beh . S.SimplePolygon $
            map (transform . si) xs
polygon3DtoCityObject _ _ (GeoPolygon []) = Left "polygon3DtoCityObject: No polygons at all!"




geojsonTransform3d :: Vector3 GLfloat
                   -> GLfloat
                   -> ( a -> STransform "Quaternion" GLfloat a
                      , b -> STransform "Quaternion" GLfloat b)
geojsonTransform3d shift angle = (s, flip wrap (inverseTransform $ s ()))
    where s = translate shift >=> rotateY angle




