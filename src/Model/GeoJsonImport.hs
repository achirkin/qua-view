{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
--
-- Module      :  Model.GeoJsonImport
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Model.GeoJsonImport
    ( polygon2DtoCityObject
    , featureCollection2DtoObjects
    ) where

import Control.Monad ((>=>))
import Control.Arrow (first, second)

import GHCJS.WebGL

import Geometry.Space
import Geometry.Space.Transform
import Geometry.Math
import qualified Geometry.Structure as S
import Model.CityObject

import Data.Geospatial
import Data.LinearRing
--import Debug.Trace (traceShow)


defaultHeight :: GLfloat
defaultHeight = 3

approxTolerance :: GLfloat
approxTolerance = 0.1

-- | Loses all rings except first one
polygon2DtoCityObject :: ObjectBehavior
                      -> GLfloat -- ^ Scale
                      -> GeoPolygon
                      -> Either String (STransform "Quaternion" GLfloat CityObject)
polygon2DtoCityObject beh scal (GeoPolygon (points:_)) = f . map (\(x:z:_) -> fmap ((scal*) . realToFrac) $ Vector2 x (-z)) . fromLinearRing $ points
    where f :: [Vector 2 GLfloat] -> Either String (STransform "Quaternion" GLfloat CityObject)
          f [_,p1,p2,p3,p4] | a@(Vector2 ax ay) <- p2.-p1
                            , b <- p4.-p1
                            , c <- p2.-p3
                            , d <- p4.-p3
                            , (s,si) <- geojsonTransform (mean [p1,p2,p3,p4]) (atan2 (-ay) ax)
                            = Right . flip runApprox approxTolerance $ do
            corner1 <- areOrthogonal a b
            corner2 <- areOrthogonal a c
            corner3 <- areOrthogonal c d
            return . s $ if corner1 && corner2 && corner3
                then BoxHut beh (Vector3 (normL2 a) defaultHeight (normL2 b))
                else bds [p1,p2,p3,p4] si
          f (_:xs@(p1:p2:_:_)) | Vector2 ax ay <- p2.-p1
                         , (s,si) <- geojsonTransform (mean xs) (atan2 (-ay) ax)
                         = Right . s $ bds xs si
          f _ = Left "polygon2DtoCityObject: not enough points!"
          bds xs si = Building beh . S.SimplePolygon $ map (\(Vector2 x z) -> transform . si $ Vector3 x defaultHeight z) xs
polygon2DtoCityObject _ _ (GeoPolygon []) = Left "polygon2DtoCityObject: No polygons at all!"






featureCollection2DtoObjects :: ObjectBehavior
                             -> GLfloat -- ^ Scale
                             -> GeoFeatureCollection a
                             -> ([STransform "Quaternion" GLfloat CityObject], [String])
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





geojsonTransform :: Vector2 GLfloat
                 -> GLfloat
                 -> ( a -> STransform "Quaternion" GLfloat a
                    , b -> STransform "Quaternion" GLfloat b)
geojsonTransform (Vector2 sx sy) angle = (s, flip wrap (inverseTransform $ s ()))
    where s = translate (Vector3 sx 0 sy) >=> rotateY angle
