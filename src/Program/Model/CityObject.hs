{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Model.CityObject
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.Model.CityObject
    ( CityObject (), LocatedCityObject, behavior, objPolygons, objPoints
    , GeoJsonGeometry (..)
    , PointData (), vertexArray, indexArray, vertexArrayLength, indexArrayLength
    , processPolygonFeature
    , ObjectBehavior (..)
    , packPointData, emptyPointData
    )
    where

import GHCJS.Foreign.Callback (Callback)

import Data.Coerce (coerce)
import Data.JSString (unpack')

import GHCJS.Foreign
import GHCJS.Marshal.Pure
import GHCJS.Types
import JavaScript.TypedArray

import GHCJS.WebGL

import SmallGL.WritableVectors

import qualified Data.JSArray as JSArray
import Data.Geometry
import qualified Data.Geometry.Transform as T
import Data.Geometry.Structure.LinearRing (toList', linearRing)
import Data.Geometry.Structure.Polygon
import Data.Geometry.Structure.PointSet (PointArray, PointSet (..), shrinkVectors, boundingRectangle2D)
import Data.Geometry.Structure.Feature


-- | Id of geometry in Luci
newtype GeomID = GeomID JSVal
instance Eq GeomID where
    (==) = cmpIDs
instance Show GeomID where
    show (GeomID jv) = unpack' (pFromJSVal jv)
{-# INLINE cmpIDs #-}
foreign import javascript unsafe "$1 === $2" cmpIDs :: GeomID -> GeomID -> Bool



-- | Whether one could interact with an object or not
data ObjectBehavior = Static | Dynamic deriving (Eq,Show)

instance PToJSVal ObjectBehavior where
    pToJSVal Static = jsTrue
    pToJSVal Dynamic = jsFalse

instance PFromJSVal ObjectBehavior where
    pFromJSVal js = if isTruthy js
        then Static
        else Dynamic

-- | what is the type of object content
data ScenarioLayer = Objects3D | Footprints | Objects2D
    deriving (Eq, Show)

instance PToJSVal ScenarioLayer where
    pToJSVal Objects3D  = jsval ("objects3D"  :: JSString)
    pToJSVal Footprints = jsval ("footprints" :: JSString)
    pToJSVal Objects2D  = jsval ("objects2D"  :: JSString)

instance PFromJSVal (Maybe ScenarioLayer) where
    pFromJSVal js = if not (isTruthy js)
        then Nothing
        else case pFromJSVal js :: JSString of
            "objects3D"  -> Just Objects3D
            "footprints" -> Just Footprints
            "objects2D"  -> Just Objects2D
            _            -> Nothing


-- | Basic entity in the program; Defines the logic of the interaction and visualization
newtype CityObject = CityObject JSVal
instance JSArray.LikeJS CityObject

-- | Try to process geometry into building.
--   Assumes that feature contains polygon or multipolygon.
--   If the geometry is a single polygon or multipolygon with one polygon inside, it will add walls to it.
processPolygonFeature :: GLfloat -- ^ default height in camera space
                      -> GLfloat -- ^ scale objects before processing
                      -> Vector2 GLfloat -- ^ shift objects before processing
                      -> Feature -> Either JSString LocatedCityObject
processPolygonFeature defHeight scale shift sObj = if isSlave sObj then Left "Skipping a slave scenario object." else
    getSizedGeoJSONGeometry (vector3 0 0 (defHeight / scale)) sObj
    >>= \geom -> toBuildingMultiPolygon geom
    >>= \mpoly ->
    let qt@(T.QFTransform trotScale tshift locMPoly) = locateMultiPolygon scale shift mpoly
        pdata = buildingPointData locMPoly
    in Right . flip T.wrap qt  $ js_FeatureToCityObject sObj pdata locMPoly tshift trotScale

locateMultiPolygon :: GLfloat
                   -> Vector2 GLfloat
                   -> MultiPolygon 3 GLfloat
                   -> T.QFTransform (MultiPolygon 3 GLfloat)
locateMultiPolygon scale shift mpoly = T.QFTransform (
        -- getRotScale (vector3 1 0 0) (unit xdir)
        axisRotation (vector3 0 0 1) (a)
        ) center $ mapCallbackSet f mpoly
    where points = shrinkVectors $ toPointArray mpoly :: PointArray 2 GLfloat
          (center', vx, vy) = boundingRectangle2D points
          a = atan2 (indexVector 1 vx) (indexVector 0 vx)
          scalev = broadcastVector scale
          center = resizeVector (center' - shift) * scalev
          xdir = resizeVector (unit vx) * scalev
          ydir = resizeVector (unit vy) * scalev
          zdir = vector3 0 0 scale
          f = getLocatingCallback (resizeVector center') xdir ydir zdir



foreign import javascript unsafe "$r = function(v){var t = [v[0]-$1[0],v[1]-$1[1],v[2]-$1[2]]; return [dotJSVec($2,t),dotJSVec($3,t),dotJSVec($4,t)];}"
    getLocatingCallback :: Vector3 GLfloat -- ^ shift
                        -> Vector3 GLfloat -- ^ x dir
                        -> Vector3 GLfloat -- ^ y dir
                        -> Vector3 GLfloat -- ^ z dir
                        -> Callback (Vector3 GLfloat -> Vector3 GLfloat)


{-# INLINE behavior #-}
behavior :: CityObject -> ObjectBehavior
behavior = pFromJSVal . behavior'
{-# INLINE behavior' #-}
foreign import javascript unsafe "$1['properties']['static']"
    behavior' :: CityObject -> JSVal
{-# INLINE objPolygons #-}
foreign import javascript unsafe "$1['geometry']"
    objPolygons :: CityObject -> MultiPolygon 3 GLfloat
{-# INLINE objPoints #-}
foreign import javascript unsafe "$1['pointData']"
    objPoints :: CityObject -> PointData


{-# INLINE isSlave #-}
foreign import javascript unsafe "$1['properties']['isSlave']"
    isSlave :: Feature -> Bool




type LocatedCityObject = T.QFTransform CityObject

instance JSArray.LikeJS LocatedCityObject where
    asJSVal (T.QFTransform rs sh obj) = rs `seq` sh `seq` obj `seq` js_delocateCityObject obj sh rs
    asLikeJS js = case js_locateCityObject js of
                   (jv, sh, rs) -> T.QFTransform rs sh (coerce jv)


instance PFromJSVal (Maybe LocatedCityObject) where
    pFromJSVal js = if not (isTruthy jv)
        then Nothing
        else Just $ T.QFTransform rs sh (coerce jv)
        where (jv, sh, rs) = js_locateCityObject js

foreign import javascript unsafe "if($1['properties']['transform'] && $1['properties']['transform']['shift'] && $1['properties']['transform']['rotScale']\
                                 \ && $1['pointData']){\
                                 \   $r1 = $1;\
                                 \   $r2 = $1['properties']['transform']['shift'].slice();\
                                 \   $r3 = $1['properties']['transform']['rotScale'].slice();}\
                                 \else{$r1 = null; $r2 = null; $r3 = null;}"
    js_locateCityObject :: JSVal
                        -> (JSVal, Vector3 GLfloat, QFloat)



foreign import javascript unsafe "$r = gm$cloneCityObject($1); $r['properties']['transform']['shift'] = $2.slice(); $r['properties']['transform']['rotScale'] = $3.slice();"
    js_delocateCityObject :: CityObject
                          -> Vector3 GLfloat
                          -> QFloat
                          -> JSVal

instance PToJSVal LocatedCityObject where
    pToJSVal (T.QFTransform rs sh obj) = js_delocateCityObject obj sh rs


foreign import javascript unsafe "$r = {}; $r['properties'] = $1['properties']; $r['type'] = $1['type']; $r['geometry'] = $3; $r['pointData'] = $2;\
                                 \$r['properties']['transform'] = {}; $r['properties']['transform']['shift'] = $4; $r['properties']['transform']['rotScale'] = $5;"
    js_FeatureToCityObject :: Feature
                                  -> PointData
                                  -> MultiPolygon 3 GLfloat
                                  -> Vector3 GLfloat
                                  -> QFloat
                                  -> CityObject



toBuildingMultiPolygon :: GeoJsonGeometry 3 GLfloat -> (Either JSString (MultiPolygon 3 GLfloat))
toBuildingMultiPolygon (GeoPoint _) = Left "toBuildingMultiPolygon: GeoJSON Point is not convertible to MultiPolygon"
toBuildingMultiPolygon (GeoMultiPoint _) = Left "toBuildingMultiPolygon: GeoJSON MultiPoint is not convertible to MultiPolygon"
toBuildingMultiPolygon (GeoLineString _) = Left "toBuildingMultiPolygon: GeoJSON LineString is not convertible to MultiPolygon"
toBuildingMultiPolygon (GeoMultiLineString _) = Left "toBuildingMultiPolygon: GeoJSON MultiLineString is not convertible to MultiPolygon"
toBuildingMultiPolygon (GeoPolygon p) = Right $ completeBuilding [p]
toBuildingMultiPolygon (GeoMultiPolygon mp) = Right . completeBuilding . JSArray.toList $ mp

completeBuilding :: [Polygon 3 GLfloat] -> MultiPolygon 3 GLfloat
completeBuilding xs@(_:_:_) = JSArray.fromList xs
completeBuilding [] = JSArray.fromList []
completeBuilding [roof] = JSArray.fromList $ roof : map buildWall wallLines
    where wallLines :: [(Vector3 GLfloat, Vector3 GLfloat)]
          wallLines = JSArray.toList roof >>= toLines . toList'
          toLines (p1:p2:pts) = (p1,p2) : toLines (p2:pts)
          toLines _ = []
          buildWall :: (Vector3 GLfloat, Vector3 GLfloat) -> Polygon 3 GLfloat
          buildWall (p1, p2) = JSArray.fromList [linearRing p1 p2 (flat p2)  [flat p1]]
          flat (unpackV3 -> (x,y,_)) = vector3 x y 0


newtype PointData = PointData JSVal
instance IsJSVal (PointData)

{-# INLINE vertexArray #-}
foreign import javascript unsafe "$1.vertexArray"
    vertexArray :: PointData -> ArrayBuffer
{-# INLINE indexArray #-}
foreign import javascript unsafe "$1.indexArray"
    indexArray :: PointData -> ArrayBuffer
{-# INLINE vertexArrayLength #-}
foreign import javascript unsafe "$r = ($1.vertexArray.byteLength / 20) | 0;"
    vertexArrayLength :: PointData -> GLsizei
{-# INLINE indexArrayLength #-}
foreign import javascript unsafe "$r = ($1.indexArray.byteLength / 2) | 0;"
    indexArrayLength :: PointData -> GLsizei


buildingPointData :: MultiPolygon 3 GLfloat -> PointData
buildingPointData mp = packPointData vertices indices
    where (points, normals', indices) = triangulateMultiPolygon3D mp
          (normals, texcoords) = hackyNormalsTexcoords normals'
          vertices = packPoints points normals texcoords

-- | TODO: Here I create texture coordinates incorrectly
foreign import javascript unsafe "$r1 = $1.map(function(p){return p.map(function(x){return Math.min(127, Math.max(-128, x*128 | 0));});});\
                                 \$r2 = $1.map(function(p){return [0,0]});"
    hackyNormalsTexcoords :: PointArray 3 GLfloat -> (PointArray 3 GLbyte, PointArray 2 GLushort)


foreign import javascript unsafe "var iss = new Uint16Array($2.length); iss.set($2); $r = { vertexArray: $1, indexArray: iss['buffer'] }"
    packPointData :: ArrayBuffer -> JSArray.JSArray Int -> PointData

foreign import javascript unsafe "{ vertexArray: new ArrayBuffer(0), indexArray: new Uint16Array(0) }"
    emptyPointData :: PointData

