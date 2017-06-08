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
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.Model.CityObject
    ( CityObject (), LocatedCityObject, behavior, objPolygons, objPoints, geomId, allProps, getCityObjectColor
    , GeoJsonGeometry (..)
    , PointData (), vertexArray, indexArray, vertexArrayLength, indexArrayLength
    , processPolygonFeature
    , ObjectBehavior (..)
    , packPointData, emptyPointData
    , StoreMode (..)
    , storeCityObject
    )
    where

import JsHs.Callback (Callback)

import Data.Maybe
import Data.Coerce (coerce)
--import JsHs.JSString (unpack')

---- import GHCJS.Foreign
--import GHCJS.Marshal.Pure
import JsHs.Types
import JsHs.Types.Prim
import JsHs.TypedArray
import JsHs.LikeJS.Class

import JsHs.WebGL

import SmallGL.WritableVectors

import qualified JsHs.Array as JSArray
import Data.Geometry
import qualified Data.Geometry.Transform as T
import Data.Geometry.Structure.LinearRing (toList', linearRing)
import Data.Geometry.Structure.Polygon
import Data.Geometry.Structure.PointSet (PointArray, PointSet (..), shrinkVectors, boundingRectangle2D)
import Data.Geometry.Structure.Feature
import Unsafe.Coerce
import Program.Types

-- | Whether one could interact with an object or not
data ObjectBehavior = Static | Dynamic deriving (Eq,Show)

instance LikeJS "Bool" ObjectBehavior where
    asJSVal Static = asJSVal True
    asJSVal Dynamic = asJSVal False
    asLikeJS js = if asLikeJS js
        then Static
        else Dynamic

-- | what is the type of object content
data ScenarioLayer = Objects3D | Footprints | Objects2D
    deriving (Eq, Show)

instance LikeJS "String" ScenarioLayer where
    asJSVal Objects3D  = jsval ("objects3D"  :: JSString)
    asJSVal Footprints = jsval ("footprints" :: JSString)
    asJSVal Objects2D  = jsval ("objects2D"  :: JSString)
    asLikeJS js = if jsIsNullOrUndef js
        then unsafeCoerce jsNull
        else case asLikeJS js :: JSString of
            "objects3D"  -> Objects3D
            "footprints" -> Footprints
            "objects2D"  -> Objects2D
            _            -> unsafeCoerce jsNull


-- | Basic entity in the program; Defines the logic of the interaction and visualization
newtype CityObject = CityObject JSVal
instance LikeJS "Object" CityObject

-- | Try to process geometry into building.
--   Assumes that feature contains polygon or multipolygon.
--   If the geometry is a single polygon or multipolygon with one polygon inside, it will add walls to it.
processPolygonFeature :: GLfloat -- ^ default height in camera space
                      -> GLfloat -- ^ scale objects before processing
                      -> Vector2 GLfloat -- ^ shift objects before processing
                      -> Feature -> Either JSString LocatedCityObject
processPolygonFeature defHeight scale shift sObj = if isSlave sObj then Left "Skipping a slave scenario object." else
    getSizedGeoJSONGeometry (vector3 0 0 (getHeight (defHeight / scale) sObj )) sObj
    >>= \geom -> toBuildingMultiPolygon geom
    >>= \mpoly ->
    let qt@(T.QFTransform trotScale tshift locMPoly) = locateMultiPolygon scale shift mpoly
        pdata = buildingPointData locMPoly
    in Right . flip T.wrap qt  $ js_FeatureToCityObject sObj pdata locMPoly tshift trotScale

data StoreMode = PlainFeature
               | QuaternionTranformedFeature
               | LuciMatrixTranformedFeature

-- | Store City Object into plain Feature
storeCityObject :: GLfloat -> Vector2 GLfloat -> StoreMode -> LocatedCityObject -> Feature
storeCityObject sc shift storeMode sObj' =
    case storeMode of
      PlainFeature                -> js_delocateCityObjectNoTransform obj . T.transform $ objPolygons <$> sObj
      QuaternionTranformedFeature -> JSArray.asLikeJS $ asJSVal sObj
      LuciMatrixTranformedFeature -> js_delocateCityObjectLuciMatrix obj mat
    where sObj = (pure id >>= T.translate (resizeVector shift) >>= T.scale (1/sc)) <*> sObj'
          obj = T.unwrap sObj
          (T.MTransform mat _) = T.mergeSecond (pure id) sObj

locateMultiPolygon :: GLfloat
                   -> Vector2 GLfloat
                   -> MultiPolygon 3 GLfloat
                   -> T.QFTransform (MultiPolygon 3 GLfloat)
locateMultiPolygon scale shift mpoly = T.QFTransform (
        -- getRotScale (vector3 1 0 0) (unit xdir)
        axisRotation (vector3 0 0 1) (a)
        ) center newmpoly
    where points = shrinkVectors $ toPointArray mpoly :: PointArray 2 GLfloat
          (center', vx, vy) = boundingRectangle2D points
          a = atan2 (indexVector 1 vx) (indexVector 0 vx)
          scalev = broadcastVector scale
          center = resizeVector (center' - shift) * scalev
          xdir' = resizeVector (unit vx)
          ydir' = resizeVector (unit vy)
          zdir = vector3 0 0 scale
          (xdir, ydir) = case (normL2 xdir', normL2 ydir') of
                         (0,0) -> (vector3 scale 0 0, vector3 0 scale 0)
                         (0,_) -> (cross ydir' zdir, ydir' * scalev)
                         (_,0) -> (xdir' * scalev, cross zdir xdir')
                         _     -> (xdir' * scalev, ydir' * scalev  )
          ncenter = resizeVector center'
          newmpoly = mapCallbackSet f mpoly :: MultiPolygon 3 GLfloat
          f = ncenter `seq` xdir `seq` ydir `seq` zdir `seq`
                getLocatingCallback ncenter xdir ydir zdir


foreign import javascript unsafe "$r = $2['properties'].hasOwnProperty('height') ? $2['properties']['height'] : $1; \
                                 \$r = ($r).constructor == String ? (parseFloat($r) || 0) : ($r || 0); \
                                 \$r = ($r).constructor == Number ? $r : $1;"
    getHeight :: GLfloat -> Feature -> GLfloat



foreign import javascript unsafe "$r = function(v){var t = [v[0]-$1[0],v[1]-$1[1],v[2]-$1[2]]; return [dotJSVec($2,t),dotJSVec($3,t),dotJSVec($4,t)];}"
    getLocatingCallback :: Vector3 GLfloat -- ^ shift
                        -> Vector3 GLfloat -- ^ x dir
                        -> Vector3 GLfloat -- ^ y dir
                        -> Vector3 GLfloat -- ^ z dir
                        -> Callback (Vector3 GLfloat -> Vector3 GLfloat)

getCityObjectColor :: CityObject -> (GLfloat, GLfloat, GLfloat, GLfloat)
getCityObjectColor obj = case isNothing color of
                          True -> (0.75, 0.75, 0.7, 1) -- ^ should be default color later on
                          False -> unpackV4 $ fromJust color
    where color = asLikeJS (js_smartCityObjectColor obj) :: Maybe (Vector4 GLfloat)

foreign import javascript unsafe "$r = gm$smartCityObjectColor($1);"
  js_smartCityObjectColor :: CityObject -> JSVal
  

{-# INLINE behavior #-}
behavior :: CityObject -> ObjectBehavior
behavior = asLikeJS . behavior'
{-# INLINE behavior' #-}
foreign import javascript unsafe "$1['properties']['static']"
    behavior' :: CityObject -> JSVal
{-# INLINE objPolygons #-}
foreign import javascript unsafe "$1['geometry']"
    objPolygons :: CityObject -> MultiPolygon 3 GLfloat
{-# INLINE objPoints #-}
foreign import javascript unsafe "$1['pointData']"
    objPoints :: CityObject -> PointData

foreign import javascript unsafe "$1['properties']['geomID']"
    geomId :: CityObject -> GeomId


{-# INLINE isSlave #-}
foreign import javascript unsafe "$1['properties']['isSlave']"
    isSlave :: Feature -> Bool


foreign import javascript unsafe "$1['properties']"
    allProps :: CityObject -> JSVal

type LocatedCityObject = T.QFTransform CityObject

instance LikeJS "Object" LocatedCityObject where
    asJSVal (T.QFTransform rs sh obj) = rs `seq` sh `seq` obj `seq` js_delocateCityObject obj sh rs
    asLikeJS js = case js_locateCityObject js of
                   (jv, sh, rs) -> T.QFTransform rs sh (coerce jv)
--instance PToJSVal LocatedCityObject where
--    pToJSVal (T.QFTransform rs sh obj) = rs `seq` sh `seq` obj `seq` js_delocateCityObject obj sh rs
--
--instance PFromJSVal (Maybe LocatedCityObject) where
--    pFromJSVal js = if not (isTruthy jv)
--        then Nothing
--        else Just $ T.QFTransform rs sh (coerce jv)
--        where (jv, sh, rs) = js_locateCityObject js

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

foreign import javascript unsafe "$r = {}; $r['type'] = 'Feature';\
                                 \$r['properties'] = JSON.parse(JSON.stringify($1['properties']));\
                                 \$r['geometry'] = $2;\
                                 \delete $r['properties']['transform'];"
    js_delocateCityObjectNoTransform :: CityObject -> MultiPolygon 3 GLfloat -> Feature

foreign import javascript unsafe "$r = {}; $r['type'] = 'Feature';\
                                 \$r['properties'] = JSON.parse(JSON.stringify($1['properties']));\
                                 \$r['geometry'] = $1['geometry'];\
                                 \$r['properties']['transform'] = matRowsJS($2, 4);"
    js_delocateCityObjectLuciMatrix :: CityObject -> Matrix4 GLfloat -> Feature


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
    packPointData :: ArrayBuffer -> JSArray.Array Int -> PointData

foreign import javascript unsafe "{ vertexArray: new ArrayBuffer(0), indexArray: new Uint16Array(0) }"
    emptyPointData :: PointData

