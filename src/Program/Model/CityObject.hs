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
    , processScenarioObject
    , ObjectBehavior (..)
--    ( CityObject (..)
--    , ObjectBehavior (..)
--    , PointData (..)
--    , LocatedCityObject
--    , building
--    , ScenarioObject (..), GeomID, ScenarioLayer (..), ImportedScenarioObject (..)
    )
    where

--import Debug.Trace (traceShow)

import GHCJS.Foreign.Callback (Callback)

import GHC.TypeLits
import Data.Proxy (Proxy(..))
import Data.Coerce (coerce)
import Data.JSString (unpack', append, pack)

import GHCJS.Foreign
import GHCJS.Marshal.Pure
import GHCJS.Types
import JavaScript.TypedArray


import Data.JSArray
import GHCJS.WebGL

import SmallGL.WritableVectors

import qualified Data.JSArray as JSArray
import Data.Geometry
import qualified Data.Geometry.Transform as T
import Data.Geometry.Structure.LinearRing (toList', linearRing)
import Data.Geometry.Structure.Polygon
import Data.Geometry.Structure.PointSet (PointArray, PointSet (..), shrinkVectors, boundingRectangle2D)
import Data.Geometry.Structure.Feature

--defaultHeightDynamic :: GLfloat
--defaultHeightDynamic = 3
--
--defaultHeightStatic :: GLfloat
--defaultHeightStatic = 3




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
instance LikeJS CityObject

processScenarioObject :: GLfloat -- ^ default height in camera space
                      -> GLfloat -- ^ scale objects before processing
                      -> Vector2 GLfloat -- ^ shift objects before processing
                      -> Feature -> Either JSString LocatedCityObject
processScenarioObject defHeight scale shift sObj | scale <= 0 = Left . pack $ "processScenarioObject: Scale is wrong possible (" ++ show scale ++ ")"
                                                 | otherwise  = if isSlave sObj then Left "Skipping a slave scenario object." else
    pFromJSVal (getGeoJSONGeometry sObj) >>= \(ND geom) ->
    toBuildingMultiPolygon (defHeight / scale) geom >>= \mpoly ->
    let qt@(T.QFTransform trotScale tshift locMPoly) = locateMultiPolygon scale shift mpoly
        pdata = buildingPointData locMPoly
    in Right . flip T.wrap qt $ js_ScenarioObjectToCityObject sObj pdata locMPoly tshift trotScale

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



data GeoJsonGeometryND = forall n . KnownNat n => ND (GeoJsonGeometry n)

data GeoJsonGeometry n = GeoPoint JSVal -- coordinates (Vector n GLfloat)
                       | GeoMultiPoint JSVal -- coordinates (PointArray n GLfloat)
                       | GeoLineString JSVal -- coordinates LineString
                       | GeoMultiLineString JSVal -- coordinates MultiLineString
                       | GeoPolygon (Polygon n GLfloat)
                       | GeoMultiPolygon (MultiPolygon n GLfloat)

instance PFromJSVal (Either JSString (GeoJsonGeometry n)) where
    pFromJSVal js = if not (isTruthy js)
        then Left "Could parse GeoJsonGeometry: it is falsy!"
        else case getGeoJSONType js of
            "Polygon"      -> Right . GeoPolygon $ pFromJSVal js
            "MultiPolygon" -> Right . GeoMultiPolygon $ pFromJSVal js
            t              -> Left $ "Could parse GeoJsonGeometry: type " `append` t `append` " is not supported currently."

instance PFromJSVal (Either JSString GeoJsonGeometryND) where
    pFromJSVal js = if not (isTruthy js)
        then Left "Could parse GeoJsonGeometryND: it is falsy!"
        else let mdims = someNatVal . toInteger $ getDimensionality js
             in case mdims of
                 Nothing -> Left "Could parse GeoJsonGeometryND: failed to find dimensionality of the data"
                 Just (SomeNat proxy) -> pFromJSVal js >>= Right . ND . dimensionalize proxy


{-# INLINE dimensionalize #-}
dimensionalize :: KnownNat n => Proxy n -> GeoJsonGeometry n -> GeoJsonGeometry n
dimensionalize _ = id

{-# INLINE getDim2 #-}
getDim2 :: KnownNat n => a n x -> Proxy n
getDim2 _ = Proxy


{-# INLINE getGeoJSONType #-}
foreign import javascript unsafe "$1['type']"
    getGeoJSONType :: JSVal -> JSString
{-# INLINE getGeoJSONGeometry #-}
foreign import javascript unsafe "$1['geometry']"
    getGeoJSONGeometry :: Feature -> JSVal
--{-# INLINE getGeoJSONProperties #-}
--foreign import javascript unsafe "$1['properties']"
--    getGeoJSONProperties :: JSVal -> JSVal

type LocatedCityObject = T.QFTransform CityObject

instance LikeJS LocatedCityObject where
    asJSVal (T.QFTransform rs sh obj) = js_delocateCityObject obj sh rs
    asLikeJS js = T.QFTransform rs sh (coerce jv)
        where (jv, sh, rs) = js_locateCityObject js


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
    js_ScenarioObjectToCityObject :: Feature
                                  -> PointData
                                  -> MultiPolygon 3 GLfloat
                                  -> Vector3 GLfloat
                                  -> QFloat
                                  -> CityObject

{-# INLINE getDimensionality #-}
-- | Get length of the coordinates in GeoJSON object.
--   Default length is 3.
foreign import javascript unsafe "var dims = $1['coordinates'] ? gm$GeometryDims($1['coordinates']) : 3; $r = dims === 0 ? 3 : dims;"
    getDimensionality :: JSVal -> Int


toBuildingMultiPolygon :: KnownNat n => GLfloat -> GeoJsonGeometry n -> (Either JSString (MultiPolygon 3 GLfloat))
toBuildingMultiPolygon _ (GeoPoint _) = Left "toBuildingMultiPolygon: GeoJSON Point is not convertible to MultiPolygon"
toBuildingMultiPolygon _ (GeoMultiPoint _) = Left "toBuildingMultiPolygon: GeoJSON MultiPoint is not convertible to MultiPolygon"
toBuildingMultiPolygon _ (GeoLineString _) = Left "toBuildingMultiPolygon: GeoJSON LineString is not convertible to MultiPolygon"
toBuildingMultiPolygon _ (GeoMultiLineString _) = Left "toBuildingMultiPolygon: GeoJSON MultiLineString is not convertible to MultiPolygon"
toBuildingMultiPolygon defHeight (GeoPolygon p) = toBuildingMultiPolygon defHeight . GeoMultiPolygon $ JSArray.fromList [p]
toBuildingMultiPolygon defHeight (GeoMultiPolygon mp) =
    let dims = natVal $ getDim2 mp
    in fmap (completeBuilding . JSArray.toList ) $
        case dims of
         0 -> Left "toBuildingMultiPolygon: 0 is wrong dimensionality for points."
         1 -> Left "toBuildingMultiPolygon: 1 is wrong dimensionality for points."
         3 -> Right $ coerce mp
         _ -> Right $ toMultiPolygon3D defHeight mp

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


foreign import javascript unsafe "{ vertexArray: $1, indexArray: Uint16Array.from($2).buffer }"
    packPointData :: ArrayBuffer -> JSArray Int -> PointData

--norm = fmap (round . max (-128) . min 127)
----            $ ((if c then 1 else -1) * 127 / normL2 nr') ..* nr' :: Vector3 GLbyte

--{-# INLINE wrapInJSQTransform #-}
--foreign import javascript unsafe "if (! $1['properties']){ $1['properties'] = {}; }\
--                                 \if (! $1['properties']['transform']){ $1['properties']['transform'] = {}; }\
--                                 \$1['properties']['transform']['shift'] = [0,0,0];\
--                                 \$1['properties']['transform']['rotScale'] = [0,0,0,1];\
--                                 \$r = $1;"
--    wrapInJSQTransform :: JSVal -> JSQTransform x

--data ScenarioObject = ScenarioObject ScenarioLayer GeomID (Maybe GeomID) LocatedCityObject
--    deriving Show
--
----data ImportedScenarioObject = ISObject ScenarioLayer (Maybe GeomID) ObjectBehavior (Polygon 3 GLfloat)
----    deriving Show
--
---- | Basic entity in the program; Defines the logic of the interaction and visualization
--data CityObject =
--    -- | Building object.
--    --   Polygon of the building represents the roof shape;
--    --   floor is assumed to be at zero height;
--    --   walls are strictly vertical.
--    --   All together this gives full info on the building shape - extruded polygon.
--    --   Points of the polygon assumed to be centered around zero coords.
--    Building
--    { behavior   :: !ObjectBehavior
--    , objPolygon :: !(Polygon 3 GLfloat)
--    , points     :: !(PointData Vertex20CNT GLushort)
--    }
--
--instance Show CityObject where
--    show obj = show (behavior obj) ++ " CityObject: " ++ show (objPolygon obj)
--
--
--
--data PointData points indices = PointData
--    { vertexArray       :: !ByteArray
--    , vertexArrayLength :: !GLsizei
--    , indexArray        :: !ByteArray
--    , indexArrayLength  :: !GLsizei
--    }
--
--
--building :: ObjectBehavior -> Polygon 3 GLfloat -> CityObject
--building beh poly =  Building
--    { behavior   = beh
--    , objPolygon = poly
--    , points     = fillBuildingArrays pts
--    }
--    where mkpts pol = case pol of
--            SimpleConvexPolygon xs -> xs
--            SimplePolygon xs -> xs
--            GenericPolygon [] -> []
--            GenericPolygon (p:_) -> mkpts p
--          pts = mkpts poly
--
--
--
----instance Boundable CityObject 2 GLfloat where
----    minBBox Building{ objPolygon = poly} = boundingBox (Vector2 lx lz)
----                                                       (Vector2 hx hz)
----        where bound3 = minBBox poly
----              Vector3 lx _ lz = lowBound bound3
----              Vector3 hx _ hz = lowBound bound3
----
----instance Boundable CityObject 3 GLfloat where
----    minBBox Building{ objPolygon = poly} = boundPair bb zbound
----        where bb = minBBox poly
----              Vector3 lx _ lz = lowBound bb
----              Vector3 hx _ hz = highBound bb
----              zbound = boundingBox (Vector3 lx 0 lz) (Vector3 hx 0 hz)
--
--
------------------------------------------------------------------------------------------------------
---- Helpers
------------------------------------------------------------------------------------------------------
--
---- create list of points, normals, texture coords for a wall out of two points
--buildWall :: Vector3 GLfloat -- first point
--          -> Vector3 GLfloat -- second point
--          -> Bool -- if the normal is correct
--          -> (MutableByteArray (PrimState (ST s)), Int, MutableByteArray (PrimState (ST s)), Int)
--          -> ST s (MutableByteArray (PrimState (ST s)), Int, MutableByteArray (PrimState (ST s)), Int)
--buildWall p1@(Vector3 x1 y1 z1) p2@(Vector3 x2 y2 z2) c (arr, i, ix, j) = do
--    -- write points
--    writeByteArray arr  i    (p1,norm,Vector2 m h1)
--    writeByteArray arr (i+1) (p2,norm,Vector2 0 h2)
--    writeByteArray arr (i+2) (Vector3 x2 0 z2,norm, Vector2 0 0 :: Vector2 GLushort)
--    writeByteArray arr (i+3) (Vector3 x1 0 z1,norm, Vector2 m 0)
--    -- write indices
--    writeByteArray ix  j     i'
--    writeByteArray ix (j+1) (i'+1)
--    writeByteArray ix (j+2) (i'+2)
--    writeByteArray ix (j+3)  i'
--    writeByteArray ix (j+4) (i'+2)
--    writeByteArray ix (j+5) (i'+3)
--    -- return modified
--    return (arr, i+4, ix, j+6)
--    where (h1,h2) = if y1 > y2 then (m, round $ y2/y1 * fromIntegral m)
--                               else (round $ y1/y2 * fromIntegral m, m) :: (GLushort, GLushort)
--          m = 65535
--          i' = fromIntegral i :: GLushort
--          nr' = Vector3 0 1 0 `cross` (p2.-p1)
--          norm = fmap (round . max (-128) . min 127)
--            $ ((if c then 1 else -1) * 127 / normL2 nr') ..* nr' :: Vector3 GLbyte
--
--
--buildRoof :: [Vector3 GLfloat]
--          -> (MutableByteArray (PrimState (ST s)), Int, MutableByteArray (PrimState (ST s)), Int)
--          -> ST s (MutableByteArray (PrimState (ST s)), Int, MutableByteArray (PrimState (ST s)), Int)
--buildRoof pts (arr, i, ix, j) = do
--    il <- M.foldM (\ii x -> writeByteArray arr ii x >> return (ii+1)) i rpts
--    jl <- M.foldM (\jj x -> writeByteArray ix jj (fromIntegral x :: GLushort) >> return (jj+1))
--        j (triangulate3 (SimplePolygon pts) >>= (\(i0,i1,i2) -> [i+i0-1,i+i1-1,i+i2-1]))
--    -- return modified
--    return (arr, il, ix, jl)
--    where norm :: Vector3 GLfloat -> Vector3 GLbyte
--          norm v = fmap (round . max (-128) . min 127)
--                . (*..127) . unit $ v `cross` Vector3 0 1 0 `cross` v
--          rpts = zip (ptl:pts) pts >>= \(p1,p2) -> let no = norm (p2 .- p1)
--                                                   in [(p2,no, Vector2 0 0 :: Vector2 GLushort)]
--          ptl = last pts
--
--
--fillBuildingArrays :: [Vector3 GLfloat]
--                   -> PointData Vertex20CNT GLushort
--fillBuildingArrays pts = runST $ do
--    marr <- newByteArray (psize * sizeOf (undefined :: Vertex20CNT))
--    miarr <- newByteArray (isize * sizeOf (undefined :: GLushort))
--    s1 <- M.foldM (\s (p1,p2) -> buildWall p1 p2 clockwise s) (marr, 0, miarr, 0) $ zip (last pts:pts) pts
--    (marr', _, miarr', _) <- buildRoof pts s1
--    arr <- unsafeFreezeByteArray marr'
--    iarr <- unsafeFreezeByteArray miarr'
--    return $ PointData arr (fromIntegral psize) iarr (fromIntegral isize)
--    where n = length pts
--          psize = n*4 -- walls
--                  + n -- roof
--          isize = n*6 -- walls
--                  + (n-2)*3 -- roof
--          clockwise = shoelace pts
--
--
---- | For simple polygons says for each point whether it is convex (True) or concave (False)
--shoelace :: (Fractional x, Ord x) => [Vector3 x] -> Bool
--shoelace pts = area > 0
--    where area = sum $ f pts (head pts) -- area of whole thing (shoelace)
--          f (Vector3 x1 _ z1 :xs@(Vector3 x2 _ z2 :_)) l = x1*z2 - x2*z1 : f xs l
--          f [Vector3 x1 _ z1] (Vector3 x2 _ z2) = [x1*z2 - x2*z1]
--          f [] _ = []
