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


--import Unsafe.Coerce
import GHCJS.Foreign
--import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Instances ()
--import GHCJS.Useful

import Data.Primitive
import Data.List (find)
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
                    fmap ((*scal) . realToFrac) (Vector3 x y (-z))]
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

--city2ref :: Maybe (GLfloat, Vector2 GLfloat) -> City -> IO (JSVal)
--city2ref mtrasform city = do
--        props <- newObj
----        toJSVal >>= flip (unsafeSetProp "layer") props
----        toJSVal >>= flip (unsafeSetProp "geomID") props
----        toJSVal >>= flip (unsafeSetProp "static") props
----        case mid of
----            Nothing -> return ()
----            Just i -> toJSVal >>= flip (unsafeSetProp "masterGeomID") props
----        geom <- newObj
----        unsafeSetProp "type" (toJSString "MultiPolygon") geom
----        case layer of
----            SLbuildings -> mapM toJSRef (cityObjectPolygons3D lobj) >>= toArray
----                >>= flip (unsafeSetProp "coordinates") geom
----            SLfootprints -> mapM toJSRef (cityObjectPolygons2D lobj) >>= toArray
----                >>= flip (unsafeSetProp "coordinates") geom
----        unsafeSetProp "geometry" geom jobj
--        jobj <- newObj
--        unsafeSetProp "type" (toJSString "FeatureCollection") jobj
--        unsafeSetProp "properties" props jobj
--        return jobj

--wiredGeom2FeaturesRef :: Maybe (GLfloat, Vector2 GLfloat) -> WiredGeometry -> IO [JSVal]
--wiredGeom2FeaturesRef mt (WiredGeometry )


--toJSRef (ScenarioObject layer gid mid lobj) = do
--        props <- newObj
--        toJSVal >>= flip (unsafeSetProp "layer") props
--        toJSVal >>= flip (unsafeSetProp "geomID") props
--        toJSVal >>= flip (unsafeSetProp "static") props
--        case mid of
--            Nothing -> return ()
--            Just i -> toJSVal >>= flip (unsafeSetProp "masterGeomID") props
--        jobj <- newObj
--        geom <- newObj
--        unsafeSetProp "type" (toJSString "MultiPolygon") geom
--        case layer of
--            SLbuildings -> mapM toJSRef (cityObjectPolygons3D lobj) >>= toArray
--                >>= flip (unsafeSetProp "coordinates") geom
--            SLfootprints -> mapM toJSRef (cityObjectPolygons2D lobj) >>= toArray
--                >>= flip (unsafeSetProp "coordinates") geom
--        unsafeSetProp "geometry" geom jobj
--        unsafeSetProp "type" (toJSString "Feature") jobj
--        unsafeSetProp "properties" props jobj
--        return jobj
--        where behav = behavior $ unwrap lobj

instance S.Boundable (GeoFeatureCollection a) 2 GLfloat where
    minBBox gfc = S.boundSet (map _geometry (_geofeatures gfc) >>= f)
      where f :: GeospatialGeometry -> [S.BoundingBox 2 GLfloat]
            f NoGeometry = []
            f (Point p) = toVec (_unGeoPoint p) >>= \x -> [S.boundingBox x x]
            f (MultiPoint mp) = [S.boundSet $ map _unGeoPoint (splitGeoMultiPoint mp) >>= toVec]
            f (Polygon poly) = [S.boundSet $ _unGeoPolygon poly >>= fromLinearRing >>= toVec]
            f (MultiPolygon mpoly) = map Polygon (splitGeoMultiPolygon mpoly) >>= f
            f (Line l) = [S.boundSet $ fromLineString (_unGeoLine l) >>= toVec]
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
    f . map (\(x:z:_) -> (shift .+) $ ((scal*) . realToFrac) <$> Vector2 x (-z)) . fromLinearRing $ pts
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
    g . map (\(x:z:y:_) -> (shift .+) $ ((scal*) . realToFrac) <$> Vector3 x y (-z)) . fromLinearRing $ pts
    where f :: [Vector 3 GLfloat] -> Either String (STransform "Quaternion" GLfloat CityObject)
          f (_:xs@(p1:p2:_:_)) | Vector3 ax _ az <- p2.-p1
                         , (s,si) <- geojsonTransform3d (mean xs) (atan2 (-az) ax)
                         = Right . s $ bds xs si
          f _ = Left "polygon3DtoCityObject: not enough points!"
          g xs = if isVertical' xs then Left "Ignoring wall"
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
wiredGeometryToLines (WiredGeometry _ _ n' byteArray) = Collection $ parse 0
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

cityObjectPolygons2D :: LocatedCityObject -> [S.Polygon 2 GLfloat]
cityObjectPolygons2D lobj = [mapPoly f . transform $ liftM objPolygon lobj ]
    where f (Vector3 x _ z) = Vector2 x (-z)


cityObjectToPolygon3D :: LocatedCityObject -> Maybe GeoMultiPolygon
cityObjectToPolygon3D = liftM mergeGeoPolygons
                             . mapM (polygon2geo f)
                             . cityObjectPolygons3D
    where f (Vector3 x y z) = [realToFrac x, realToFrac y, realToFrac z]

cityObjectPolygons3D :: LocatedCityObject -> [S.Polygon 3 GLfloat]
cityObjectPolygons3D lobj = map (mapPoly f) . makeWalls
                             . transform
                             $ liftM objPolygon lobj
    where f (Vector3 x y z) = Vector3 x (-z) y
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

mapPoly :: (Vector n a -> Vector m b)
        -> S.Polygon n a
        -> S.Polygon m b
mapPoly f (S.SimpleConvexPolygon xs) = S.SimplePolygon $ map f xs
mapPoly f (S.SimplePolygon xs) = S.SimplePolygon $ map f xs
mapPoly f (S.GenericPolygon xxs) = S.GenericPolygon $ map (mapPoly f) xxs


instance ToJSVal where
    toJSRef (ScenarioObject layer gid mid lobj) = do
        props <- newObj
        toJSVal >>= flip (unsafeSetProp "layer") props
        toJSVal >>= flip (unsafeSetProp "geomID") props
        toJSVal >>= flip (unsafeSetProp "static") props
        case mid of
            Nothing -> return ()
            Just i -> toJSVal >>= flip (unsafeSetProp "masterGeomID") props
        jobj <- newObj
        geom <- newObj
        unsafeSetProp "type" (toJSString "MultiPolygon") geom
        case layer of
            SLbuildings -> mapM toJSRef (cityObjectPolygons3D lobj) >>= toArray
                >>= flip (unsafeSetProp "coordinates") geom
            SLfootprints -> mapM toJSRef (cityObjectPolygons2D lobj) >>= toArray
                >>= flip (unsafeSetProp "coordinates") geom
        unsafeSetProp "geometry" geom jobj
        unsafeSetProp "type" (toJSString "Feature") jobj
        unsafeSetProp "properties" props jobj
        return jobj
        where behav = behavior $ unwrap lobj

instance ToJSVal where
    toJSRef (WiredGeometry mid color n' byteArray) = do
        props <- newObj
        unsafeSetProp "layer" (toJSString "footprints") props
        unsafeSetProp "static" (toJSBool True) props
        toJSVal >>= flip (unsafeSetProp "color") props
        case mid of
            Nothing -> return ()
            Just i -> toJSVal >>= flip (unsafeSetProp "geomID") props
        jobj <- newObj
        geom <- newObj
        unsafeSetProp "type" (toJSString "MultiLineString") geom
        mapM ((>>= toArray) . mapM ((>>= toArray) . mapM toJSRef)) lines
            >>= toArray
                >>= flip (unsafeSetProp "coordinates") geom
        unsafeSetProp "geometry" geom jobj
        unsafeSetProp "type" (toJSString "Feature") jobj
        unsafeSetProp "properties" props jobj
        return jobj
        where n = fromIntegral n'
              lines = parse 0
              parse i | i >= n = []
                      | otherwise = case parseLine [] i of
                (Nothing, j) -> parse j
                (Just gl, j) -> gl : parse j
              parseLine :: [Vector3 GLfloat] -> Int -> (Maybe [[GLfloat]], Int)
              parseLine [] i | i >= n = (Nothing, i+1)
                             | otherwise = parseLine [indexByteArray byteArray i] (i+1)
              parseLine [x] i | i >= n = (Nothing, i+1)
                              | otherwise = parseLine [indexByteArray byteArray i, x] (i+1)
              parseLine xs@(x:_) i | i >= n = (Just $ map f xs, i+1)
                                   | otherwise = let cur = indexByteArray byteArray i
                in if x /= cur || i == n - 1
                   then (Just $ map f xs, i+1)
                   else parseLine (indexByteArray byteArray (i+1):xs) (i+2)
              f (Vector3 x y z) = [realToFrac x, realToFrac (-z), realToFrac y]


liftMaybe :: (Monad m) => (a -> Maybe b) -> m (Maybe a) -> m (Maybe b)
liftMaybe f = fmap (>>= f)

(>>=~) :: (Monad m) => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
infixl 1 >>=~
mma >>=~ f = mma >>= \ma -> case ma of
    Nothing -> return Nothing
    Just a -> f a

(>>~) :: (Monad m) => m (Maybe a) -> m (Maybe b) -> m (Maybe b)
infixl 1 >>~
mma >>~ f = mma >>= \ma -> case ma of
    Nothing -> return Nothing
    Just _ -> f

maybeTrue :: Monad m => (a -> Bool) -> a -> m (Maybe a)
maybeTrue test i = return $ if test i
    then Just i
    else Nothing

instance FromJSVal where
    fromJSVal =
        unsafeGetPropMaybe "type" jobj -- check if it is proper Feature
        >>=~ maybeTrue ((== "Feature") . fromJSString)
            >>~ unsafeGetPropMaybe "properties" jobj
        >>=~ \props -> do -- check if it is slave geometry
            masterG <- unsafeGetPropMaybe "masterGeomID" props
            case masterG of
                Nothing -> return $ Just ()
                Just _ -> return Nothing
        >>~ unsafeGetPropMaybe "geometry" jobj
        >>=~ \jgeometry -> unsafeGetPropMaybe "type" jgeometry
            >>=~ \jgtype -> case fromJSString jgtype of
                "MultiPolygon" -> unsafeGetPropMaybe "coordinates" jgeometry
                                >>=~ fromJSRefListOf
                "Polygon" -> unsafeGetPropMaybe "coordinates" jgeometry
                                >>=~ fromJSRef >>=~ return . return . (:[])
                _ -> return (Nothing :: Maybe [S.Polygon 3 GLfloat])
        >>=~ \rawpolygons -> liftM Just
            (unsafeGetPropMaybe "geomID" props >>=~ fromJSRef)
        >>=~ \mgeomID -> do
            mbehavior <- unsafeGetPropMaybe "static" props >>=~ fromJSRef :: IO (Maybe ObjectBehavior)
            return $ case mbehavior of
                Nothing -> Just Dynamic
                ji -> ji
        >>=~ \behav ->  do
            mlayer <- unsafeGetPropMaybe "layer" props >>=~ fromJSRef :: IO (Maybe ScenarioLayer)
            return $ case mlayer of
                Nothing -> Just SLbuildings
                ji -> ji
        >>=~ \layer -> return $ importScenarioObject layer mgeomID behav rawpolygons


importScenarioObject :: ScenarioLayer -> Maybe Int -> ObjectBehavior -> [S.Polygon 3 GLfloat]
                     -> Maybe ImportedScenarioObject
importScenarioObject layer mGeomID behav rawpolygons =
    parse3Dpolygons (map (mapPoly (\(Vector3 x mz y) -> Vector3 x y (-mz))) rawpolygons)
    >>= Just . ISObject layer mGeomID behav


parse3Dpolygons :: [S.Polygon 3 GLfloat] -> Maybe (S.Polygon 3 GLfloat)
parse3Dpolygons [] = Nothing
parse3Dpolygons ps = find (not . isVertical) ps


isVertical :: S.Polygon 3 GLfloat -> Bool
isVertical (S.GenericPolygon (p:_)) = isVertical p
isVertical (S.SimplePolygon ps) = isVertical' ps
isVertical (S.SimpleConvexPolygon ps) = isVertical' ps
isVertical _ = True


isVertical' :: [Vector3 GLfloat] -> Bool
isVertical' [] = True
isVertical' [_] = True
isVertical' [_,_] = True
isVertical' xs = all (\(a,b,c) -> flip runApprox approxTolerance
                               . areOrthogonal (Vector3 0 1 0) $ (b .- a) `cross` (c .- a))
                          . triangles $ xs
    where triangles [] = []
          triangles [_,_] = []
          triangles [a,b,c] = [(a,b,c)]
          triangles ps = map (\(i,j,k) -> (xs !! i, xs !! j, xs !! k)) . S.triangulate3 $ S.SimplePolygon ps

