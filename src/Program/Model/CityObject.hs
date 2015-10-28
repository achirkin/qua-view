{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
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
    ( CityObject (..)
    , ObjectBehavior (..)
    , PointData (..)
    , LocatedCityObject
    , building
    , ScenarioObject (..), GeomID, ScenarioLayer (..), ImportedScenarioObject (..)
    ) where

import Unsafe.Coerce
import GHCJS.Foreign
--import GHCJS.Types
import GHCJS.Marshal
--import GHCJS.Instances ()

--import Control.Monad.Primitive
import qualified Control.Monad as M
import Control.Monad.ST (runST, ST)
--import Data.Primitive.ByteArray
--import Data.Primitive (sizeOf)
import GHCJS.WebGL

--import SmallGL.WritableVectors

import Data.Geometry
import Data.Geometry.Transform

--import Geometry.Space
--import Geometry.Space.Transform
--import Geometry.Structure

type LocatedCityObject = QFTransform CityObject

-- | Whether one could interact with an object or not
data ObjectBehavior = Static | Dynamic deriving (Eq,Show)

--instance ToJSVal where
--    toJSVal = return $ unsafeCoerce jsTrue
--    toJSVal = return $ unsafeCoerce jsFalse

--instance FromJSVal where
--    fromJSVal = return . Just $  if fromJSBool' jr
--        then Static
--        else Dynamic

data ScenarioLayer = SLbuildings | SLfootprints
    deriving Show

--instance ToJSVal where
--    toJSVal = return . unsafeCoerce $ toJSString "buildings"
--    toJSVal = return . unsafeCoerce $ toJSString "footprints"
----    toJSVal = return . unsafeCoerce $ toJSString "clutter"
--
--instance FromJSVal where
--    fromJSVal = return $ case fromJSString $ unsafeCoerce l of
--        "buildings" -> Just SLbuildings
--        "footprints" -> Just SLfootprints
--        _ -> Nothing


type GeomID = Int

data ScenarioObject = ScenarioObject ScenarioLayer GeomID (Maybe GeomID) LocatedCityObject
    deriving Show

--data ImportedScenarioObject = ISObject ScenarioLayer (Maybe GeomID) ObjectBehavior (Polygon 3 GLfloat)
--    deriving Show

-- | Basic entity in the program; Defines the logic of the interaction and visualization
data CityObject =
    -- | Building object.
    --   Polygon of the building represents the roof shape;
    --   floor is assumed to be at zero height;
    --   walls are strictly vertical.
    --   All together this gives full info on the building shape - extruded polygon.
    --   Points of the polygon assumed to be centered around zero coords.
    Building
    { behavior   :: !ObjectBehavior
    , objPolygon :: !(Polygon 3 GLfloat)
    , points     :: !(PointData Vertex20CNT GLushort)
    }

instance Show CityObject where
    show obj = show (behavior obj) ++ " CityObject: " ++ show (objPolygon obj)



data PointData points indices = PointData
    { vertexArray       :: !ByteArray
    , vertexArrayLength :: !GLsizei
    , indexArray        :: !ByteArray
    , indexArrayLength  :: !GLsizei
    }


building :: ObjectBehavior -> Polygon 3 GLfloat -> CityObject
building beh poly =  Building
    { behavior   = beh
    , objPolygon = poly
    , points     = fillBuildingArrays pts
    }
    where mkpts pol = case pol of
            SimpleConvexPolygon xs -> xs
            SimplePolygon xs -> xs
            GenericPolygon [] -> []
            GenericPolygon (p:_) -> mkpts p
          pts = mkpts poly



--instance Boundable CityObject 2 GLfloat where
--    minBBox Building{ objPolygon = poly} = boundingBox (Vector2 lx lz)
--                                                       (Vector2 hx hz)
--        where bound3 = minBBox poly
--              Vector3 lx _ lz = lowBound bound3
--              Vector3 hx _ hz = lowBound bound3
--
--instance Boundable CityObject 3 GLfloat where
--    minBBox Building{ objPolygon = poly} = boundPair bb zbound
--        where bb = minBBox poly
--              Vector3 lx _ lz = lowBound bb
--              Vector3 hx _ hz = highBound bb
--              zbound = boundingBox (Vector3 lx 0 lz) (Vector3 hx 0 hz)


----------------------------------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------------------------------

-- create list of points, normals, texture coords for a wall out of two points
buildWall :: Vector3 GLfloat -- first point
          -> Vector3 GLfloat -- second point
          -> Bool -- if the normal is correct
          -> (MutableByteArray (PrimState (ST s)), Int, MutableByteArray (PrimState (ST s)), Int)
          -> ST s (MutableByteArray (PrimState (ST s)), Int, MutableByteArray (PrimState (ST s)), Int)
buildWall p1@(Vector3 x1 y1 z1) p2@(Vector3 x2 y2 z2) c (arr, i, ix, j) = do
    -- write points
    writeByteArray arr  i    (p1,norm,Vector2 m h1)
    writeByteArray arr (i+1) (p2,norm,Vector2 0 h2)
    writeByteArray arr (i+2) (Vector3 x2 0 z2,norm, Vector2 0 0 :: Vector2 GLushort)
    writeByteArray arr (i+3) (Vector3 x1 0 z1,norm, Vector2 m 0)
    -- write indices
    writeByteArray ix  j     i'
    writeByteArray ix (j+1) (i'+1)
    writeByteArray ix (j+2) (i'+2)
    writeByteArray ix (j+3)  i'
    writeByteArray ix (j+4) (i'+2)
    writeByteArray ix (j+5) (i'+3)
    -- return modified
    return (arr, i+4, ix, j+6)
    where (h1,h2) = if y1 > y2 then (m, round $ y2/y1 * fromIntegral m)
                               else (round $ y1/y2 * fromIntegral m, m) :: (GLushort, GLushort)
          m = 65535
          i' = fromIntegral i :: GLushort
          nr' = Vector3 0 1 0 `cross` (p2.-p1)
          norm = fmap (round . max (-128) . min 127)
            $ ((if c then 1 else -1) * 127 / normL2 nr') ..* nr' :: Vector3 GLbyte


buildRoof :: [Vector3 GLfloat]
          -> (MutableByteArray (PrimState (ST s)), Int, MutableByteArray (PrimState (ST s)), Int)
          -> ST s (MutableByteArray (PrimState (ST s)), Int, MutableByteArray (PrimState (ST s)), Int)
buildRoof pts (arr, i, ix, j) = do
    il <- M.foldM (\ii x -> writeByteArray arr ii x >> return (ii+1)) i rpts
    jl <- M.foldM (\jj x -> writeByteArray ix jj (fromIntegral x :: GLushort) >> return (jj+1))
        j (triangulate3 (SimplePolygon pts) >>= (\(i0,i1,i2) -> [i+i0-1,i+i1-1,i+i2-1]))
    -- return modified
    return (arr, il, ix, jl)
    where norm :: Vector3 GLfloat -> Vector3 GLbyte
          norm v = fmap (round . max (-128) . min 127)
                . (*..127) . unit $ v `cross` Vector3 0 1 0 `cross` v
          rpts = zip (ptl:pts) pts >>= \(p1,p2) -> let no = norm (p2 .- p1)
                                                   in [(p2,no, Vector2 0 0 :: Vector2 GLushort)]
          ptl = last pts


fillBuildingArrays :: [Vector3 GLfloat]
                   -> PointData Vertex20CNT GLushort
fillBuildingArrays pts = runST $ do
    marr <- newByteArray (psize * sizeOf (undefined :: Vertex20CNT))
    miarr <- newByteArray (isize * sizeOf (undefined :: GLushort))
    s1 <- M.foldM (\s (p1,p2) -> buildWall p1 p2 clockwise s) (marr, 0, miarr, 0) $ zip (last pts:pts) pts
    (marr', _, miarr', _) <- buildRoof pts s1
    arr <- unsafeFreezeByteArray marr'
    iarr <- unsafeFreezeByteArray miarr'
    return $ PointData arr (fromIntegral psize) iarr (fromIntegral isize)
    where n = length pts
          psize = n*4 -- walls
                  + n -- roof
          isize = n*6 -- walls
                  + (n-2)*3 -- roof
          clockwise = shoelace pts


-- | For simple polygons says for each point whether it is convex (True) or concave (False)
shoelace :: (Fractional x, Ord x) => [Vector3 x] -> Bool
shoelace pts = area > 0
    where area = sum $ f pts (head pts) -- area of whole thing (shoelace)
          f (Vector3 x1 _ z1 :xs@(Vector3 x2 _ z2 :_)) l = x1*z2 - x2*z1 : f xs l
          f [Vector3 x1 _ z1] (Vector3 x2 _ z2) = [x1*z2 - x2*z1]
          f [] _ = []
