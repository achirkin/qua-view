{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.View.GroundMapView
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.View.GroundMapView
    ( GroundMapView (..)
    , createGroundMapView, drawGroundMapView
    ) where

import JsHs.Types
import JsHs.WebGL
import JsHs.JSString
import Control.Concurrent.Chan
import Data.List (sortOn)
import Data.IORef
import qualified Data.JSString as JSString
import Control.Concurrent (forkIO)
import Data.Function ((&))
import Control.Monad ((<=<), forever, join)
import Data.Geometry
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Program.View

import GHCJS.Concurrent
import SmallGL.WritableVectors
import JsHs.Array
import qualified Data.Geometry.Structure.PointSet as PS

viewDistance :: GLfloat
viewDistance = 200

data GroundMapCell = GroundMapCell
    { gmcVertexBuffer :: !WebGLBuffer
    , gmcMapTexture   :: !WebGLTexture
    }


data GroundMapView = GroundMapView
    { gmvLonLatCenter :: !(Vector 2 GLfloat)
    , gmvLocalCenter  :: !(Vector 3 GLfloat)
    , gmvTileCenter   ::  !(Int,Int)
    , gmvCellWidth    :: !GLfloat
    , gmvZoomLevel    :: !Int
    , gmvTiles        :: !(Map (Int,Int) (IORef (Maybe GroundMapCell)))
    , gmvMapUrl       :: !JSString
    }

createGroundMapView :: WebGLRenderingContext
                    -> JSString
                    -> Int -- ^ zoom level
                    -> (GLfloat, Vector2 GLfloat) -- ^ view scale and shift
                    -> Vector 3 GLfloat -- ^ longitude, latitude, altitude of origin
                    -> IO GroundMapView
createGroundMapView gl mapUrl zoomlvl (vscale, vshift) lonlatalt = do
    let gmv = GroundMapView (vector2 lon0 lat0) pos0 (xtile0,ytile0) tileWidth zoomlvl Map.empty mapUrl
    loadingchannel <- newChan
    tiles <- Map.fromList <$> mapM (\p -> (,) p <$> createGroundMapCell loadingchannel gl gmv p)
                ( sortOn (\(i,j) -> (i - xtile0)*(i - xtile0) + (j - ytile0)*(j - ytile0))
                         [(xtile0+i,ytile0+j) | i <- [- nTiles .. nTiles -1], j <- [- nTiles .. nTiles -1]]
                )
    _ <- forkIO . forever . Control.Monad.join $ readChan loadingchannel
    return $ gmv {gmvTiles = tiles}
  where
    (lon, lat, _) = unpackV3 lonlatalt
    -- set up the center point to real center of the tile
    (xtile0,ytile0) = zoomLonLat2xy zoomlvl (lon,lat)
    (lon0, lat0) = zoomXY2LonLat zoomlvl (xtile0,ytile0)
    (lon1, lat1) = zoomXY2LonLat zoomlvl (xtile0+1,ytile0+1)

    -- a transform from WGS'84 to our local coordinates
    wgs2metric x =  broadcastVector vscale *
         (js_useWGS84toUTMTransform (js_createWGS84toUTMTransform lon lat) x - resizeVector vshift)

    -- get center positions in local metric system
    pos0 = wgs2metric (vector3 lon0 lat0 0)
    pos1 = wgs2metric (vector3 lon1 lat1 0)
    tileWidth = normL2 (pos1 - pos0) / sqrt 2

    nTiles = ceiling $ viewDistance / tileWidth / 2



createGroundMapCell :: Chan (IO ())
                    -> WebGLRenderingContext
                    -> GroundMapView
                    -> (Int, Int) -- ^ tile x and y
                    -> IO (IORef (Maybe GroundMapCell))
createGroundMapCell loadingchannel gl GroundMapView{..} tilexy@(x,y) = do
    buf <- createBuffer gl
    bindBuffer gl gl_ARRAY_BUFFER buf
    bufferData gl gl_ARRAY_BUFFER arrayBuffer gl_STATIC_DRAW
    gmc <- newIORef Nothing
    _ <- forkIO $ do
       img <- createTex gmvMapUrl gmvZoomLevel tilexy
       writeChan loadingchannel . withoutPreemption $ do
         tex <- initTexture gl (Left img)
         writeIORef gmc . Just $ GroundMapCell buf tex
    return gmc
  where
    arrayBuffer = packPoints (groundPoints (gmvLocalCenter + vector3 xx yy 0) gmvCellWidth)
                              groundNormals
                              groundTexCoords
    xx = (gmvCellWidth *) . fromIntegral $ x - fst gmvTileCenter
    yy = (gmvCellWidth *) . fromIntegral $ snd gmvTileCenter - y



groundPoints :: Vector 3 GLfloat -> GLfloat -> PS.PointArray 3 GLfloat
groundPoints p side = fromList
  [ p + vector3 0 (-side) 0
  , p + vector3 side (-side) 0
  , p + vector3 0 0 0
  , p + vector3 side 0 0
  ]

groundNormals :: PS.PointArray 3 GLbyte
groundNormals = fromList
  [ vector3 0 0 maxBound
  , vector3 0 0 maxBound
  , vector3 0 0 maxBound
  , vector3 0 0 maxBound
  ]

groundTexCoords :: PS.PointArray 2 GLushort
groundTexCoords = fromList
  [ vector2 minBound minBound
  , vector2 maxBound minBound
  , vector2 minBound maxBound
  , vector2 maxBound maxBound
  ]


drawGroundMapView :: WebGLRenderingContext
                  -> (GLuint,GLuint,GLuint)
                  -> GroundMapView -> IO ()
drawGroundMapView gl locs gmv = do
    depthMask gl False
    mapM_ (drawGroundMapCell gl locs <=< readIORef) $ gmvTiles gmv
    depthMask gl True



drawGroundMapCell :: WebGLRenderingContext
                  -> (GLuint,GLuint,GLuint)
                  -> Maybe GroundMapCell -> IO ()
drawGroundMapCell _ _ Nothing = return ()
drawGroundMapCell gl (ploc,_,tloc) (Just GroundMapCell {..}) = do
    bindTexture gl gl_TEXTURE_2D gmcMapTexture
    bindBuffer gl gl_ARRAY_BUFFER gmcVertexBuffer
    vertexAttribPointer gl ploc 3 gl_FLOAT False 20 0
    --vertexAttribPointer gl nloc 3 gl_BYTE True 20 12
    vertexAttribPointer gl tloc 2 gl_UNSIGNED_SHORT True 20 16
    drawArrays gl gl_TRIANGLE_STRIP 0 4




foreign import javascript interruptible
    "var osmImg = new Image(); osmImg.addEventListener('load', function(){$c(osmImg)}); osmImg.crossOrigin = 'anonymous'; osmImg.src = $1;"
    js_createTex :: JSString -> IO TexImageSource


createTex :: JSString -> Int -> (Int,Int) -> IO TexImageSource
createTex urlPat zoom (xtile,ytile)
   = js_createTex
   $ urlPat
   & JSString.replace "${z}" (pack $ show zoom)
   & JSString.replace "${x}" (pack $ show xtile)
   & JSString.replace "${y}" (pack $ show ytile)


zoomLonLat2xy :: Int -> (Float, Float) -> (Int, Int)
zoomLonLat2xy zoom (lon, lat) = (xtile, ytile)
    where
      n = 2 ^ zoom
      xtile = round $ n * ((lon + 180) / 360)
      ytile = round $ n * (1 - (log(tan(lat * pi / 180) + 1/cos(lat * pi / 180)) / pi)) / 2

zoomXY2LonLat :: Int -> (Int,Int) -> (Float, Float)
zoomXY2LonLat zoom (xtile, ytile) = (lon, lat)
    where
      n = 2 ^ zoom
      lon = fromIntegral xtile / n * 360.0 - 180.0
      lat = atan(sinh(pi * (1 - 2 * fromIntegral ytile / n))) * 180 / pi



foreign import javascript unsafe "gm$createWGS84toUTMTransform($1, $2)"
    js_createWGS84toUTMTransform :: Float -> Float -> JSVal

foreign import javascript unsafe "$1($2)"
    js_useWGS84toUTMTransform :: JSVal -> Vector n GLfloat -> Vector n GLfloat



