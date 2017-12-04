{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Program.MapTiles
    ( downloadMapTiles
    ) where


import Commons
import Numeric.DataFrame
import Control.Lens
import JavaScript.WebGL
import qualified Data.JSString as JSString
import Control.Concurrent (forkIO)
import Reflex
import Unsafe.Coerce (unsafeCoerce)

import SmallGL

import Model.Scenario
import Model.GeoJSON.Coordinates.Wgs84
import Program.Scenario


data GroundMapView = GroundMapView
    { gmvLonLatCenter :: !Vec2f
    , gmvLocalCenter  :: !Vec4f
    , gmvTileCenter   :: !(Int,Int)
    , gmvCellWidth    :: !GLfloat
    , gmvZoomLevel    :: !Int
    , gmvMapUrl       :: !JSString
    , gmvCallback     :: !((DataFrame Float '[4,4], TexImageSource) -> IO ())
    }


downloadMapTiles :: ( Reflex t, MonadIO m, TriggerEvent t m
                    , PerformEvent t m, MonadIO (Performable m))
                 => Behavior t Scenario
                 -> QuaViewT Writing t m ()
downloadMapTiles scenarioB = do
    mapUpdatesE <- (mapUpdate <$> scenarioB <@>) <$> askEvent (ScenarioUpdate ScenarioStateUpdatedOut)

    -- set new opacity levels if needed
    registerEvent (SmallGLInput SetMapTileOpacity) $ fmapMaybe getOpacity  mapUpdatesE

    (addMapTileE, addMapTileCbk) <- newTriggerEvent

    performEvent_ $ liftIO . downloadTiles addMapTileCbk <$> mapUpdatesE

    registerEvent (SmallGLInput AddMapTileToRendering) addMapTileE
  where
    mapUpdate scenario viewS
      = if scenario^.useMapLayer
        then (,,,,) (scenario^.mapZoomLevel)
                    (scenario^.mapOpacity)
                    (scenario^.mapUrl)
                    (viewS^.clippingDist)
                  <$> scenario^.geoLoc
                         else Nothing
    getOpacity (Just (_,o,_,_,_)) = Just (realToFrac o)
    getOpacity Nothing = Nothing


downloadTiles :: ((DataFrame Float '[4,4], TexImageSource) -> IO ())
              -> Maybe (Int, Double, JSString, Float, (Double,Double,Double))
              -> IO ()
downloadTiles _ Nothing = return ()
downloadTiles cbk (Just (zoomLvl, _, mUrl, viewDist, (lon,lat,_)) ) =
    mapM_ (createMapTilesAsync gmv)
      [[ (xtile0+i,ytile0+j)
        | i' <- [0 .. nTiles -1], i <- [i', -i'-1]]
        | j' <- [0 .. nTiles -1], j <- [j', -j'-1]]
  where
    gmv = GroundMapView
      { gmvLonLatCenter = vec2 lon0 lat0
      , gmvLocalCenter  = pos0 <:> vec2 0 1
      , gmvTileCenter   = (xtile0,ytile0)
      , gmvCellWidth    = tileWidth
      , gmvZoomLevel    = zoomLvl
      , gmvMapUrl       = mUrl
      , gmvCallback     = cbk
      }
    -- set up the center point to real center of the tile
    (xtile0,ytile0) = zoomLonLat2xy zoomLvl (realToFrac lon, realToFrac lat)
    (lon0, lat0) = zoomXY2LonLat zoomLvl (xtile0,ytile0)
    (lon1, lat1) = zoomXY2LonLat zoomLvl (xtile0+1,ytile0+1)

    -- a transform from WGS'84 to our local coordinates
    wgs2metric = wgs84ToMetric (vec2 (realToFrac lon) (realToFrac lat))

    -- get center positions in local metric system
    pos0 = wgs2metric (vec2 lon0 lat0)
    pos1 = wgs2metric (vec2 lon1 lat1)
    tileWidth = unScalar $ normL2 (pos1 - pos0) / sqrt 2

    nTiles = min 25 . max 3 . ceiling $ viewDist / tileWidth * 0.8


createMapTilesAsync :: GroundMapView
                    -> [(Int, Int)] -- ^ tile x and y
                    -> IO ()
createMapTilesAsync gmv = void . forkIO . mapM_ (createMapTile gmv)


createMapTile :: GroundMapView
              -> (Int, Int) -- ^ tile x and y
              -> IO ()
createMapTile GroundMapView {..} tilexy@(x,y)
    = createTex gmvMapUrl gmvZoomLevel tilexy >>= mapM_ (gmvCallback . (,) df)
  where
    df = groundPoints cellCoord gmvCellWidth
    cellCoord = gmvLocalCenter + vec4 xx yy 0 0
    xx = (gmvCellWidth *) . fromIntegral $ x - fst gmvTileCenter
    yy = (gmvCellWidth *) . fromIntegral $ snd gmvTileCenter - y



groundPoints :: Vec4f -> GLfloat -> DataFrame GLfloat '[4,4]
groundPoints p side
  =    (p + vec4 0 (-side) 0 0)
  <::> (p + vec4 side (-side) 0 0)
  <+:> (p + vec4 0 0 0 0)
  <+:> (p + vec4 side 0 0 0)




foreign import javascript interruptible
    "var tryDownload = function(attempt) {\
    \  if(attempt > 0){ \
    \    var osmImg = new Image(); \
    \    osmImg.addEventListener('load', function(){$c(osmImg);});\
    \    osmImg.addEventListener('error', function(){tryDownload(attempt-1);});\
    \    osmImg['crossOrigin'] = 'Anonymous'; \
    \    osmImg['src'] = $1; \
    \  } else { $c(null); }\
    \}; tryDownload(10); "
    js_createTex :: JSString -> IO (Nullable JSVal)


createTex :: JSString -> Int -> (Int,Int) -> IO (Maybe TexImageSource)
createTex urlPat z (xtile,ytile)
    = fmap unsafeCoerce . nullableToMaybe <$> js_createTex url
  where
    url = urlPat & JSString.replace "${z}" (toJSString $ show z)
                 & JSString.replace "${x}" (toJSString $ show xtile)
                 & JSString.replace "${y}" (toJSString $ show ytile)


zoomLonLat2xy :: Int -> (Float, Float) -> (Int, Int)
zoomLonLat2xy z (lon, lat) = (xtile, ytile)
    where
      n = 2 ^ z
      xtile = round $ n * ((lon + 180) / 360)
      ytile = round $ n * (1 - (log(tan(lat * pi / 180) + 1/cos(lat * pi / 180)) / pi)) / 2

zoomXY2LonLat :: Int -> (Int,Int) -> (Float, Float)
zoomXY2LonLat z (xtile, ytile) = (lon, lat)
    where
      n = 2 ^ z
      lon = fromIntegral xtile / n * 360.0 - 180.0
      lat = atan(sinh(pi * (1 - 2 * fromIntegral ytile / n))) * 180 / pi



