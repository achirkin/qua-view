{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.VisualService
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-- Run Luci services using common interface
--
-----------------------------------------------------------------------------

module Program.VisualService
  ( VisualService (..), VisualServiceResult (..), VisualServiceRun (..)
  , parseResult, makeRunRequest, ColorPalette (..), makeColors, applyPalette
  , VisualServiceMode (..), VSManager (..), runQuaServiceList
  , vsManagerBehavior
  ) where

import Control.Arrow (Arrow(..))
import Data.Time (UTCTime,secondsToDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Monoid
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HashMap

import Data.Geometry (Vector4)
import qualified Data.Geometry.Structure.PointSet as PS

import JsHs.Types (JSVal)
import JsHs.JSString (JSString, pack)
import JsHs.LikeJS.Class (LikeJS(..))
import qualified JsHs.Array as JS
import qualified JsHs.TypedArray as JSTA
import JsHs.WebGL (GLfloat, GLubyte)

import Program.Types
import Program.Controllers.LuciClient
import Program.Settings

import Reactive.Banana.Combinators
--import Reactive.Banana.Frameworks

-- | Working mode of a service
data VisualServiceMode
  = VS_POINTS
  | VS_OBJECTS
  | VS_SCENARIO
  | VS_NEW
  | VS_UNKNOWN JSVal
instance LikeJS "String" VisualServiceMode where
  asLikeJS jsv = case asLikeJS jsv :: JSString of
                   "points"   -> VS_POINTS
                   "objects"  -> VS_OBJECTS
                   "scenario" -> VS_SCENARIO
                   "new"      -> VS_NEW
                   _          -> VS_UNKNOWN jsv
  asJSVal VS_POINTS      = asJSVal ("points" :: JSString)
  asJSVal VS_OBJECTS     = asJSVal ("objects" :: JSString)
  asJSVal VS_SCENARIO    = asJSVal ("scenario" :: JSString)
  asJSVal VS_NEW         = asJSVal ("new" :: JSString)
  asJSVal (VS_UNKNOWN s) = s

-- | Manage available visual services
newtype VSManager = VSManager
  { registeredServices :: HashMap.HashMap ServiceName VisualService
  }


vsManagerBehavior :: MonadMoment m
                  => Event (ServiceName, ServiceResult, JS.Array JSTA.ArrayBuffer)
                  -> m (Behavior VSManager, Event VisualServiceResult)
vsManagerBehavior serviceFinE = do
    manB <- accumB  (VSManager HashMap.empty) serviceListUpdatesE
    let registeredSRE = filterJust $ filterSR <$> manB <@> serviceResultE
    return (manB, registeredSRE)
  where
    nameFilterServices ("FilterServices", ServiceResult r, _) = Left (JS.asLikeJS r :: LuciResultServiceList)
    nameFilterServices (n,r,b) = Right (VisualService n, parseResult r b)
    (serviceListE, serviceResultE) = split $ nameFilterServices <$> serviceFinE
    filterSR _ (_, VisualServiceResultUnknown _ _) = Nothing
    filterSR VSManager{registeredServices = rs} (s, rez) | HashMap.member (vsName s) rs = Just rez
                                                         | otherwise                    = Nothing
    serviceListUpdatesE = (\(ServiceList ls) vs -> vs{ registeredServices = HashMap.fromList
                                                                          . map ((\n -> (n,VisualService n)) . ServiceName)
                                                                          $ JS.toList ls
                                                     } ) <$> serviceListE

-- | Encapsulate runtime service parameters
newtype VisualService = VisualService
  { vsName  :: ServiceName
  }


-- | Result of a service execution
data VisualServiceResult
  = VisualServiceResultPoints !JSString !(JSTA.TypedArray GLfloat)
  | VisualServiceResultObjects !JSString !(JSTA.TypedArray Int) !(JSTA.TypedArray GLfloat)
  | VisualServiceResultScenario !JSString !GLfloat
  | VisualServiceResultNew !ScenarioId !UTCTime !UTCTime
  | VisualServiceResultUnknown !JSVal !JSString

-- | Request of a service to run
data VisualServiceRun
  = VisualServiceRunPoints !ScenarioId ![(JSString, JSString)] ![(JSString, GLfloat)] ![(JSString, Bool)] !(JSTA.TypedArray GLfloat)
  | VisualServiceRunObjects !ScenarioId ![(JSString, JSString)] ![(JSString, GLfloat)] ![(JSString, Bool)]
  | VisualServiceRunScenario !ScenarioId ![(JSString, JSString)] ![(JSString, GLfloat)] ![(JSString, Bool)]
  | VisualServiceRunNew !(Maybe ScenarioId) ![(JSString, JSString)] ![(JSString, GLfloat)] ![(JSString, Bool)]


makeRunRequest :: VisualService -> VisualServiceRun -> LuciMessage
makeRunRequest s (VisualServiceRunPoints scid strings numbers bools points) = toLuciMessage
    ( MsgRun (vsName s) (("ScID", asJSVal scid)
                        :("points", asJSVal $ makeAttDesc 1 "Float32x3Array" ab)
                        :("mode", asJSVal VS_POINTS):sndToJSVal strings ++ sndToJSVal numbers ++ sndToJSVal bools))
    [ab]
  where
    ab = JSTA.arrayBuffer points
makeRunRequest s (VisualServiceRunObjects scid strings numbers bools) = toLuciMessage
  ( MsgRun (vsName s) (("ScID", asJSVal scid):("mode", asJSVal VS_OBJECTS):sndToJSVal strings ++ sndToJSVal numbers ++ sndToJSVal bools))
  []
makeRunRequest s (VisualServiceRunScenario scid strings numbers bools) = toLuciMessage
  ( MsgRun (vsName s) (("ScID", asJSVal scid):("mode", asJSVal VS_SCENARIO):sndToJSVal strings ++ sndToJSVal numbers ++ sndToJSVal bools))
  []
makeRunRequest s (VisualServiceRunNew (Just scid) strings numbers bools) = toLuciMessage
  ( MsgRun (vsName s) (("ScID", asJSVal scid):("mode", asJSVal VS_NEW):sndToJSVal strings ++ sndToJSVal numbers ++ sndToJSVal bools))
  []
makeRunRequest s (VisualServiceRunNew Nothing strings numbers bools) = toLuciMessage
  ( MsgRun (vsName s) (("mode", asJSVal VS_NEW):sndToJSVal strings ++ sndToJSVal numbers ++ sndToJSVal bools))
  []


sndToJSVal :: LikeJS s a => [(JSString, a)] -> [(JSString, JSVal)]
sndToJSVal = map (second asJSVal)

-- | Interpret Luci's ServiceResult as a result of computation of given VisualService
parseResult :: ServiceResult -> JS.Array JSTA.ArrayBuffer -> VisualServiceResult
parseResult(ServiceResult jsv) atts = case getProp "mode" jsv of
    Just VS_POINTS      -> parsePoints
    Just VS_OBJECTS     -> parseObjects
    Just VS_SCENARIO    -> parseScenario
    Just VS_NEW         -> parseNew
    Just (VS_UNKNOWN _) -> VisualServiceResultUnknown jsv "Unknown mode"
    Nothing             -> VisualServiceResultUnknown jsv "No mode specified"
  where
    parsePoints = case (,) <$> getProp "unit" jsv <*> getProp "values" jsv of
         Nothing -> VisualServiceResultUnknown jsv "No 'unit' or 'values' fields"
         Just (unit, MessageAttachment {maPosition = p}) ->
            if p > 0 && p <= JS.length atts
            then VisualServiceResultPoints unit (JSTA.arrayView $ atts JS.! (p-1))
            else VisualServiceResultUnknown jsv $ "Wrong attachment position (" <> pack (show p) <> ")"
    parseObjects = case (,,) <$> getProp "unit" jsv <*> getProp "geomIDs" jsv <*> getProp "values" jsv of
         Nothing -> VisualServiceResultUnknown jsv "No 'unit' or 'geomIDs' or 'values' fields"
         Just (unit, MessageAttachment {maPosition = p1} , MessageAttachment {maPosition = p2}) ->
            if p1 > 0 && p1 <= JS.length atts && p2 > 0 && p2 <= JS.length atts && p1 /= p2
            then VisualServiceResultObjects unit (JSTA.arrayView $ atts JS.! (p1-1)) (JSTA.arrayView $ atts JS.! (p2-1))
            else VisualServiceResultUnknown jsv $ "Wrong attachment positions (" <> pack (show p1) <> "," <> pack (show p2) <> ")"
    parseScenario = case (,) <$> getProp "unit" jsv <*> getProp "value" jsv of
         Nothing -> VisualServiceResultUnknown jsv "No 'unit' or 'values' fields"
         Just (unit, val) -> VisualServiceResultScenario unit val
    parseNew = case (,,) <$> getProp "ScID" jsv <*> getProp "timestamp_accessed" jsv  <*> getProp "timestamp_modified" jsv of
         Nothing -> VisualServiceResultUnknown jsv "No 'unit' or 'values' fields"
         Just (scid, access, modif) -> VisualServiceResultNew scid (tsToDate access) (tsToDate modif)
    tsToDate = posixSecondsToUTCTime . realToFrac . secondsToDiffTime







----------------------------------------------------------------------------------------------------
-- Visualization Grid
----------------------------------------------------------------------------------------------------



-- | Color range to describe color pallete
data ColorPalette = LinearPalette  !(Vector4 GLubyte) !(Vector4 GLubyte)
                  | Bezier2Palette !(Vector4 GLubyte) !(Vector4 GLubyte) !(Vector4 GLubyte)
                  | Bezier3Palette !(Vector4 GLubyte) !(Vector4 GLubyte) !(Vector4 GLubyte) !(Vector4 GLubyte)

-- | Generate list of colors
makeColors :: ColorPalette
           -> JSTA.TypedArray GLfloat
           -> PS.PointArray 4 GLubyte -- ^ set of values in RGBA form [0..255]
makeColors pal sf = applyPalette pal Nothing $ normalized sf


-- | Generate list of colors
applyPalette :: ColorPalette
             -> Maybe (Vector4 GLfloat, Vector4 GLubyte) -- ^ substitute a certain color
             -> PS.PointArray 4 GLfloat
             -> PS.PointArray 4 GLubyte -- ^ set of values in RGBA form [0..255]
applyPalette (LinearPalette p0 p1) Nothing sf = JS.fromJSArray . JS.map f $ sf
    where f x = round $ (1-x) * v p0
                      +    x  * v p1
applyPalette (Bezier2Palette p0 p1 p2) Nothing sf = JS.fromJSArray . JS.map f $ sf
    where f x | y <- 1-x = round $   y*y * v p0
                                 + 2*x*y * v p1
                                 +   x*x * v p2
applyPalette (Bezier3Palette p0 p1 p2 p3) Nothing sf = JS.fromJSArray . JS.map f $ sf
    where f x | y <- 1-x = round $   y*y*y * v p0
                                 + 3*x*y*y * v p1
                                 + 3*x*x*y * v p2
                                 +   x*x*x * v p3
applyPalette (LinearPalette p0 p1) (Just (i,o)) sf = JS.fromJSArray . JS.map f $ sf
    where f x | x == i    = o
              | otherwise = round $ (1-x) * v p0
                                  +    x  * v p1
applyPalette (Bezier2Palette p0 p1 p2) (Just (i,o)) sf = JS.fromJSArray . JS.map f $ sf
    where f x | x == i   = o
              | otherwise
              , y <- 1-x = round $   y*y * v p0
                                 + 2*x*y * v p1
                                 +   x*x * v p2
applyPalette (Bezier3Palette p0 p1 p2 p3) (Just (i,o)) sf = JS.fromJSArray . JS.map f $ sf
    where f x | x == i   = o
              | otherwise
              , y <- 1-x = round $   y*y*y * v p0
                                 + 3*x*y*y * v p1
                                 + 3*x*x*y * v p2
                                 +   x*x*x * v p3


-- helpers

v :: Vector4 GLubyte -> Vector4 GLfloat
v = coerce


foreign import javascript safe "gm$normalizeValues($1,0)" normalized :: JSTA.TypedArray GLfloat -> PS.PointArray 4 GLfloat




