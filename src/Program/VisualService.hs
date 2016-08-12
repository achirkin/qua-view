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
  , parseResult, makeRunRequest, ColorPalette (..), makeColors
  ) where

import Control.Arrow (Arrow(..))
import Data.Time (UTCTime,secondsToDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Monoid
import Data.Coerce (coerce)

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


-- | Encapsulate runtime service parameters
data VisualService = VisualService
  { vsName :: !ServiceName
  }

--data ServiceInputDescription
--  =


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

--MessageAttachment = MessageAttachment
--  { maFormat   :: !JSString
--  , maLength   :: !Int
--  , maMD5      :: !JSString
--  , maPosition :: !Int
--  }

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
makeColors (LinearPalette p0 p1) sf = JS.fromJSArray . JS.map f $ normalized sf
    where f x = round $ (1-x) * v p0
                      +    x  * v p1
makeColors (Bezier2Palette p0 p1 p2) sf = JS.fromJSArray . JS.map f $ normalized sf
    where f x | y <- 1-x = round $   y*y * v p0
                                 + 2*x*y * v p1
                                 +   x*x * v p2
makeColors (Bezier3Palette p0 p1 p2 p3) sf = JS.fromJSArray . JS.map f $ normalized sf
    where f x | y <- 1-x = round $   y*y*y * v p0
                                 + 3*x*y*y * v p1
                                 + 3*x*x*y * v p2
                                 +   x*x*x * v p3


-- helpers

v :: Vector4 GLubyte -> Vector4 GLfloat
v = coerce


foreign import javascript safe "var bs = Array.prototype.reduce.call($1, function(a,x){return [Math.min(a[0],x),Math.max(a[0],x)];}, [Infinity,-Infinity]);\
      \ var xspan = Math.max(bs[1] - bs[0], 0.000001), f = function(e) {return Math.min(1,Math.max(0,(e - bs[0]) / xspan));}, t = 0; \
      \ $r = Array.prototype.map.call( $1, function(n){t = f(n); return [t,t,t,t];});" normalized :: JSTA.TypedArray GLfloat -> PS.PointArray 4 GLfloat




