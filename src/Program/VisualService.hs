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
  , parseResult, runVService, ColorPalette (..), makeColors, applyPalette
  , VisualServiceMode (..), VSManager (), runQuaServiceList
  , ServiceParameter (..)
  , vsManagerBehavior
  ) where

import Control.Arrow (Arrow(..))
import Data.Time (UTCTime,secondsToDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Monoid
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HashMap

import Data.Geometry (Vector4)
import Data.Default.Class
import Data.List (foldl')
import qualified Data.Geometry.Structure.PointSet as PS

import JsHs.Types (JSVal)
import JsHs.JSString (JSString, pack)
import JsHs.LikeJS.Class (LikeJS(..))
--import JsHs.Types.Prim (jsNull)
import qualified JsHs.Array as JS
import qualified JsHs.TypedArray as JSTA
import JsHs.WebGL (GLfloat, GLubyte)
import JsHs.Useful

import Program.Types
import Program.Controllers.LuciClient
import Program.Settings

import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

import Control.Lens

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
data VSManager = VSManager
  { _registeredServices :: !(HashMap.HashMap ServiceName VisualService)
  , _activeService      :: !(Maybe ServiceName)
  , resultEventHandler  :: !(Handler VisualServiceResult)
  }

registeredServices :: Lens' VSManager (HashMap.HashMap ServiceName VisualService)
registeredServices m t = (\x -> t{_registeredServices = x }) <$> m (_registeredServices t)

activeService :: Lens' VSManager (Maybe ServiceName)
activeService m t = (\x -> t{_activeService = x }) <$> m (_activeService t)


-- | Possible optional parameters
data ServiceParameter
  = ServiceParameterString JSString StringEnum
  | ServiceParameterInt    JSString RangedInt
  | ServicePapameterFloat  JSString RangedFloat
  | ServiceParameterBool   JSString Bool


spToJS :: ServiceParameter -> (JSString, JSVal)
spToJS (ServiceParameterString pname v) = (pname, JS.asJSVal $ seVal v)
spToJS (ServiceParameterInt    pname v) = (pname, JS.asJSVal $ riVal v)
spToJS (ServicePapameterFloat  pname v) = (pname, JS.asJSVal $ rfVal v)
spToJS (ServiceParameterBool   pname v) = (pname, JS.asJSVal v)


spName :: Lens' ServiceParameter JSString
spName m t = mv t <$> m (get t)
  where
    mv (ServiceParameterString _ x) n = ServiceParameterString n x
    mv (ServiceParameterInt    _ x) n = ServiceParameterInt    n x
    mv (ServicePapameterFloat  _ x) n = ServicePapameterFloat  n x
    mv (ServiceParameterBool   _ x) n = ServiceParameterBool   n x
    get (ServiceParameterString n _) = n
    get (ServiceParameterInt    n _) = n
    get (ServicePapameterFloat  n _) = n
    get (ServiceParameterBool   n _) = n

--newtype LuciResultServiceList = ServiceList (JS.Array JSString)

-- | Manage service parameters and executions.
--   Returns behavior object and an event with generic service results.
--   The event should fire whenever service results come from luci (for any service).
vsManagerBehavior :: Event ServiceName
                     -- ^ When user selects a service, it becomes active
                  -> Event [ServiceParameter]
                     -- ^ Change or set optional parameters for visual services
                  -> Event LuciResultServiceList
                     -- ^ Update list of available services
                  -> MomentIO (Behavior VSManager, Event VisualServiceResult)
vsManagerBehavior selectedServiceE changeSParamsE refreshSListE = do
    (resultE, resultH) <- newEvent
    manB <- accumB (VSManager HashMap.empty Nothing resultH)
                       (unions [ set activeService . Just <$> selectedServiceE
                               , over registeredServices . updateServiceMap <$> refreshSListE
                               , (\pams man -> over registeredServices
                                                ( alterService (_activeService man)
                                                    (\s -> s{_vsParams = changeServiceParams pams $ _vsParams s})
                                                ) man
                                 ) <$> changeSParamsE
                               ])
    return (manB, resultE)
  where
    updateServiceMap :: LuciResultServiceList -> HashMap.HashMap ServiceName VisualService
                                              -> HashMap.HashMap ServiceName VisualService
    updateServiceMap (ServiceList jsarray) m = foldl'
        (\nm x -> let s = ServiceName x
                  in case HashMap.lookup s m of
            Nothing -> HashMap.insert s (VisualService s [] []) nm
            Just vs -> HashMap.insert s vs nm
        ) HashMap.empty (JS.toList jsarray)
    changeServiceParams :: [ServiceParameter] -> [ServiceParameter] -> [ServiceParameter]
    changeServiceParams newpams oldpams = foldl' changeOrAdd  oldpams newpams
    changeOrAdd (x:xs) y | view spName y == view spName x = y:xs
                         | otherwise                      = changeOrAdd xs y
    changeOrAdd [] y = [y]
    alterService :: Maybe ServiceName -> (VisualService -> VisualService)
                                      -> HashMap.HashMap ServiceName VisualService
                                      -> HashMap.HashMap ServiceName VisualService
    alterService Nothing _ = id
    alterService (Just n) f = HashMap.adjust f n




--foldl :: ( LikeJSArray tt t
--           , LikeJS ta a)
--        => (a -> ArrayElem t -> a)
--        -> a -> t -> a
--foldl f x0 arr
--vsManagerBehavior luciClientB serviceRunsE

--vsManagerBehavior :: Event ServiceName
--                     -- ^ A service is selected in the viewer
--                  -> Event LuciResultServiceList
--                     -- ^ Service list is updated
--                  -> MomentIO (Behavior VSManager)
--vsManagerBehavior setActiveE updateSLE = do
--
--    manB <- accumB  (VSManager HashMap.empty) serviceListUpdatesE
--    let registeredSRE = filterJust $ filterSR <$> manB <@> serviceResultE
--    return (manB, registeredSRE)
--  where
--    updateActiveNameE = (\n m -> m{activeService = n}) <$> setActiveE
--    updateListE = <$> updateSLE
--    (serviceListE, serviceResultE) = split $ nameFilterServices <$> serviceFinE
--    filterSR _ (_, VisualServiceResultUnknown _ _) = Nothing
--    filterSR VSManager{registeredServices = rs} (s, rez) | HashMap.member (vsName s) rs = Just rez
--                                                         | otherwise                    = Nothing
--    serviceListUpdatesE = (\(ServiceList ls) vs -> vs{ registeredServices = HashMap.fromList
--                                                                          . map ((\n -> (n,VisualService n)) . ServiceName)
--                                                                          $ JS.toList ls
--                                                     } ) <$> serviceListE

-- | Encapsulate runtime service parameters
data VisualService
  = VisualService
    { _vsName   :: !ServiceName
      -- ^ serviceName in Luci
    , _vsModes  :: ![VisualServiceMode]
      -- ^ supported evaluation modes
    , _vsParams :: ![ServiceParameter]
      -- ^ optional service parameters
    }

vsName :: Lens' VisualService ServiceName
vsName m t = (\x -> t{_vsName = x }) <$> m (_vsName t)

vsModes :: Traversal' VisualService VisualServiceMode
vsModes f (VisualService n m p) = flip (VisualService n) p <$> traverse f m

vsParams :: Traversal' VisualService ServiceParameter
vsParams f (VisualService n m p) = VisualService n m <$> traverse f p


-- | Result of a service execution
data VisualServiceResult
  = VisualServiceResultPoints !JSString !(JSTA.TypedArray GLfloat)
  | VisualServiceResultObjects !JSString !(JSTA.TypedArray Int) !(JSTA.TypedArray GLfloat)
  | VisualServiceResultScenario !JSString !GLfloat
  | VisualServiceResultNew !ScenarioId !UTCTime !UTCTime
  | VisualServiceResultUnknown !JSVal !JSString

-- | Request of a service to run
data VisualServiceRun
  = VisualServiceRunPoints !ScenarioId !(JSTA.TypedArray GLfloat)
  | VisualServiceRunObjects !ScenarioId
  | VisualServiceRunScenario !ScenarioId
  | VisualServiceRunNew !(Maybe ScenarioId)


runVService :: Behavior VSManager -> Behavior LuciClient -> Event VisualServiceRun -> MomentIO ()
runVService vsManB lcB pamsE = do
    responseE <- runHelper lcB (fmap toPams . filterJust $ getServRun <$> vsManB <@> pamsE)
    reactimate $ onResponse <$> vsManB <@> responseE
  where
    getServRun man@VSManager{_activeService = Just sname} run = flip (,) run <$> HashMap.lookup sname (_registeredServices man)
    getServRun _ _ = Nothing
    toPams (service, VisualServiceRunPoints scid points) =
        ( _vsName service
        ,    ("ScID", asJSVal scid)
          :  ("points", asJSVal $ makeAttDesc 1 "Float32x3Array" ab)
          :  ("mode", asJSVal VS_POINTS)
          :  map spToJS (_vsParams service)
        , [ab]
        ) where ab = JSTA.arrayBuffer points
    toPams (service, VisualServiceRunObjects scid) =
        ( _vsName service
        , ("ScID", asJSVal scid):("mode", asJSVal VS_OBJECTS) : map spToJS (_vsParams service)
        , []
        )
    toPams (service, VisualServiceRunScenario scid) =
        ( _vsName service
        , ("ScID", asJSVal scid):("mode", asJSVal VS_SCENARIO) : map spToJS (_vsParams service)
        , []
        )
    toPams (service, VisualServiceRunNew (Just scid)) =
        ( _vsName service
        , ("ScID", asJSVal scid):("mode", asJSVal VS_NEW) : map spToJS (_vsParams service)
        , []
        )
    toPams (service, VisualServiceRunNew Nothing) =
        ( _vsName service
        , ("mode", asJSVal VS_NEW) : map spToJS (_vsParams service)
        , []
        )
    onResponse :: VSManager -> ServiceResponse VisualServiceResult -> IO ()
    onResponse man (SRResult _ r _) = resultEventHandler man r
    onResponse man (SRProgress i p (Just r) _) = logText ("Visual service progress: " ++ show i  ++ " - " ++ show p) >> resultEventHandler man r
    onResponse _   (SRProgress i p Nothing _) = logText ("Visual service progress: " ++ show i  ++ " - " ++ show p)
    onResponse _   (SRError i s) = logText' $ "Visual service error [" <> pack (show i) <> "]: " <> s



runHelper :: Behavior LuciClient -> Event (ServiceName, [(JSString, JSVal)], [JSTA.ArrayBuffer]) -> MomentIO (Event (ServiceResponse VisualServiceResult))
runHelper lcB pams = fmap f <$> runService lcB pams
  where
    f (SRResult i r x) = SRResult i (parseResult r x) x
    f (SRProgress i p (Just r) x) = SRProgress i p (Just $ parseResult r x) x
    f (SRProgress i p Nothing x) = SRProgress i p Nothing x
    f (SRError i s) = SRError i s


--sndToJSVal :: LikeJS s a => [(JSString, a)] -> [(JSString, JSVal)]
--sndToJSVal = map (second asJSVal)

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




