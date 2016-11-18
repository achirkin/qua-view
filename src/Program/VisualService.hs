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
  , vsName, vsModes, vsParams
  , parseServiceInfoResult
  , drawParameters
  ) where

import Control.Arrow (Arrow(..))
import Data.Time (UTCTime,secondsToDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Monoid
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HashMap

import Data.Geometry (Vector4)
--import Data.Default.Class
import Data.List (foldl',sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Geometry.Structure.PointSet as PS

import JsHs.Types (JSVal)
import JsHs.JSString (JSString, pack)
import qualified JsHs.JSString as JSString
import JsHs.LikeJS.Class (LikeJS(..))
import JsHs.Types.Prim (jsNull)
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

--import JsHs.Debug

-- | Working mode of a service
data VisualServiceMode
  = VS_POINTS
  | VS_OBJECTS
  | VS_SCENARIO
  | VS_NEW
  | VS_UNKNOWN JSVal

instance Show VisualServiceMode where
  show VS_POINTS      = "VS_POINTS"
  show VS_OBJECTS     = "VS_OBJECTS"
  show VS_SCENARIO    = "VS_SCENARIO"
  show VS_NEW         = "VS_NEW"
  show (VS_UNKNOWN _) = "VS_UNKNOWN"

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
  = ServiceParameterString JSString JSString
  | ServiceParameterEnum   JSString StringEnum
  | ServiceParameterInt    JSString RangedInt
  | ServicePapameterFloat  JSString RangedFloat
  | ServiceParameterBool   JSString Bool
  deriving Show


spToJS :: ServiceParameter -> (JSString, JSVal)
spToJS (ServiceParameterString pname v) = (pname, JS.asJSVal v)
spToJS (ServiceParameterEnum   pname v) = (pname, JS.asJSVal $ seVal v)
spToJS (ServiceParameterInt    pname v) = (pname, JS.asJSVal $ riVal v)
spToJS (ServicePapameterFloat  pname v) = (pname, JS.asJSVal $ rfVal v)
spToJS (ServiceParameterBool   pname v) = (pname, JS.asJSVal v)


spName :: Lens' ServiceParameter JSString
spName m t = mv t <$> m (get t)
  where
    mv (ServiceParameterString _ x) n = ServiceParameterString n x
    mv (ServiceParameterEnum _   x) n = ServiceParameterEnum   n x
    mv (ServiceParameterInt    _ x) n = ServiceParameterInt    n x
    mv (ServicePapameterFloat  _ x) n = ServicePapameterFloat  n x
    mv (ServiceParameterBool   _ x) n = ServiceParameterBool   n x
    get (ServiceParameterString n _) = n
    get (ServiceParameterEnum   n _) = n
    get (ServiceParameterInt    n _) = n
    get (ServicePapameterFloat  n _) = n
    get (ServiceParameterBool   n _) = n


-- | Construct a ServiceParameter from Luci ServiceInfo part
spFromInfo :: JSString    -- ^ name of a parameter
           -> JSString    -- ^ type of a parameter
           -> Maybe JSVal -- ^ constraint
           -> Either JSString ServiceParameter
spFromInfo pname "number"   Nothing   = Right $ ServicePapameterFloat  pname 0
spFromInfo pname "string"   Nothing   = Right $ ServiceParameterString pname ""
spFromInfo pname "boolean"  Nothing   = Right $ ServiceParameterBool   pname False
spFromInfo pname "number"  (Just jsv) = Right $
    if isInt then ServiceParameterInt pname   $ case (getProp "min" jsv, getProp "max" jsv) of
               (Nothing, Nothing) -> RangedInt minBound maxBound . fromMaybe 0 $ getProp "def" jsv
               (Nothing, Just ma) -> RangedInt minBound ma       . fromMaybe ma $ getProp "def" jsv
               (Just mi, Nothing) -> RangedInt mi       maxBound . fromMaybe mi $ getProp "def" jsv
               (Just mi, Just ma) -> RangedInt mi       ma       . fromMaybe ((ma - mi) `div` 2) $ getProp "def" jsv
             else ServicePapameterFloat pname $ case (getProp "min" jsv, getProp "max" jsv) of
               (Nothing, Nothing) -> RangedFloat minv maxv . fromMaybe 0 $ getProp "def" jsv
               (Nothing, Just ma) -> RangedFloat minv ma   . fromMaybe ma $ getProp "def" jsv
               (Just mi, Nothing) -> RangedFloat mi   maxv . fromMaybe mi $ getProp "def" jsv
               (Just mi, Just ma) -> RangedFloat mi   ma   . fromMaybe ((ma - mi) / 2) $ getProp "def" jsv
  where
    isInt = Just True == getProp "integer" jsv
    maxv  = (2^)  . (+ (-3)) . snd $ floatRange (0::Float)
    minv  = (2^^) . (+3) . fst $ floatRange (0::Float)
spFromInfo pname "string"  (Just jsv) = case xs of
     x:_ -> Right $ ServiceParameterEnum pname $ StringEnum xs x
     _   -> Left $ "Constraint for parameter " <> pname <> " exists, but is empty."
  where
   xs = JS.asLikeJS jsv
spFromInfo pname "boolean"  (Just jsv) = Right $ ServiceParameterBool pname (Just True == getProp "def" jsv)
spFromInfo pname t _ = Left $ "Unknown type \"" <> t <> "\" for parameter " <> pname <> "."


vsFromInfo :: JSString -- ^ name of a service
           -> JSVal    -- ^ content of ServiceInfo["serviceName"]
           -> ( [JSString] , Maybe VisualService)
vsFromInfo sname jsv = if null modes then (["Cannot parse service info for \"" <> sname <> "\": no operation modes found."], Nothing)
   else (errs, Just $ VisualService (ServiceName sname) (map (JS.asLikeJS . JS.asJSVal) modes) pams)
  where
   (errs,pams) = paramsAndErrs $ mkParameters inputs constraints
   inputs = map (second JS.asLikeJS). sortOn fst . remPrefs . fromMaybe [] $ toProps <$> getProp "inputs" jsv :: [(JSString, JSString)]
   constraintsR = getProp "constraints" jsv :: Maybe JSVal
   modes = fromMaybe [] $ JS.toList <$> (constraintsR >>= getProp "mode" :: Maybe (JS.Array JSString)) :: [JSString]
   constraints = sortOn fst . remPrefs . fromMaybe [] $ toProps <$> constraintsR :: [(JSString, JSVal)]
   remPrefs [] = []
   remPrefs ((s,v):xs) = if JSString.isPrefixOf "OPT " s || JSString.isPrefixOf "XOR " s || JSString.isPrefixOf "ANY " s
                         then (JSString.drop 4 s, v):remPrefs xs
                         else (s,v):remPrefs xs
   -- create parameters from list of inputs and constraints
   mkParameters [] [] = []
   -- ignore certain types of inputs
   mkParameters (("mode",_):inpts) cs = mkParameters inpts cs
   mkParameters (("points",_):inpts) cs = mkParameters inpts cs
   mkParameters (("geomIDs",_):inpts) cs = mkParameters inpts cs
   mkParameters (("ScID",_):inpts) cs = mkParameters inpts cs
   -- ignore some constraints
   mkParameters inpts (("mode",_):cs) = mkParameters inpts cs
   -- actual merge
   mkParameters ((ikey,itype):inpts) ((ckey,cval):cs)
      | ikey == ckey = spFromInfo ikey itype (Just cval) : mkParameters inpts cs
      | ikey <  ckey = spFromInfo ikey itype Nothing     : mkParameters inpts ((ckey,cval):cs)
      | otherwise    = Left ("Found a parameter constraint without corresponding input field: " <> ckey)
                                                         : mkParameters ((ikey,itype):inpts) cs
   mkParameters ((ikey,itype):inpts) [] = spFromInfo ikey itype Nothing
                                                          : mkParameters inpts []
   mkParameters [] ((ckey,_):cs) = Left ("Found a parameter constraint without corresponding input field: " <> ckey)
                                                         : mkParameters [] cs
   paramsAndErrs [] = ([],[])
   paramsAndErrs (Left err : xs) = first (err:) $ paramsAndErrs xs
   paramsAndErrs (Right pa : xs) = second (pa:) $ paramsAndErrs xs


parseServiceInfoResult :: JSVal -> ([JSString], [VisualService])
parseServiceInfoResult jsv = merge $ map (uncurry vsFromInfo) $ toProps jsv
  where
    merge [] = ([],[])
    merge ((errs, Just s):xs) = (errs++) *** (s:) $ merge xs
    merge ((errs, Nothing):xs) = first (errs++) $ merge xs


--newtype LuciResultServiceList = ServiceList (JS.Array JSString)

-- | Manage service parameters and executions.
--   Returns behavior object and an event with generic service results.
--   The event should fire whenever service results come from luci (for any service).
vsManagerBehavior :: Event ServiceName
                     -- ^ When user selects a service, it becomes active
                  -> Event ServiceParameter
                     -- ^ Change or set optional parameters for visual services
                  -> Event LuciResultServiceList
                     -- ^ Update list of available services
                  -> Event VisualService
                     -- ^ Update a single service (set service parameters)
                  -> MomentIO (Behavior VSManager, Event VisualServiceResult)
vsManagerBehavior selectedServiceE changeSParamsE refreshSListE refreshServiceE = do
    (resultE, resultH) <- newEvent
    manB <- accumB (VSManager HashMap.empty Nothing resultH)
                       (unions [ set activeService . Just <$> selectedServiceE
                               , over registeredServices . updateServiceMap <$> refreshSListE
                               , (\pams man -> over registeredServices
                                                ( alterService (_activeService man)
                                                    (\s -> s{_vsParams = changeServiceParams [pams] $ _vsParams s})
                                                ) man
                                 ) <$> changeSParamsE
                               , (\vs man -> over registeredServices
                                                ( HashMap.adjust
                                                    (\s -> s{_vsParams = changeServiceParams (_vsParams vs) $ _vsParams s
                                                            ,_vsModes  = _vsModes vs
                                                            }) (_vsName vs)
                                                ) man
                                 ) <$> refreshServiceE
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
                         | otherwise                      = x:changeOrAdd xs y
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
data VisualService = VisualService
    { _vsName   :: !ServiceName
      -- ^ serviceName in Luci
    , _vsModes  :: ![VisualServiceMode]
      -- ^ supported evaluation modes
    , _vsParams :: ![ServiceParameter]
      -- ^ optional service parameters
    }
  deriving Show

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
        ( VS_POINTS
        , (_vsName service
          ,    ("ScID", asJSVal scid)
            :  ("points", asJSVal $ makeAttDesc 1 "Float32x3Array" ab)
            :  ("mode", asJSVal VS_POINTS)
            :  map spToJS (_vsParams service)
          , [ab]
          )
        ) where ab = JSTA.arrayBuffer points
    toPams (service, VisualServiceRunObjects scid) =
        ( VS_OBJECTS
        , (_vsName service
          , ("ScID", asJSVal scid):("mode", asJSVal VS_OBJECTS) : map spToJS (_vsParams service)
          , []
          )
        )
    toPams (service, VisualServiceRunScenario scid) =
        ( VS_SCENARIO
        , ( _vsName service
          , ("ScID", asJSVal scid):("mode", asJSVal VS_SCENARIO) : map spToJS (_vsParams service)
          , []
          )
        )
    toPams (service, VisualServiceRunNew (Just scid)) =
        ( VS_NEW
        , ( _vsName service
          , ("ScID", asJSVal scid):("mode", asJSVal VS_NEW) : map spToJS (_vsParams service)
          , []
          )
        )
    toPams (service, VisualServiceRunNew Nothing) =
        ( VS_NEW
        , ( _vsName service
          , ("mode", asJSVal VS_NEW) : map spToJS (_vsParams service)
          , []
          )
        )
    onResponse :: VSManager -> ServiceResponse VisualServiceResult -> IO ()
    onResponse man (SRResult i r _) = logText ("Visual service complete [" ++ show i ++ "]: 100%") >> resultEventHandler man r
    onResponse man (SRProgress i p (Just r) _) = logText ("Visual service progress [" ++ show i  ++ "]: " ++ show p) >> resultEventHandler man r
    onResponse _   (SRProgress i p Nothing _) = logText ("Visual service progress [" ++ show i  ++ "]: " ++ show p)
    onResponse _   (SRError i s) = logText' $ "Visual service error [" <> pack (show i) <> "]: " <> s



runHelper :: Behavior LuciClient -> Event (VisualServiceMode, (ServiceName, [(JSString, JSVal)], [JSTA.ArrayBuffer]))
          -> MomentIO (Event (ServiceResponse VisualServiceResult))
runHelper lcB pams = do
    modeB <- stepper (VS_UNKNOWN jsNull) (fst <$> pams)
    (\e -> f <$> modeB <@> e) <$> runService lcB (snd <$> pams)
  where
    f m (SRResult i r x) = SRResult i (parseResult m r x) x
    f m (SRProgress i p (Just r) x) = SRProgress i p (Just $ parseResult m r x) x
    f _ (SRProgress i p Nothing x) = SRProgress i p Nothing x
    f _ (SRError i s) = SRError i s


--sndToJSVal :: LikeJS s a => [(JSString, a)] -> [(JSString, JSVal)]
--sndToJSVal = map (second asJSVal)

-- | Interpret Luci's ServiceResult as a result of computation of given VisualService
parseResult :: VisualServiceMode -> ServiceResult -> JS.Array JSTA.ArrayBuffer -> VisualServiceResult
parseResult m (ServiceResult jsv) atts = case m of
    VS_POINTS      -> parsePoints
    VS_OBJECTS     -> parseObjects
    VS_SCENARIO    -> parseScenario
    VS_NEW         -> parseNew
    VS_UNKNOWN _   -> VisualServiceResultUnknown jsv "Unknown mode"
  where
    parsePoints = case (,) <$> getProp "units" jsv <*> getProp "values" jsv of
         Nothing -> VisualServiceResultUnknown jsv "No 'units' or 'values' fields"
         Just (unit, MessageAttachment {maPosition = p}) ->
            if p > 0 && p <= JS.length atts
            then VisualServiceResultPoints unit (JSTA.arrayView $ atts JS.! (p-1))
            else VisualServiceResultUnknown jsv $ "Wrong attachment position (" <> pack (show p) <> ")"
    -- not really correct values!
    parseObjects = case (,,) <$> getProp "units" jsv <*> getProp "geomIDs" jsv <*> getProp "values" jsv of
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
    where f x = round $ (1-x) * vvv p0
                      +    x  * vvv p1
applyPalette (Bezier2Palette p0 p1 p2) Nothing sf = JS.fromJSArray . JS.map f $ sf
    where f x | y <- 1-x = round $   y*y * vvv p0
                                 + 2*x*y * vvv p1
                                 +   x*x * vvv p2
applyPalette (Bezier3Palette p0 p1 p2 p3) Nothing sf = JS.fromJSArray . JS.map f $ sf
    where f x | y <- 1-x = round $   y*y*y * vvv p0
                                 + 3*x*y*y * vvv p1
                                 + 3*x*x*y * vvv p2
                                 +   x*x*x * vvv p3
applyPalette (LinearPalette p0 p1) (Just (i,o)) sf = JS.fromJSArray . JS.map f $ sf
    where f x | x == i    = o
              | otherwise = round $ (1-x) * vvv p0
                                  +    x  * vvv p1
applyPalette (Bezier2Palette p0 p1 p2) (Just (i,o)) sf = JS.fromJSArray . JS.map f $ sf
    where f x | x == i   = o
              | otherwise
              , y <- 1-x = round $   y*y * vvv p0
                                 + 2*x*y * vvv p1
                                 +   x*x * vvv p2
applyPalette (Bezier3Palette p0 p1 p2 p3) (Just (i,o)) sf = JS.fromJSArray . JS.map f $ sf
    where f x | x == i   = o
              | otherwise
              , y <- 1-x = round $   y*y*y * vvv p0
                                 + 3*x*y*y * vvv p1
                                 + 3*x*x*y * vvv p2
                                 +   x*x*x * vvv p3


-- helpers

vvv :: Vector4 GLubyte -> Vector4 GLfloat
vvv = coerce


foreign import javascript safe "gm$normalizeValues($1,0)" normalized :: JSTA.TypedArray GLfloat -> PS.PointArray 4 GLfloat





-- Visualize service parameters

--  = ServiceParameterString JSString JSString
--  | ServiceParameterEnum   JSString StringEnum
--  | ServiceParameterInt    JSString RangedInt
--  | ServicePapameterFloat  JSString RangedFloat
--  | ServiceParameterBool   JSString Bool


drawParameters :: VisualService -> JSString
drawParameters (VisualService _ _ pams) = JSString.concat
  [ "<table style=\"width: 98%\">"
  , JSString.concat (map (\s -> "<tr>" <> parameterWidget s <> "</tr>") pams)
  , "</table>"
  ]


parameterWidget :: ServiceParameter -> JSString
parameterWidget (ServiceParameterString pname pval) =
  "<td class=\"spKey\"><label for=\"" <> pname <> "\">" <> pname <> ": </label></td> \
  \<td class=\"spVal\"><input id=\"" <> pname <> "\" name=\"" <> pname <> "\" type=\"text\" value\"" <> pval <> "\"> \
  \</td>"
parameterWidget (ServiceParameterEnum pname (StringEnum options selected)) =
  "<td class=\"spKey\"><label for=\"" <> pname <> "\">" <> pname <> ": </label></td> \
  \<td class=\"spVal\"><select class=\"form-control\" id=\"" <> pname <> "\">"
  <> JSString.concat (map addOpt options) <>
  "</select>\
  \</td>"
  where
    addOpt opt = "<option value=\""<>opt<>"\" " <> (if selected == opt then "selected" else "") <> ">"<>opt<>"</option>"
parameterWidget (ServiceParameterInt pname (RangedInt minv maxv val)) =
  "<td class=\"spKey\"><label for=\"" <> pname <> "\">" <> pname <> ": </label></td> \
  \<td class=\"spVal\"><input id=\"" <> pname <> "\" name=\"" <> pname <> "\"\
    \ value=\""<>pack (show val) <>"\" min=\""<>pack (show minv) <>"\" max=\""<>pack (show maxv) <>"\" style=\"width: 6em;\" type=\"number\">"
  <>
  "</td>"
parameterWidget (ServicePapameterFloat pname (RangedFloat minv maxv val)) =
  "<td class=\"spKey\"><label for=\"" <> pname <> "\">" <> pname <> ": </label></td> \
  \<td class=\"spVal\"><input id=\"" <> pname <> "\" name=\"" <> pname <> "\"\
    \ value=\""<>pack (show val) <>"\" min=\""<>pack (show minv) <>"\" max=\""<>pack (show maxv) <>"\" style=\"width: 6em;\" type=\"number\">"
  <>
  "</td>"
parameterWidget (ServiceParameterBool pname checked) =
 "<td class=\"spKey\"><label for=\"" <> pname <> "\">" <> pname <> ": </label></td> \
 \<td class=\"spVal\"><div class=\"checkbox switch\"><label for=\"" <> pname <> "\">\
   \<input class=\"access-hide\" id=\"" <> pname <> "\" name=\"" <> pname <> "\" type=\"checkbox\" " <> (if checked then "checked=\"\"" else "") <> ">\
   \<span class=\"switch-toggle\"></span>\
 \</label></div></td>"

