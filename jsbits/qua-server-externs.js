/** Registers two callbacks; comes from Handler.Home.PanelGeometry.
 *  onSuccess :: JSON -> IO ()
 *  onFailure :: JSString -> IO ()
 *  return :: IO ()
 */
function registerLoadingFile(onSuccess, onFailure) {}


/** Registers one callback; comes from Handler.Home.PanelGeometry.
 *  onClick :: IO ()
 *  return :: IO ()
 */
function registerClearGeometry(onClick) {}

/** Call this when scenarios are parsed; comes from Handler.Home.PanelGeometry.
 *  xs :: [{ScenarioDescription, as-is}]
 *  return :: IO ()
 */
function displayScenarios(xs) {}

/** Registers one callback; comes from Handler.Home.PanelGeometry.
 *  h :: ScID -> JSString -> IO ()
 *  return :: IO ()
 */
function registerAskLuciForScenario(sendMsg) {}

/** Registers one callback; comes from Handler.Home.PanelGeometry.
 *  onClick :: IO ()
 *  return :: IO ()
 */
function registerGetScenarioList(onClick) {}

/** Registers one callback; comes from Handler.Home.PanelServices.
 *  onClick :: JSString -> IO () -- address of websocket host
 *  return :: IO ()
 */
function registerUserConnectToLuci(onClick){}

/** Display "luci connected message"; comes from Handler.Home.PanelServices.
 *  connectedHost :: JSString -- address of websocket host
 *  return :: IO ()
 */
function showLuciConnected(connectedHost){}

/** Display "luci connecting message"; comes from Handler.Home.PanelServices.
 *  connectedHost :: JSString -- address of websocket host
 *  return :: IO ()
 */
function showLuciConnecting(connectedHost){}

/** Display "connect to luci" form; comes from Handler.Home.PanelServices.
 *  defaultHost :: JSString -- default address of websocket host
 *  return :: IO ()
 */
function showLuciConnectForm(defaultHost){}

/** Registers one callback; comes from Handler.Home.PanelGeometry.
 *  sendMsg :: JSString -> IO ()
 *  return :: IO ()
 */
function registerSaveScenario(sendMsg) {}

/** call it to setup scenario buttons state; comes from Handler.Home.PanelGeometry.
 *  showButton :: Bool -- whether to show "save scenario" button
 *  scName :: JSString -- name of the scenario displayed on a panel
 *  return :: IO ()
 */

function toggleSaveScenarioButton(showButton, scName) {}

/** Registers one callback; comes from Handler.Home.UIButtons.
 *  onClick :: IO ()
 *  return :: IO ()
 */
function registerServiceClear(onClick) {}

/** Registers one callback; comes from Handler.Home.UIButtons.
 *  onClick :: IO ()
 *  return :: IO ()
 */
function registerServiceRun(onClick) {}

/** Shows or hides button "clear"; comes from Handler.Home.UIButtons.
 *  state :: Bool
 *  return :: IO ()
 */
function toggleServiceClear(state) {}


/** Registers one callback; comes from Handler.Home.PanelInfo.
 *  f :: JSString -> IO ()
 *  return :: IO ()
 */
function registerColorizeProperty(f) {}

/** Show info (pairs of key-value); comes from Handler.Home.PanelInfo.
 *  obj :: Object -- all property names and values inside an object
 *  return :: IO ()
 */
function showInfo(obj) {}

/** Registers one callback; comes from Handler.Home.UIButtons.
*  onClick :: (submitUrl -> FeatureCollection -> Image -> IO ()) -> IO ()
*  return :: IO ()
*/
function registerSubmit(onClick) {}
/** Registers one callback; comes from Handler.Home.UIButtons.
 *  onClick :: IO ()
 *  return :: IO ()
 */
function registerResetCamera(onClick) {}

/** Registers one callback; comes from Handler.Home.PanelServices.
 *  refreshSLRun :: IO ()
 *  return :: IO ()
 */
function registerRefreshServiceList(refreshSLRun) {}

/** Updates visible service list; comes from Handler.Home.PanelServices.
 *  xs :: [ServiceName]
 *  return :: IO ()
 */
function updateServiceNames(xs) {}

/** Registers one callback; comes from Handler.Home.PanelServices.
 *  setActiveService :: String -> IO ()
 *  return :: IO ()
 */
function registerSetActiveService(setActiveService) {}

/** Registers one callback; comes from Handler.Home.PanelServices.
 *  updateParam :: String -> JSVal -> IO ()
 *  return :: IO ()
 */
function registerUpdateSParamValue(updateParam) {}
