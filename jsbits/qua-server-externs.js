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
 *  h :: ScID -> IO ()
 *  return :: IO ()
 */
function registerAskLuciForScenario(sendMsg) {}

/** Registers one callback; comes from Handler.Home.PanelGeometry.
 *  onClick :: IO ()
 *  return :: IO ()
 */
function registerGetScenarioList(onClick) {
  document.getElementById('#{rawJS browseScenarios}').addEventListener("click", onClick);
}

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

/** Display "connect to luci" form; comes from Handler.Home.PanelServices.
 *  defaultHost :: JSString -- default address of websocket host
 *  return :: IO ()
 */
function showLuciConnectForm(defaultHost){}
