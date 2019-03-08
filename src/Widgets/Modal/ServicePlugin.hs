{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.Modal.ServicePlugin
    ( popupServicePlugin
    ) where

import Reflex.Dom
import Text.Julius (julius, rawJS)
import Language.Haskell.TH
import Control.Lens
import GHC.Generics (Generic)

import QuaTypes
import Commons
import Model.Scenario.ServicePlugin
import Model.Scenario (Scenario)
import qualified Model.Scenario as Scenario
import Model.GeoJSON.Scenario () -- toJSON instance for Scenario
import JavaScript.JSON.Types.Instances
import Widgets.Commons
import Widgets.Generation
import Widgets.Modal


popupServicePlugin :: Reflex t
                   => ServicePlugin
                   -> Behavior t Scenario
                   -> Event t (ElementClick servicePluginButton)
                   -> QuaWidget t x ()
popupServicePlugin servicePlugin scB scContentPopupE = do
    settingsD <- quaSettings
    let scAndSettingsE = (,) <$> scB <*> current settingsD <@ scContentPopupE
        requestE = pushAlways getRequest scAndSettingsE

    targetAndReqE <- renderFormTarget servicePlugin requestE
    
    performEvent_
       $  (uncurry $ submitForm servicePlugin)
      <$> targetAndReqE

  where
    getRequest (sc, ss)
      = liftIO $ case ( getSubmissionGeometryUrl ss
                      , isViewerOnly $ permissions ss ) of
          (Just url, True ) -> pure $ Url url
          _                 -> Geom <$> jsonStringify (toJSON sc)


type FormTarget = JSString
data View = NewTab | Modal
data RequestType = Geom JSString | Url JSString
  deriving Generic

instance ToJSON RequestType
instance PToJSVal RequestType where
    pToJSVal = pToJSVal . toJSON




-- | Submit a POST or GET form to a specified target
submitForm :: MonadIO m
           => ServicePlugin
           -> FormTarget
           -> RequestType
           -> m ()
submitForm servicePlugin fTarget req = do
    $(qjs
            [julius|
                document['q$submitServicePluginForm'] = function(servicePlugin, fTarget, req) {
                  var isPOST = (req.tag == "Geom");
                  var f = document.createElement("form");
                  f.hidden = true;
                  f.target = fTarget;
                  f.method = isPOST ? "POST" : "GET";
                  f.action = servicePlugin.url;

                  var ic = document.createElement("input");
                  ic.type = "hidden";
                  ic.name = isPOST ? "geometry" : "url";
                  ic.value = req.contents;
                  f.appendChild(ic);

                  var it = document.createElement("input");
                  it.type = "hidden";
                  it.name = "view";
                  it.value = (fTarget == "_blank") ? "newtab" : "modal";
                  f.appendChild(it);
                  
                  document.body.appendChild(f);
                  f.submit();
                  document.body.removeChild(f);
                };
            |]
       )
    liftIO $ js_submitServicePluginForm (pToJSVal servicePlugin) fTarget (pToJSVal req)

-- this function is updated every time `submitFOrm is called.
foreign import javascript "document['q$submitServicePluginForm']($1, $2, $3);"
  js_submitServicePluginForm :: JSVal -> JSString -> JSVal -> IO ()


renderFormTarget :: Reflex t
                 => ServicePlugin
                 -> Event t RequestType
                 -> QuaWidget t x (Event t (FormTarget, RequestType))
renderFormTarget servicePlugin reqE = mdo
    
    (_, closeButtonE ) <- createModal
      (leftmost [showModalE, Inactive <$ closeButtonE ] )
      Inactive
      (do
          elAttr "iframe"
            (  "name" =: iFrameTargetName
            <> "style" =: "width: 100%; border: none;"
            ) blank
          elClass "div" "modal-footer" $
            elClass "p" "text-right" $
              buttonFlat "close" def
      )
      "standard"

    return $ inferTarget <$> reqE
  where
    showModalE = fmapMaybe activeModal $ inferView <$> reqE
    activeModal Modal = Just Active
    activeModal NewTab = Nothing
    inferTarget req
      = case inferView req of
          Modal -> (textToJSString iFrameTargetName, req)
          NewTab -> ("_blank", req)
    inferView req
      = case servicePlugin ^. spView of
          SPVModal  -> Modal
          SPVNewTab -> NewTab
          SPVAuto   -> case req of
            Geom _ -> Modal
            Url  _ -> NewTab
    iFrameTargetName = $(newVar >>= returnVars . (:[]))


