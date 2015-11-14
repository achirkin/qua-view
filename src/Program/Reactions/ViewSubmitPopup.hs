{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Reactions.ViewSubmitPopup
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Program.Reactions.ViewSubmitPopup () where

import Reactive

import Program
--import Program.Model.GeoJSON

import Controllers.GUIEvents

--import GHCJS.Useful
--import Program.View

--import GHCJS.Types
--import GHCJS.Foreign
--import GHCJS.Marshal


instance Reaction Program PView SubmitScenario "Show popup" 1 where
--    response _ (SubmitScenario url) _ program view = do
--        -- this must be ASAP - before drawing buffer is flushed
--        previewURL <- getPreviewURL
--        programInProgress
--        -- ... and then all the rest
--        json <- toJSRef_aeson . geometries2features . cityGeometryFull3D $ city program
--        showSubmitPopup previewURL json (toJSString url)
--        programIdle
--        return $ Left view
--
--foreign import javascript safe "document.getElementById('previewcontainer').innerHTML = \
--    \   '<img src=\"' + $1 + '\" style=\"width:100%;\"></img>';\
--    \ document.getElementById('sfSessionId').setAttribute('value', httpArgs['sessionID']);\
--    \ document.getElementById('sfGeometry').setAttribute('value', JSON.stringify($2));\
--    \ document.getElementById('sfPreview').setAttribute('value', $1);\
--    \ document.getElementById('submitform').setAttribute('action', $3);\
--    \ document.getElementById('popupsave').style.display='block';\
--    \ document.getElementById('popupbg').style.display='block';"
--    showSubmitPopup :: JSVal -> JSVal -> JSString -> IO ()
--
--foreign import javascript safe "$r = document.getElementById(\"glcanvas\").toDataURL('image/png');"
--    getPreviewURL :: IO (JSVal)
