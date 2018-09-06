{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Widgets.Tabs
    ( tabWidget
    ) where


import Commons
import Widgets.Generation
import Reflex.Dom
import qualified Data.Text as T
import qualified GHCJS.DOM.EventM as JSFFI (on)
import qualified GHCJS.DOM.JSFFI.Generated.Element     as JSFFI (getScrollTop)
import qualified GHCJS.DOM.JSFFI.Generated.HTMLElement as JSFFI (getOffsetTop)
import qualified GHCJS.DOM.JSFFI.Generated.GlobalEventHandlers as JSFFI (scroll)
import qualified GHCJS.DOM.Types as JSFFI ( HTMLElement(..), Element(..) )


-- | Build widgets from several tabs
tabWidget :: Reflex t
           => [(Text, QuaWidget t x ())] -> QuaWidget t x ()
tabWidget tabWidgets = mdo
    let offsetsAndScrollE = pushAlways getOffsets $ (,) ws <$> scrollE
        namesE = zip names <$> tabStates <$> offsetsAndScrollE
    void $ widgetHold (renderTabNames initNames) (renderTabNames <$> namesE)
    (containerEl, ws) <- elClass' "div" tabContentClass $
                           for tabWidgets $ \(name, w) ->
                             elAttr' "div" ("id" =: (toId name)) w
    scrollE <- elementScroll $ _element_raw containerEl

    -- logMsgEvents LevelWarn (LogSource "scrollE") $ (show) <$> scrollE

    return ()
  where
    names = fst $ unzip tabWidgets
    initNames = zip names $ Active:(repeat Inactive)
    (tabContentClass) = $(do
      tabContent   <- newVar
      qcss
        [cassius|
          .#{tabContent}
            overflow-y: auto
            overflow-x: hidden
            height: 100%
            padding-bottom: calc(100vh - 300px)
            margin-left: 32px
            > div
              padding-bottom: 60px
          .tab-nav
            margin-left: 32px
            box-shadow: 2px 2px 4px rgba(0,0,0,.24)
            li
              display: inline-block
        |]
      returnVars [tabContent]
      )

renderTabNames :: Reflex t
               => [(Text, ComponentState s)] -> QuaWidget t x ()
renderTabNames names =
  elClass "nav" "tab-nav" $
    elClass "ul" "nav" $ mapM_ renderName names
  where
    lnk name = elAttr "a" ("href" =: ("#" <> toId name)) $ text name
    renderName (n, Active)   = elClass "li" "active" $ lnk n
    renderName (n, Inactive) = elClass "li" "" $ lnk n

-- | for HTML id attribute
toId :: Text -> Text
toId name = T.filter (/=' ') name

tabStates :: Reflex t
          => ([Double], Int)
          -> [ComponentState s]
tabStates (offsets, scroll) = go offsets
  where
    go [] = []
    go (offset:rest) = if fromIntegral scroll < offset
                       then Active   : (replicate (length rest) Inactive)
                       else Inactive : (go rest)

-- | Creates an event that contains the scroll position of `elm`
elementScroll :: TriggerEvent t m => RawElement GhcjsDomSpace -> m (Event t Int)
elementScroll elm =
  newEventWithLazyTriggerWithOnComplete $ \cb ->
    JSFFI.on (coerce elm :: JSFFI.HTMLElement) JSFFI.scroll
      $ liftIO $ JSFFI.getScrollTop elm >>= flip cb (return ())

getOffsets :: (Reflex t, MonadIO m)
           => ([(Element EventResult GhcjsDomSpace t, a)], b)
           -> m ([Double], b)
getOffsets (wEls, s) = do
  os <- forM wEls $ JSFFI.getOffsetTop .
          (coerce :: JSFFI.Element -> JSFFI.HTMLElement) . _element_raw . fst
  return (os, s)
