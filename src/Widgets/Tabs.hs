{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Widgets.Tabs
    ( TabWidget, SelectedTab (..)
    , runTabWidget, addTab
    ) where

import Reflex.Dom
import Data.Text (pack)
import Commons
import Widgets.Generation

import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.RWS.Lazy

-- | Wrapper around widget that keeps track of tab names.
--   Use `addTab` function to add new content in there,
--   and `runTabWidget` to build a normal widget.
newtype TabWidget x t a = TabWidget (RWST Text [(Int, SelectedTab)] Int (Widget x) a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO)


-- | Name of the selected tab
newtype SelectedTab = SelectedTab Text
  deriving Eq

-- | Internal function that creates a tab pane with tab names
tabsNavigator :: Reflex t => Text -> [(Int, SelectedTab)] -> Widget x (Dynamic t SelectedTab)
tabsNavigator baseid tabs = do
      evs <- elClass "ul" "nav nav-justified" $
        for tabs $ \(tid, SelectedTab tname) -> do
          e <- elClass "li" (if tid == 1 then "active" else "") $
            fst <$> elAttr' "a" (aAttrs <> "href" =: ("#" <> baseid <> pack (show tid))) (text tname)
          return $ SelectedTab tname <$ domEvent Click e
      elClass "div" "tab-nav-indicator" blank
      holdDyn (headEm tabs) $ leftmost evs
  where
    headEm [] = SelectedTab ""
    headEm ((_,x):_) = x
    aAttrs = "aria-expanded" =: "true"
          <> "class"         =: "waves-attach waves-effect"
          <> "data-toggle"   =: "tab"


-- | Add a widget tab
addTab :: Reflex t => Text -> Widget x a -> TabWidget x t a
addTab tname twidget = TabWidget $ do
    baseIdStr <- ask
    i <- state (\j -> (j, j+1))
    tell [(i,SelectedTab tname)]
    lift $ elAttr "div" (pAttrs i baseIdStr) twidget
  where
    pAttrs i ids = "class" =: toPanelClass i
                <> "id"    =: (ids <> pack (show i))
    toPanelClass i | i == 1    = "tab-pane fade active in"
                   | otherwise = "tab-pane fade"


-- | Build a widgets from several tabs
runTabWidget :: Reflex t => TabWidget x t a -> Widget x (Dynamic t SelectedTab, a)
runTabWidget (TabWidget tw) = do
    (contentEl, (r, _, tabNames)) <- elClass' "div" ("tab-content " <> tabContentClass) $
                 runRWST tw baseIdStr 1
    (navEl, selTab) <- elClass' "nav" "tab-nav tab-nav-red margin-top-no" $
                 tabsNavigator baseIdStr tabNames

    insertBefore (_element_raw navEl) (_element_raw contentEl)
    return (selTab, r)
  where
    (baseIdStr, tabContentClass) = $(do
        baseIdStr' <- newVar
        tabContentClass' <- newVar
        qcss
          [cassius|
            .#{tabContentClass'}
                padding-left: 20px;
                padding-right: 20px;
          |]
        returnVars [baseIdStr', tabContentClass']
      )
