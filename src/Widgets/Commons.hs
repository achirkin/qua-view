{-# LANGUAGE OverloadedStrings #-}

module Widgets.Commons
    ( flatButton
    ) where

import Reflex.Dom
import Commons

-- | Render a button with a click event attached.
--   Click event is labeled with a component name.
flatButton :: Reflex t => Text -> Widget x (Event t (ElementClick s))
flatButton name = do
    (e, _) <- elAttr' "a" attrs $ text name
    return $ ElementClick <$ domEvent Click e
  where
    attrs = ("class" =: "btn btn-flat btn-brand-accent waves-attach waves-effect")
         <> ("data-dismiss" =: "modal")
