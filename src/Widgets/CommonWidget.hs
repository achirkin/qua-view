{-# LANGUAGE OverloadedStrings #-}

module Widgets.CommonWidget
    ( flatButton'
    ) where

import Data.Semigroup
import Reflex.Dom

import CommonTypes
import Widgets.Generation

flatButton' :: Reflex t => Text -> Widget x (Event t ())
flatButton' name = do
    (e, _) <- elAttr' "a" attrs $ text name
    return $ domEvent Click e
  where
    attrs = ("class" =: "btn btn-flat btn-brand-accent waves-attach waves-effect")
         <> ("data-dismiss" =: "modal")