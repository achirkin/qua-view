{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Widgets.Commons
    ( -- * Buttons
      buttonFlat
    , buttonRed
    , buttonFlatHideDyn
      -- * Common classes to use
    , smallMarginClass
    ) where

import Reflex.Dom
import Commons
import Widgets.Generation

-- | Render a button with a click event attached.
--   Click event is labeled with a component name.
buttonFlat :: forall s t m
            . (Reflex t, DomBuilder t m)
           => Text  -- ^ name of the button
           -> Map Text Text -- ^ additional attributes
           -> m (Event t (ElementClick s))
buttonFlat name moreAttrs = do
    (e, _) <- elAttr' "a" attrs $ text name
    return $ ElementClick <$ domEvent Click e
  where
    attrs = "class" =: ("btn btn-flat btn-brand-accent waves-attach waves-effect " <> smallMarginClass) <> moreAttrs

-- | Render a button with a click event attached.
--   Hide the button if supplied Dynamic ComponentState is Inactive
--   Click event is labeled with a component name.
buttonFlatHideDyn :: forall s t m
                   . (Reflex t, DomBuilder t m, PostBuild t m)
                  => Dynamic t (ComponentState s) -- ^ Active or Inactive
                  -> Text          -- ^ name of the button
                  -> Map Text Text -- ^ additional attributes
                  -> m (Event t (ElementClick s))
buttonFlatHideDyn stateDyn name moreAttrs = do
    (e, _) <- elDynAttr' "a" (fmap stateToAttr stateDyn) $ text name
    return $ ElementClick <$ domEvent Click e
  where
    stateToAttr Active   = attrs
    stateToAttr Inactive = attrs <> ("style" =: "display: none;")
    attrs = "class" =: ("btn btn-flat btn-brand-accent waves-attach waves-effect "
                          <> smallMarginClass) <> moreAttrs

-- | Render a button with a click event attached.
--   Click event is labeled with a component name.
buttonRed :: forall s t m
           . (Reflex t, DomBuilder t m)
          => Text  -- ^ name of the button
          -> Map Text Text -- ^ additional attributes
          -> m (Event t (ElementClick s))
buttonRed name moreAttrs = do
    (e, _) <- elAttr' "a" attrs $ text name
    return $ ElementClick <$ domEvent Click e
  where
    attrs = "class" =: ("btn btn-red waves-attach waves-light waves-effect " <> smallMarginClass) <> moreAttrs


-- | add this class to make a small margin between buttons
smallMarginClass :: Text
smallMarginClass = $(do
    c <- newVar
    qcss [cassius|
          .#{c}
            margin: 2px
         |]
    returnVars [c]
  )
