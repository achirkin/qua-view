{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Widgets.Commons
    ( -- * Buttons
      buttonFlat, buttonFlatDyn
    , buttonRed
    , hr
      -- * Common classes to use
    , smallMarginClass
      -- * Helpers
    , whenActive
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
--   Hide the button if supplied Dynamic ComponentState is Inactive,
--   show button if the supplied state is Active.
--   Click event is labeled with a component name.
buttonFlatDyn :: forall s t m
                   . (Reflex t, DomBuilder t m, PostBuild t m)
                  => Dynamic t (ComponentState s) -- ^ Active or Inactive
                  -> Text          -- ^ name of the button
                  -> Map Text Text -- ^ additional attributes
                  -> m (Event t (ElementClick s))
buttonFlatDyn stateDyn name moreAttrs = do
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

-- | Horizontal line with not so much spacing around
hr :: (Reflex t, DomBuilder t m) => m ()
hr = elAttr "hr" ("style" =: "margin: 2px") (pure ())


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

whenActive :: (Reflex t, MonadSample t m, DomBuilder t m, MonadHold t m)
           => Dynamic t (ComponentState s) -> m () -> m ()
whenActive cstateD w = do
    cstateI <- sample $ current cstateD
    void $ widgetHold (whenActiveF cstateI) (whenActiveF <$> updated cstateD)
  where
    whenActiveF Active   = w
    whenActiveF Inactive = blank



