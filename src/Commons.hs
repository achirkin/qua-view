-- | This module should not depend on anything in qua-view.
--   It can be imported by any other modules to use certain handy functions or types.
--
--   The structure of Commons provides three levels of the scope.
--
--   * Commons -- includes all helpers and data types, depends on reflex and reflex-dom packages.
--   * Commons.NoReflexDom -- does not depend on reflex-dom package
--   * Commons.NoReflex    -- does not depend on reflex-dom or reflex packages.
--
--   Additionally, you might want to use Widgets.Commons and Widgets.Generation modules
--   in Widgets module hierarchy. Those to should never excape Widgets.
--
module Commons
    ( module Commons.Import
    , module Commons.Http
    , module Commons.QuaViewMonad
    , module Commons.NoReflexDom
    ) where


import Commons.NoReflexDom
import Commons.Http
import Commons.QuaViewMonad
import Commons.Import
