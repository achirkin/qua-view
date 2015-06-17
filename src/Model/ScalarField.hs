-----------------------------------------------------------------------------
--
-- Module      :  Model.ScalarField
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Model.ScalarField
    ( ScalarField(..)
    , ColorPalette (..)
    , makeColors
    ) where

--import GHC.TypeLits

import GHCJS.WebGL
import Geometry.Space

-- | Describes evaluations performed on geometry.
--   Provides functions to convert set of points into set of values,
--   so that these values can be used in geometry to create textures.
data ScalarField = ScalarField
    { cellSize  :: !GLfloat -- ^ desired size of computed cell
    , sfPoints  :: ![Vector3 GLfloat] -- ^ set of points to compute values on
    , sfRange   :: !(Vector2 GLfloat) -- ^ min and max values of the field values
    , sfValues  :: ![GLfloat] -- ^ set of values
    }

-- | Color range to describe color pallete
data ColorPalette = LinearPalette !(Vector4 GLubyte) !(Vector4 GLubyte)
                  | Bezier2Palette !(Vector4 GLubyte) !(Vector4 GLubyte) !(Vector4 GLubyte)
                  | Bezier3Palette !(Vector4 GLubyte) !(Vector4 GLubyte) !(Vector4 GLubyte) !(Vector4 GLubyte)

-- | Generate list of colors
makeColors :: ColorPalette
           -> ScalarField
           -> [Vector4 GLubyte] -- ^ set of values in RGBA form [0..255]
makeColors (LinearPalette p0 p1) sf = map f $ normalized sf
    where f x = fmap round $ (1-x) ..* v p0
                          .+    x  ..* v p1
makeColors (Bezier2Palette p0 p1 p2) sf = map f $ normalized sf
    where f x | y <- 1-x = fmap round $   y*y ..* v p0
                                     .+ 2*x*y ..* v p1
                                     .+   x*x ..* v p2
makeColors (Bezier3Palette p0 p1 p2 p3) sf = map f $ normalized sf
    where f x | y <- 1-x = fmap round $   y*y*y ..* v p0
                                     .+ 3*x*y*y ..* v p1
                                     .+ 3*x*x*y ..* v p2
                                     .+   x*x*x ..* v p3


-- helpers

v :: Vector4 GLubyte -> Vector4 GLfloat
v p = fmap fromIntegral p

normalized :: ScalarField -> [GLfloat]
normalized ScalarField
    { sfRange  = Vector2 xmin xmax
    , sfValues = vals
    } = map f vals
    where f x = (min 1 . max 0 $ (x - xmin)/xspan)
          xspan = max 10e-5 $ xmax - xmin
