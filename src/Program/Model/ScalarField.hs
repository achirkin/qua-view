{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Model.ScalarField
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.Model.ScalarField
    ( ScalarField(..)
    , ColorPalette (..)
    , makeColors
    ) where

--import GHC.TypeLits

import JsHs.WebGL
import Data.Geometry
import Data.Geometry.Structure.PointSet
import JsHs.Array as JS
import Data.Coerce

-- | Describes evaluations performed on geometry.
--   Provides functions to convert set of points into set of values,
--   so that these values can be used in geometry to create textures.
data ScalarField = ScalarField
    { cellSize  :: !GLfloat -- ^ desired size of computed cell
    , sfPoints  :: !(PointArray 3 GLfloat) -- ^ set of points to compute values on
    , sfRange   :: !(GLfloat, GLfloat) -- ^ min and max of the field values
    , sfValues  :: !(JS.Array GLfloat) -- ^ set of values
    }

-- | Color range to describe color pallete
data ColorPalette = LinearPalette !(Vector4 GLubyte) !(Vector4 GLubyte)
                  | Bezier2Palette !(Vector4 GLubyte) !(Vector4 GLubyte) !(Vector4 GLubyte)
                  | Bezier3Palette !(Vector4 GLubyte) !(Vector4 GLubyte) !(Vector4 GLubyte) !(Vector4 GLubyte)

-- | Generate list of colors
makeColors :: ColorPalette
           -> ScalarField
           -> PointArray 4 GLubyte -- ^ set of values in RGBA form [0..255]
makeColors (LinearPalette p0 p1) sf = fromJSArray . JS.map f $ normalized sf
    where f x = round $ (1-x) * v p0
                      +    x  * v p1
makeColors (Bezier2Palette p0 p1 p2) sf = fromJSArray . JS.map f $ normalized sf
    where f x | y <- 1-x = round $   y*y * v p0
                                 + 2*x*y * v p1
                                 +   x*x * v p2
makeColors (Bezier3Palette p0 p1 p2 p3) sf = fromJSArray . JS.map f $ normalized sf
    where f x | y <- 1-x = round $   y*y*y * v p0
                                 + 3*x*y*y * v p1
                                 + 3*x*x*y * v p2
                                 +   x*x*x * v p3


-- helpers

v :: Vector4 GLubyte -> Vector4 GLfloat
v = coerce

normalized :: ScalarField -> PointArray 4 GLfloat
normalized ScalarField
    { sfRange  = (xmin, xmax)
    , sfValues = vals
    } = fromJSArray $ JS.map f vals
    where f x = realToFrac . min 1 . max 0 $ (x - xmin)/xspan
          xspan = max 10e-5 $ xmax - xmin
