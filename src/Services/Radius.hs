-----------------------------------------------------------------------------
-- |
-- Module      :  Services.Radius
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Services.Radius where

--import JsHs.WebGL
--
--import Data.Geometry
--import JsHs.Array as JS
--import Program.Model.ScalarField
--
--import Services
--
---- | This service computes simple euclidian distance from one pre-defined point
--newtype Radius = Radius (Vector3 GLfloat) deriving Show


--instance ComputingService Radius where
--    runService (Radius x) _ _ sf@ScalarField{ sfPoints = pnts} = return $
--      if JS.length pnts == 0 then Nothing
--      else Just sf
--        { sfRange = ( JS.foldl1 min vals
--                    , JS.foldl1 max vals)
--        , sfValues = vals
--        } where vals = JS.map (\v -> - normL2 (x - v)) pnts
