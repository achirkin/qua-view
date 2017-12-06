{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict          #-}
-- | Transform geometrical coordinates to and from WGS84 (EPSG 4326).
--   https://www.linz.govt.nz/data/geodetic-services/coordinate-conversion/projection-conversions/transverse-mercator-transformation-formulae
module Model.GeoJSON.Coordinates.Wgs84
    ( wgs84ToMetric
    , guessIsWgs84
    ) where

import Numeric.DataFrame
import Model.Scenario.Statistics

-- | Try a simple heuristic to guess if a scenario is in lon-lat format (WGS84).
--   The premise is that coordinate values in WGS differ much-much-less than in metric system.
guessIsWgs84 :: ScenarioStatistics -> Bool
guessIsWgs84 ScenarioStatistics {..}
    | ssize < 1 = True
    | ssize < 3 && objNumber >= 10 = True
    | ssize < 5 && objNumber >= 100 = True
    | otherwise = False
  where
    ssize = normLPInf . abs $ upperCorner - lowerCorner


----------------------------------------------------------------------------------------------------

-- | equatorial radius - Semi-major axis of reference ellipsoid
_a :: Double
_a = 6378137

-- | Ellipsoidal flattening
_f :: Double
_f = 1 / 298.257223563

-- | Central meridian scale factor
_k :: Double
_k = 1

-- | Take two arguments in a (lon,lat) format (WGS84 degrees)
--   and return transverse mercator projection with origin at the first argument point.
--   No false easting or northing.
wgs84ToMetric :: Vec2f -- ^ lonlat of coordinate origin
              -> Vec2f -- ^ current coordinate (lon, lat)
              -> Vec2f
wgs84ToMetric wgsCenter wgsX = vec2 (realToFrac x) (realToFrac y)
  where
    (lon0, lat0) = wgs84ToDegrees wgsCenter
    (lon, lat) = wgs84ToDegrees wgsX
    m0 = m lat0
    sin1 = sin lat
    cos1 = cos lat
    cos2 = cos1 * cos1
    cos3 = cos2 * cos1
    cos5 = cos3 * cos2
    cos7 = cos5 * cos2
    ep = recip $ 1 - _e2 * sin1 * sin1
    v = _a * sqrt ep
    ρ = v * ep * ( 1 - _e2 )
    psi1 = v / ρ
    psi2 = psi1*psi1
    psi3 = psi2*psi1
    psi4 = psi2*psi2
    t2 = t*t
    t4 = t2*t2
    t6 = t4*t2
    t = tan lat
    w = lon - lon0
    w2 = w*w
    w4 = w2*w2
    w6 = w4*w2
    w8 = w4*w4

    y = _k * ( m lat - m0
             + 0.5 * w2 * v * sin1 * cos1
             + w4/24    * v * sin1 * cos3 * ( 4 * psi2 + psi1 - t2 )
             + w6/720   * v * sin1 * cos5 * ( 8 * psi4 * (11-24*t2) - 28*psi3*(1-6*t2)
                                                + psi2 * ( 1-32*t2) -  2*psi1*t2 + t4 )
             + w8/40320 * v * sin1 * cos7 * ( 1385 - 3111*t2 + 543*t4 - t6)
             )

    x = _k * v * w *
        ( cos1
        + w2/6    * cos3 * ( psi1 - t2 )
        + w4/120  * cos5 * ( 4 * psi3 * (1-6*t2) + psi2*(1+8*t2) - psi1*2*t2 + t4 )
        + w6/5040 * cos7 * ( 61 - 479 * t2 + 179 * t4 - t6 )
        )


wgs84ToDegrees :: Vec2f -> (Double, Double)
wgs84ToDegrees v = case unpackV2 v of
  (x, y) -> ( pi180 * realToFrac x
            , pi180 * realToFrac y
            )


-- | Function of latitude in radians
m :: Double -> Double
m φ = _a *
  ( _A0 * φ - _A2 * sin (2*φ) + _A4 * sin (4*φ) - _A6 * sin (6*φ) )


-- * Helper constants

pi180 :: Double
pi180 = pi / 180

_b :: Double
_b = _a * (1 - _f)

_e2 :: Double
_e2 = 2 * _f - _f * _f

_e4 :: Double
_e4 = _e2 * _e2

_e6 :: Double
_e6 = _e4 * _e2

_A0 :: Double
_A0 = 1 - (1/4)   * _e2
        - (3/64)  * _e4
        - (5/256) * _e6

_A2 :: Double
_A2 = (3/8) *
    ( _e2 + (1/4) * _e4  + (15/128) * _e6 )

_A4 :: Double
_A4 = (15/256) * ( _e4 + (3/4) * _e6 )


_A6 :: Double
_A6 = (35/3072) * _e6


