{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}

{-

This file is part of the package byline. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/byline/LICENSE. No part of the
byline package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}


--------------------------------------------------------------------------------
-- | Internal color operations.
module System.Console.Byline.Internal.Color
       ( Color (..)
       , colorAsANSI
       , nearestColor
       , term256Locations
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Control.Arrow (second)
import qualified Data.Colour.CIE as C
import qualified Data.Colour.SRGB as C
import Data.List (sortBy)
import Data.Maybe
import Data.Ord (comparing)
import Data.Word
import qualified System.Console.ANSI as ANSI

--------------------------------------------------------------------------------
-- Byline imports:
import System.Console.Byline.Color

--------------------------------------------------------------------------------
-- | Convert a Byline color to an ANSI color.
colorAsANSI :: Color -> ANSI.Color
colorAsANSI (ColorCode c) = c
colorAsANSI (ColorRGB  c) = nearestColor c ansiColorLocations

--------------------------------------------------------------------------------
-- | Find the nearest color given a full RGB color.
nearestColor :: (Bounded a)
             => (Word8, Word8, Word8)           -- ^ Original color.
             -> [(a, (Double, Double, Double))] -- ^ List of colors and locations.
             -> a                               -- ^ Destination color.
nearestColor (r, g, b) table =
  case listToMaybe (sortColors $ distances table) of
    Nothing     -> minBound -- Should never happen.
    Just (c, _) -> c

  where
    location :: (Double, Double, Double)
    location = C.cieXYZView (C.sRGB24 r g b)

    distance :: (Double, Double, Double) -> (Double, Double, Double) -> Double
    distance (x1, y1, z1) (x2, y2, z2) = sqrt ((x ** 2) + (y ** 2) + (z ** 2))
      where x = x1 - x2
            y = y1 - y2
            z = z1 - z2

    distances :: [(a, (Double, Double, Double))] -> [(a, Double)]
    distances = map (second (distance location))

    sortColors :: [(a, Double)] -> [(a, Double)]
    sortColors = sortBy (comparing snd)

--------------------------------------------------------------------------------
-- | Get the CIE locations for the standard ANSI colors.
--
-- Locations are based on the default xterm colors.  See also:
--
--  * <http://en.wikipedia.org/wiki/ANSI_escape_code>
--  * <http://en.wikipedia.org/wiki/Color_difference>
ansiColorLocations :: [(ANSI.Color, (Double, Double, Double))]
ansiColorLocations = [ (ANSI.Black,   (0.0,    0.0,    0.0))
                     , (ANSI.Red,     (0.2518, 0.1298, 0.0118))
                     , (ANSI.Green,   (0.2183, 0.4366, 0.0728))
                     , (ANSI.Yellow,  (0.4701, 0.5664, 0.0846))
                     , (ANSI.Blue,    (0.1543, 0.0617, 0.8126))
                     , (ANSI.Magenta, (0.3619, 0.1739, 0.592))
                     , (ANSI.Cyan,    (0.3285, 0.4807, 0.653))
                     , (ANSI.White,   (0.7447, 0.7835, 0.8532))
                     ]

--------------------------------------------------------------------------------
-- | All of the allowed colors for 256 color terminals.
term256Locations :: [(Word8, (Double, Double, Double))]
term256Locations = zipWith (\c i -> (i, C.cieXYZView c)) colors [16..]
  where
    colors :: [C.Colour Double]
    colors = do
      r <- [0.0, 0.2 .. 1.0]
      g <- [0.0, 0.2 .. 1.0]
      b <- [0.0, 0.2 .. 1.0]
      return (C.sRGB r g b)
