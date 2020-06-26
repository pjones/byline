{-# OPTIONS_HADDOCK hide #-}

-- |
--
-- Copyright:
--   This file is part of the package byline. It is subject to the
--   license terms in the LICENSE file found in the top-level
--   directory of this distribution and at:
--
--     https://github.com/pjones/byline
--
--   No part of this package, including this file, may be copied,
--   modified, propagated, or distributed except according to the
--   terms contained in the LICENSE file.
--
-- License: BSD-2-Clause
module Byline.Internal.Color
  ( Color (..),
    black,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    rgb,
    colorAsANSI,
    nearestColor,
    term256Locations,
  )
where

import Byline.Internal.Types
import qualified Data.Colour.CIE as C
import qualified Data.Colour.SRGB as C
import qualified System.Console.ANSI as ANSI

-- | Standard ANSI color by name.
--
-- @since 1.0.0.0
black, red, green, yellow, blue, magenta, cyan, white :: Color
black = ColorCode ANSI.Black
red = ColorCode ANSI.Red
green = ColorCode ANSI.Green
yellow = ColorCode ANSI.Yellow
blue = ColorCode ANSI.Blue
magenta = ColorCode ANSI.Magenta
cyan = ColorCode ANSI.Cyan
white = ColorCode ANSI.White

-- | Specify a color using a RGB triplet where each component is in
-- the range @[0 .. 255]@.  The actual rendered color will depend on
-- the terminal.
--
-- If the terminal advertises that it supports 256 colors, the color
-- given to this function will be converted to the nearest color in
-- the 216-color pallet supported by the terminal.  (216 colors
-- because the first 16 are the standard colors and the last 24 are
-- grayscale entries.)
--
-- However, if the terminal doesn't support extra colors, or doesn't
-- have a @TERMINFO@ entry (e.g., Windows) then the nearest standard
-- color will be chosen.
--
-- Nearest colors are calculated using their CIE distance from one
-- another.
--
-- See also:
--
--   * <http://en.wikipedia.org/wiki/ANSI_escape_code>
--   * <http://en.wikipedia.org/wiki/Color_difference>
--
-- @since 1.0.0.0
rgb :: Word8 -> Word8 -> Word8 -> Color
rgb r g b = ColorRGB (r, g, b)

-- | Convert a Byline color to an ANSI color.
--
-- @since 1.0.0.0
colorAsANSI :: Color -> ANSI.Color
colorAsANSI (ColorCode c) = c
colorAsANSI (ColorRGB c) = nearestColor c ansiColorLocations

-- | Find the nearest color given a full RGB color.
--
-- @since 1.0.0.0
nearestColor ::
  Bounded a =>
  -- | Original color.
  (Word8, Word8, Word8) ->
  -- | List of colors and locations.
  [(a, (Double, Double, Double))] ->
  -- | Destination color.
  a
nearestColor (r, g, b) table =
  case listToMaybe (sortColors $ distances table) of
    Nothing -> minBound -- Should never happen.
    Just (c, _) -> c
  where
    location :: (Double, Double, Double)
    location = C.cieXYZView (C.sRGB24 r g b)
    distance :: (Double, Double, Double) -> (Double, Double, Double) -> Double
    distance (x1, y1, z1) (x2, y2, z2) = sqrt ((x ** 2) + (y ** 2) + (z ** 2))
      where
        x = x1 - x2
        y = y1 - y2
        z = z1 - z2
    distances :: [(a, (Double, Double, Double))] -> [(a, Double)]
    distances = map (second (distance location))
    sortColors :: [(a, Double)] -> [(a, Double)]
    sortColors = sortBy (comparing snd)

-- | Get the CIE locations for the standard ANSI colors.
--
-- Locations are based on the default xterm colors.  See also:
--
--  * <http://en.wikipedia.org/wiki/ANSI_escape_code>
--  * <http://en.wikipedia.org/wiki/Color_difference>
--
-- @since 1.0.0.0
ansiColorLocations :: [(ANSI.Color, (Double, Double, Double))]
ansiColorLocations =
  [ (ANSI.Black, (0.0, 0.0, 0.0)),
    (ANSI.Red, (0.2518, 0.1298, 0.0118)),
    (ANSI.Green, (0.2183, 0.4366, 0.0728)),
    (ANSI.Yellow, (0.4701, 0.5664, 0.0846)),
    (ANSI.Blue, (0.1543, 0.0617, 0.8126)),
    (ANSI.Magenta, (0.3619, 0.1739, 0.592)),
    (ANSI.Cyan, (0.3285, 0.4807, 0.653)),
    (ANSI.White, (0.7447, 0.7835, 0.8532))
  ]

-- | All of the allowed colors for 256 color terminals.
--
-- @since 1.0.0.0
term256Locations :: [(Word8, (Double, Double, Double))]
term256Locations = zipWith (\c i -> (i, C.cieXYZView c)) colors [16 ..]
  where
    colors :: [C.Colour Double]
    colors = do
      r <- [0.0, 0.2 .. 1.0]
      g <- [0.0, 0.2 .. 1.0]
      b <- [0.0, 0.2 .. 1.0]
      return (C.sRGB r g b)
