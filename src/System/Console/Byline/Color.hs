{-

This file is part of the package byline. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/byline/LICENSE. No part of the
byline package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}


--------------------------------------------------------------------------------
-- | Color type and functions for specifying colors.
module System.Console.Byline.Color
       ( Color (..)
       , black, red, green, yellow, blue, magenta, cyan, white
       , rgb
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Data.Word
import qualified System.Console.ANSI as ANSI

--------------------------------------------------------------------------------
-- | Opaque type for representing a color.
--
-- A color can be one of the eight standard terminal colors
-- constructed with one of the named color functions (e.g., 'black',
-- 'red', etc.) or using the 'rgb' function.
data Color = ColorCode ANSI.Color | ColorRGB (Word8, Word8, Word8)

--------------------------------------------------------------------------------
-- | Standard ANSI color by name.
black, red, green, yellow, blue, magenta, cyan, white :: Color
black   = ColorCode ANSI.Black
red     = ColorCode ANSI.Red
green   = ColorCode ANSI.Green
yellow  = ColorCode ANSI.Yellow
blue    = ColorCode ANSI.Blue
magenta = ColorCode ANSI.Magenta
cyan    = ColorCode ANSI.Cyan
white   = ColorCode ANSI.White

--------------------------------------------------------------------------------
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
rgb :: Word8 -> Word8 -> Word8 -> Color
rgb r g b = ColorRGB (r, g, b)
