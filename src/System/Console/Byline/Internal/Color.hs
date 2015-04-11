{-

This file is part of the package byline. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/byline/LICENSE. No part of the
byline package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}


--------------------------------------------------------------------------------
module System.Console.Byline.Internal.Color
       ( Color (..)
       , black, red, green, yellow, blue, magenta, cyan, white
       , rgb
       ) where

--------------------------------------------------------------------------------
data Color = ColorANSI Int | ColorRGB (Int, Int, Int)

--------------------------------------------------------------------------------
-- | Standard ANSI colors.
black, red, green, yellow, blue, magenta, cyan, white :: Color
black   = ColorANSI 0
red     = ColorANSI 1
green   = ColorANSI 2
yellow  = ColorANSI 3
blue    = ColorANSI 4
magenta = ColorANSI 5
cyan    = ColorANSI 6
white   = ColorANSI 7

--------------------------------------------------------------------------------
-- | Full RGB colors.
rgb :: (Int, Int, Int) -> Color
rgb = ColorRGB
