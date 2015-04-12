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
       , colorAsANSI
       ) where

--------------------------------------------------------------------------------
import qualified System.Console.ANSI as ANSI

--------------------------------------------------------------------------------
data Color = ColorCode ANSI.Color | ColorRGB (Int, Int, Int)

--------------------------------------------------------------------------------
-- | Standard ANSI colors.
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
-- | Full RGB colors.
rgb :: (Int, Int, Int) -> Color
rgb = ColorRGB

--------------------------------------------------------------------------------
colorAsANSI :: Color -> ANSI.Color
colorAsANSI (ColorCode c) = c
colorAsANSI (ColorRGB _)  = ANSI.Red -- FIXME: downgrade color