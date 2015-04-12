{-

This file is part of the package byline. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/byline/LICENSE. No part of the
byline package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}


--------------------------------------------------------------------------------
module System.Console.Byline.Internal.Modifiers
       ( fg
       , bg
       , bold
       , underline
       ) where

--------------------------------------------------------------------------------
import Data.Monoid
import System.Console.Byline.Internal.Color
import System.Console.Byline.Internal.Stylized
import System.Console.Byline.Internal.Types

--------------------------------------------------------------------------------
fg :: Color -> Stylized
fg c = StylizedMod (mempty {modColorFG = OnlyOne (Just c)})

--------------------------------------------------------------------------------
bg :: Color -> Stylized
bg c = StylizedMod (mempty {modColorBG = OnlyOne (Just c)})

--------------------------------------------------------------------------------
bold :: Stylized
bold = StylizedMod (mempty {modBold = On})

--------------------------------------------------------------------------------
underline :: Stylized
underline = StylizedMod (mempty {modUnderline = On})
