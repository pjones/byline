{-

This file is part of the package byline. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/byline/LICENSE. No part of the
byline package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}


--------------------------------------------------------------------------------
-- | Modifiers for the @Stylized@ type.
module System.Console.Byline.Modifiers
       ( fg
       , bg
       , bold
       , underline
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Data.Monoid

--------------------------------------------------------------------------------
-- Byline imports:
import System.Console.Byline.Internal.Color
import System.Console.Byline.Internal.Types
import System.Console.Byline.Stylized

--------------------------------------------------------------------------------
-- The following is a kludge to avoid the "redundant import" warning
-- when using GHC >= 7.10.x.  This should be removed after we decide
-- to stop supporting GHC < 7.10.x.
import Prelude

--------------------------------------------------------------------------------
-- | Set the foreground color.  For example:
--
-- @
--     "Hello World!" <> fg magenta
-- @
fg :: Color -> Stylized
fg c = modStylized (mempty {modColorFG = OnlyOne (Just c)})

--------------------------------------------------------------------------------
-- | Set the background color.
bg :: Color -> Stylized
bg c = modStylized (mempty {modColorBG = OnlyOne (Just c)})

--------------------------------------------------------------------------------
-- | Produce bold text.
bold :: Stylized
bold = modStylized (mempty {modBold = On})

--------------------------------------------------------------------------------
-- | Produce underlined text.
underline :: Stylized
underline = modStylized (mempty {modUnderline = On})
