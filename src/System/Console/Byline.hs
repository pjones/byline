{-

This file is part of the package byline. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/byline/LICENSE. No part of the
byline package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module System.Console.Byline
       ( -- * Primary Operations
         say
       , sayLn
       , ask
       , askChar
       , askPassword
       , askUntil
       , report
       , reportLn

         -- * Constructing Stylized Text
       , text

         -- * Modifying Output Text
       , fg, bg, bold, underline

         -- * Specifying Colors
       , black, red, green, yellow, blue, magenta, cyan, white, rgb

         -- * Menus
       , Menu
       , Choice (..)
       , menu
       , askWithMenu
       , askWithMenuRepeatedly
       , banner
       , prefix
       , suffix
       , Matcher
       , matcher

         -- * Executing Terminal IO
       , Byline
       , runByline

         -- * Utility Functions, Operators, and Types
       , Stylized
       , ReportType (..)
       , withCompletionFunc
       , (<>)
       ) where

--------------------------------------------------------------------------------
import Data.Monoid ((<>))

--------------------------------------------------------------------------------
import System.Console.Byline.Internal.Byline
import System.Console.Byline.Internal.Color
import System.Console.Byline.Internal.Modifiers
import System.Console.Byline.Internal.Stylized
import System.Console.Byline.Menu
import System.Console.Byline.Primary
