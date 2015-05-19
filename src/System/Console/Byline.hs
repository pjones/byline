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
       ( -- * Introduction
         --
         -- | Byline provides a monad transformer that allows you to compose
         -- interactive terminal actions.  When producing output,
         -- these actions accept stylized text that can include
         -- foreground and background colors, underlined text, and
         -- bold text.
         --
         -- Stylized text can be constructed with string literals
         -- (using the @OverloadedStrings@ extension) or using the
         -- 'text' function.  Attributes such as color can be changed
         -- using modifier functions and the @mappend@ operator,
         -- @(<>)@.
         --
         -- Actions that read user input can work with completion
         -- functions which are activated when the user presses the
         -- tab key.  Most input actions also support default values
         -- that will be returned when the user presses the enter key
         -- without providing any input.
         --
         -- Example:
         --
         -- @
         --     {-\# LANGUAGE OverloadedStrings \#-}
         --
         --     ...
         --
         --     language <- runByline $ do
         --         sayLn ("Look mom, " <> ("colors" <> fg blue) <> "!")
         --
         --         let question = "What's your favorite " <>
         --                        ("language" <> bold)    <> "? "
         --
         --         ask question Nothing
         -- @
         --
         -- More complete examples can be found in the @examples@
         -- directory of the distribution tarball or in the
         -- repository.

         -- * Executing Interactive Sessions
         Byline
       , runByline

         -- * Primitive Operations
       , say
       , sayLn
       , ask
       , askChar
       , askPassword
       , askUntil
       , report
       , reportLn

         -- * Constructing Stylized Text
       , Stylized
       , text

         -- * Modifying Output Text

         -- | The 'Stylized' type is an instance of the monoid class.
         -- This means you can change attributes of the text by using
         -- the following functions along with @mappend@ or the @(<>)@
         -- operator.
       , fg, bg, bold, underline

         -- * Specifying Colors
       , Color
       , black, red, green, yellow, blue, magenta, cyan, white, rgb

         -- * Menus

         -- | Menus provide a way to display a small number of list items
         -- to the user.  The desired list item is selected by typing
         -- its index or by typing a unique prefix string.  A default
         -- completion function is provided to allow the user to
         -- select a list item using tab completion.
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

         -- * Completion
       , CompletionFunc
       , Completion (..)
       , withCompletionFunc

         -- * Utility Functions, Operators, and Types
       , ReportType (..)
       , (<>)
       ) where

--------------------------------------------------------------------------------
import Data.Monoid ((<>))

--------------------------------------------------------------------------------
import System.Console.Byline.Color
import System.Console.Byline.Completion
import System.Console.Byline.Internal.Byline
import System.Console.Byline.Menu
import System.Console.Byline.Modifiers
import System.Console.Byline.Primitive
import System.Console.Byline.Stylized
