{-

This file is part of the package byline. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/byline/LICENSE. No part of the
byline package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
-- | The completion types.
module System.Console.Byline.Completion
       ( CompletionFunc
       , Completion (..)
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Data.Text (Text)

--------------------------------------------------------------------------------
-- | A completion function modeled after the one used in Haskeline.
--
-- /Warning:/ If you're familiar with the Haskeline version of the
-- @CompletionFunc@ type please be sure to read this description
-- carefully since the two behave differently.
--
-- The completion function is called when the user presses the tab
-- key.  The current input line is split into two parts based on where
-- the cursor is positioned.  Text to the left of the cursor will be
-- the first value in the tuple and text to the right of the cursor
-- will be the second value.
--
-- The text returned from the completion function is the text from the
-- left of the cursor which wasn't used in the completion.  It should
-- also produce a list of possible 'Completion' values.
--
-- In Haskeline, some of these text values are reversed.  This is
-- /not/ the case in Byline.
--
-- /A note about @IO@:/
--
-- Due to the way that Byline uses Haskeline, the completion function
-- is forced to return an @IO@ value.  It would be better if it could
-- return a value in the base monad instead.  Patches welcome.
type CompletionFunc = (Text, Text) -> IO (Text, [Completion])

--------------------------------------------------------------------------------
-- | A type representing a completion match to the user's input.
data Completion = Completion
  { replacement :: Text -- ^ Text to insert to the right of the cursor.
  , display     :: Text -- ^ Text to display when listing all completions.
  , isFinished  :: Bool -- ^ Whether to follow the completed word with a
                        --  terminating space or close existing quotes.
  } deriving (Eq, Ord, Show)
