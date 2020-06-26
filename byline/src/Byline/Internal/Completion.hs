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
module Byline.Internal.Completion
  ( CompletionFunc,
    Completion (..),
    runCompletionFunction,
    runCompletionFunctions,
  )
where

import qualified Data.Text as Text
import qualified System.Console.Haskeline.Completion as Haskeline

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
-- Due to Haskeline, the completion function is forced to return an
-- @IO@ value.  It would be better if it could return a value in the
-- base monad instead but it doesn't look like that's possible.
-- Patches welcome.
--
-- @since 1.0.0.0
type CompletionFunc m = (Text, Text) -> m (Text, [Completion])

-- | A type representing a completion match to the user's input.
--
-- @since 1.0.0.0
data Completion = Completion
  { -- | Text to insert to the right of the cursor.
    replacement :: Text,
    -- | Text to display when listing all completions.
    display :: Text,
    -- | Whether to follow the completed word with a
    --  terminating space or close existing quotes.
    isFinished :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Convert a Byline completion result into a Haskeline completion result.
--
-- @since 1.0.0.0
convertCompletion :: Completion -> Haskeline.Completion
convertCompletion (Completion r d i) =
  Haskeline.Completion
    { Haskeline.replacement = toString r,
      Haskeline.display = toString d,
      Haskeline.isFinished = i
    }

-- | Adapt a completion function so it works with Haskeline.
--
-- @since 1.0.0.0
runCompletionFunction ::
  Monad m =>
  CompletionFunc m ->
  Haskeline.CompletionFunc m
runCompletionFunction comp (left, right) = do
  (output, completions) <-
    comp
      ( Text.reverse $ toText left,
        toText right
      )
  pure
    ( toString $ Text.reverse output,
      map convertCompletion completions
    )

-- | Run a list of completion functions, returning the results of the
-- first function that produced any.
--
-- @since 1.0.0.0
runCompletionFunctions ::
  forall m.
  Monad m =>
  [CompletionFunc m] ->
  Haskeline.CompletionFunc m
runCompletionFunctions fs input =
  foldlM go (mempty, mempty) fs
  where
    go ::
      (String, [Haskeline.Completion]) ->
      CompletionFunc m ->
      m (String, [Haskeline.Completion])
    go prev f = case prev of
      (_, []) -> runCompletionFunction f input
      _ -> pure prev
