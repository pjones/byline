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
module Byline.Completion
  ( -- * A Note About Completion
    -- $use

    -- * Completion Function
    CompletionFunc,
    Completion (..),

    -- * Completion Helpers
    CompLoc (..),
    completionFromList,

    -- * Setting the Active Completion Function
    pushCompletionFunction,
    popCompletionFunction,
  )
where

import           Byline.Internal.Completion
import           Byline.Internal.Eval       (MonadByline (..))
import qualified Byline.Internal.Prim       as Prim
import           Data.Char                  (isSpace)
import qualified Data.Text                  as Text

-- | Add a 'CompletionFunc' to the stack.
--
-- @since 1.0.0.0
pushCompletionFunction :: MonadByline m => CompletionFunc IO -> m ()
pushCompletionFunction = Prim.pushCompFunc >>> liftByline

-- | Remove the top completion function from the stack.
--
-- @since 1.0.0.0
popCompletionFunction :: MonadByline m => m ()
popCompletionFunction = liftByline Prim.popCompFunc

-- | Type to describe where completions are allowed.
--
-- @since 1.1.0.0
data CompLoc
  = -- | Only complete the first word of input.
    CompHead
  | -- | Complete any word except the first.
    CompTail
  | -- | Perform completion anywhere in the input.
    CompAny

-- | Generate a completion function that uses the given list as the
-- completion candidates.
--
-- @since 1.1.0.0
completionFromList ::
  forall m.
  Applicative m =>
  -- | Where to allow completion.
  CompLoc ->
  -- | List of completion candidates.
  [Text] ->
  -- | The generated completion function.
  CompletionFunc m
completionFromList loc ts (left, right) =
  case loc of
    CompHead ->
      if Text.null left || Text.all (isSpace >>> not) left
        then go (left, right)
        else pure (mempty, mempty)
    CompTail ->
      if Text.any isSpace left
        then completeLastWord (left, right)
        else pure (mempty, mempty)
    CompAny ->
      completeLastWord (left, right)
  where
    go :: CompletionFunc m
    go (left, _) =
      if Text.null left
        then pure ("", completions ts)
        else pure ("", completions (filter (Text.isPrefixOf left) ts))
    completeLastWord :: CompletionFunc m
    completeLastWord (left, right) =
      let word = Text.takeWhileEnd (isSpace >>> not) left
          prefix = Text.dropEnd (Text.length word) left
       in go (word, right) <&> first (const prefix)
    completions :: [Text] -> [Completion]
    completions = map (\t -> Completion t t True)

-- $use
--
-- Haskeline makes it very difficult (if not impossible) to implement
-- a completion function that runs in an arbitrary monad.  More
-- information can be found in the documentation for 'CompletionFunc'.
