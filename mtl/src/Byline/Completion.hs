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

    -- * Setting the Active Completion Function
    pushCompletionFunction,
    popCompletionFunction,
  )
where

import Byline.Internal.Completion
import Byline.Internal.Eval (MonadByline (..))
import qualified Byline.Internal.Prim as Prim

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

-- $use
--
-- Haskeline makes it very difficult (if not impossible) to implement
-- a completion function that runs in an arbitrary monad.  More
-- information can be found in the documentation for 'CompletionFunc'.
