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
module Byline.Internal.Prim
  ( PrimF (..),
    say,
    sayLn,
    askLn,
    askChar,
    askPassword,
    pushCompFunc,
    popCompFunc,
  )
where

import Byline.Internal.Completion (CompletionFunc)
import Byline.Internal.Stylized (Stylized, text)
import Control.Monad.Trans.Free.Church (MonadFree)
import qualified Control.Monad.Trans.Free.Church as Free

-- | Primitive operations as a free monad.
--
-- @since 1.0.0.0
data PrimF f
  = Say (Stylized Text) f
  | AskLn (Stylized Text) (Maybe Text) (Text -> f)
  | AskChar (Stylized Text) (Char -> f)
  | AskPassword (Stylized Text) (Maybe Char) (Text -> f)
  | PushCompFunc (CompletionFunc IO) f
  | PopCompFunc f
  deriving (Functor)

-- | Smart constructor.
--
-- @since 1.0.0.0
say :: MonadFree PrimF m => Stylized Text -> m ()
say = Free.liftF . (`Say` ())

-- | Smart constructor.
--
-- @since 1.0.0.0
sayLn :: MonadFree PrimF m => Stylized Text -> m ()
sayLn message = say (message <> text "\n")

-- | Smart constructor.
--
-- @since 1.0.0.0
askLn :: MonadFree PrimF m => Stylized Text -> Maybe Text -> m Text
askLn prompt def = Free.liftF (AskLn prompt def id)

-- | Smart constructor.
--
-- @since 1.0.0.0
askChar :: MonadFree PrimF m => Stylized Text -> m Char
askChar = Free.liftF . (`AskChar` id)

-- | Smart constructor.
--
-- @since 1.0.0.0
askPassword :: MonadFree PrimF m => Stylized Text -> Maybe Char -> m Text
askPassword prompt mask = Free.liftF (AskPassword prompt mask id)

-- | Smart constructor.
--
-- @since 1.0.0.0
pushCompFunc :: MonadFree PrimF m => CompletionFunc IO -> m ()
pushCompFunc = Free.liftF . (`PushCompFunc` ())

-- | Smart constructor.
--
-- @since 1.0.0.0
popCompFunc :: MonadFree PrimF m => m ()
popCompFunc = Free.liftF (PopCompFunc ())
