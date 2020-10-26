{-# LANGUAGE UndecidableInstances #-}
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
module Byline.Internal.Simulation
  ( Simulated (..),
    SimulationFunction,
    SimulationState (..),
    BylineT (..),
    runBylineT,
  )
where

import Byline.Internal.Completion
import Byline.Internal.Eval (MonadByline (..))
import Byline.Internal.Prim
import Byline.Internal.Stylized
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import qualified Control.Monad.Trans.Free.Church as Free
import qualified Data.Text as Text

-- | Simulated user input.
--
-- @since 1.0.0.0
data Simulated
  = -- | Simulate user input by providing the 'Text' value
    -- they typed as a response to a prompt.
    --
    -- If the asking function wants a single character of input then
    -- only the first character of the provided 'Text' is used.  In
    -- this case, if an empty 'Text' value is given, it will be treated
    -- as an end-of-file (EOF) character.
    SimulatedInput Text
  | -- | Simulate an end-of-file (EOF) character.  Usually this occurs
    -- when the user enters @Control-D@ or when standard input is
    -- exhausted.
    SimulatedEOF

-- | A function that simulates user input by returning a 'Simulated'
-- value.
--
-- The function has full access to the 'SimulationState' including the
-- ability to change the simulation function itself.  For example,
-- below is a function that will return the text \"Current" the first
-- time it is called and \"Next" every time after that.
--
-- @
--
--  textThenDefault :: Monad m => SimulationFunction m
--  textThenDefault = do
--    -- The next input request will come from this function:
--    modify (\s -> s {simulationFunction = pure (SimulatedInput \"Next")})
--
--    -- But this time we'll return different text:
--    pure (SimulatedInput \"Current")
-- @
--
-- @since 1.0.0.0
type SimulationFunction m = StateT (SimulationState m) m Simulated

-- | Stateful information available to the simulation function.
--
-- @since 1.0.0.0
data SimulationState m
  = SimulationState
      { -- | The prompt associated with current request for input.  This
        -- 'Text' value will /not/ contain any formatting escape codes such
        -- as colors.
        precedingPrompt :: Text,
        -- | The function that will be called to simulate user input.
        simulationFunction :: SimulationFunction m,
        -- | The stack of completion functions.
        completionFunctions :: [CompletionFunc IO]
      }

-- | A monad transformer that implements the 'MonadByline' class
-- without actually doing anything.
--
-- @since 1.0.0.0
newtype BylineT m a
  = BylineT
      {unBylineT :: MaybeT (StateT (SimulationState m) m) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader r,
      MonadError e,
      MonadCont,
      MonadThrow,
      MonadCatch
    )

instance MonadState s m => MonadState s (BylineT m) where
  state = lift . state

instance MonadTrans BylineT where
  lift = BylineT . lift . lift

instance Monad m => MonadByline (BylineT m) where
  liftByline = evalPrimF

-- | Evaluate a 'PrimF' instruction.
--
-- @since 1.0.0.0
evalPrimF :: forall m a. Monad m => Free.F PrimF a -> BylineT m a
evalPrimF = Free.iterM go
  where
    go :: PrimF (BylineT m a) -> BylineT m a
    go = \case
      Say _ k -> k
      AskLn s d k -> simulate s $ \t ->
        if Text.null t
          then k (fromMaybe t d)
          else k t
      AskChar s k -> simulate s $ \t ->
        if Text.null t
          then BylineT empty
          else k (Text.head t)
      AskPassword s _ k -> simulate s k
      PushCompFunc f k ->
        BylineT
          ( lift . modify $ \st ->
              st {completionFunctions = f : completionFunctions st}
          )
          >> k
      PopCompFunc k ->
        BylineT
          ( lift . modify $ \st ->
              case completionFunctions st of
                [] -> st {completionFunctions = []}
                _ : xs -> st {completionFunctions = xs}
          )
          >> k
    simulate :: Stylized Text -> (Text -> BylineT m b) -> BylineT m b
    simulate s f = do
      BylineT (modify $ \st -> st {precedingPrompt = renderText Plain s})
      simfun <- BylineT (gets simulationFunction)
      BylineT (lift simfun) >>= \case
        SimulatedInput t -> f t
        SimulatedEOF -> BylineT empty

-- | Discharge the 'MonadByline' effect using the given 'SimulationFunction'.
--
-- @since 1.0.0.0
runBylineT :: Monad m => SimulationFunction m -> BylineT m a -> m (Maybe a)
runBylineT f =
  unBylineT
    >>> runMaybeT
    >>> (`evalStateT` SimulationState "" f [])
