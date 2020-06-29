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
module Byline.Internal.Eval
  ( MonadByline (..),
    BylineT,
    runBylineT,
  )
where

import Byline.Internal.Completion
import Byline.Internal.Prim (PrimF (..))
import Byline.Internal.Stylized
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont (ContT, MonadCont)
import Control.Monad.Except (MonadError)
import qualified Control.Monad.State.Lazy as LState
import qualified Control.Monad.Trans.Free.Church as Free
import qualified System.Console.Haskeline as Haskeline
import qualified System.Environment as System
import qualified System.Terminfo as Terminfo
import qualified System.Terminfo.Caps as Terminfo

-- | A class of types that can lift Byline operations into a base
-- monad.
--
-- @since 1.0.0.0
class Monad m => MonadByline (m :: * -> *) where
  liftByline :: Free.F PrimF a -> m a
  default liftByline :: (MonadTrans t, MonadByline m1, m ~ t m1) => Free.F PrimF a -> m a
  liftByline = lift . liftByline

instance MonadByline m => MonadByline (ExceptT e m)

instance MonadByline m => MonadByline (StateT s m)

instance MonadByline m => MonadByline (LState.StateT s m)

instance MonadByline m => MonadByline (ReaderT r m)

instance MonadByline m => MonadByline (IdentityT m)

instance MonadByline m => MonadByline (ContT r m)

-- | A monad transformer that implements 'MonadByline'.
--
-- @since 1.0.0.0
newtype BylineT m a = BylineT
  {unBylineT :: Free.FT PrimF m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState s,
      MonadReader r,
      MonadError e,
      MonadCont,
      MonadThrow,
      MonadCatch
    )

instance MonadTrans BylineT where
  lift = BylineT . lift

instance MonadByline (BylineT m) where
  liftByline = BylineT . Free.fromF

-- | Mutable list of completion functions.
--
-- @since 1.0.0.0
type CompRef m = IORef [CompletionFunc m]

-- | Discharge the 'MonadByline' effect by running all operations and
-- returning the result in the base monad.
--
-- The result is wrapped in a 'Maybe' where a 'Nothing' value
-- indicates that an end-of-file (EOF) signal was received while
-- reading user input.
--
-- @since 1.0.0.0
runBylineT ::
  forall m a.
  (MonadIO m, MonadMask m) =>
  Maybe (Handle, Handle) ->
  BylineT m a ->
  m (Maybe a)
runBylineT handles m = do
  compRef <- newIORef []
  let settings =
        Haskeline.setComplete
          (compFunc compRef)
          Haskeline.defaultSettings
  let (behavior, outh) = case handles of
        Nothing -> (Haskeline.defaultBehavior, Nothing)
        Just (inh, outh) -> (Haskeline.useFileHandle inh, Just outh)
  Haskeline.runInputTBehavior behavior settings (go compRef outh)
  where
    compFunc :: CompRef IO -> Haskeline.CompletionFunc m
    compFunc compRef input = liftIO $
      readIORef compRef >>= \case
        [] -> Haskeline.completeFilename input
        fs -> runCompletionFunctions fs input
    go ::
      CompRef IO ->
      Maybe Handle ->
      Haskeline.InputT m (Maybe a)
    go compRef outh = do
      mode <- defaultRenderMode
      unBylineT m
        & evalPrimF mode (fromMaybe stdout outh) compRef
        & unEvalT
        & runMaybeT

-- | Internal transformer for evaluating primitive operations in the
-- 'Haskeline.InputT' transformer with EOF handling.
--
-- @since 1.0.0.0
newtype EvalT m a = EvalT
  {unEvalT :: MaybeT (Haskeline.InputT m) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadTrans EvalT where
  lift = EvalT . lift . lift

-- | Evaluate 'PrimF' values.
--
-- @since 1.0.0.0
evalPrimF ::
  forall m a.
  (MonadIO m, MonadMask m) =>
  RenderMode ->
  Handle ->
  CompRef IO ->
  Free.FT PrimF m a ->
  EvalT m a
evalPrimF renderMode outputHandle compRef = Free.iterTM go
  where
    go ::
      PrimF (EvalT m a) ->
      EvalT m a
    go = \case
      Say s k ->
        liftIO (render renderMode outputHandle s) >> k
      AskLn s d k -> do
        let prompt =
              renderText renderMode $
                maybe s (\d' -> s <> text "[" <> text d' <> "] ") d
        liftHaskeline (Haskeline.getInputLine (toString prompt)) >>= \case
          Nothing -> EvalT empty
          Just answer
            | null answer -> k (fromMaybe mempty d)
            | otherwise -> k (toText answer)
      AskChar s k -> do
        let prompt = toString (renderText renderMode s)
        liftHaskeline (Haskeline.getInputChar prompt) >>= \case
          Nothing -> EvalT empty
          Just c -> k c
      AskPassword s m k -> do
        let prompt = toString (renderText renderMode s)
        liftHaskeline (Haskeline.getPassword m prompt) >>= \case
          Nothing -> EvalT empty
          Just str -> k (toText str)
      PushCompFunc f k ->
        modifyIORef' compRef (f :) >> k
      PopCompFunc k ->
        modifyIORef'
          compRef
          ( \case
              [] -> []
              (_ : fs) -> fs
          )
          >> k
    liftHaskeline :: Haskeline.InputT m b -> EvalT m b
    liftHaskeline = Haskeline.withInterrupt >>> lift >>> EvalT

-- | Calculate the default rendering mode based on the terminal type.
defaultRenderMode :: MonadIO m => Haskeline.InputT m RenderMode
defaultRenderMode = do
  termHint <- Haskeline.haveTerminalUI
  maxColors <- liftIO (runMaybeT getMaxColors)
  pure $ case (termHint, maxColors) of
    (True, Just n)
      | n < 256 -> Simple
      | otherwise -> Term256
    (True, Nothing) -> Simple
    (False, _) -> Plain
  where
    getMaxColors :: MaybeT IO Int
    getMaxColors = do
      term <- MaybeT (System.lookupEnv "TERM")
      liftIO (Terminfo.acquireDatabase term) >>= \case
        Left _ -> empty
        Right db -> hoistMaybe (Terminfo.queryNumTermCap db Terminfo.MaxColors)
