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
    Settings (..),
    defaultBylineSettings,
    runBylineT',
    defaultRenderMode,
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
import qualified System.Console.ANSI as ANSI
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
newtype BylineT m a
  = BylineT
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
  (MonadIO m, MonadMask m) =>
  BylineT m a ->
  m (Maybe a)
runBylineT = runBylineT' defaultBylineSettings

-- | Settings that control Byline at run time.
--
-- @since 1.0.0.0
data Settings
  = Settings
      { -- | The output handle to write to.  If 'Nothing' use standard
        -- output.
        --
        -- NOTE: This only affects Byline (i.e. functions that use
        -- @say@).  Functions like @ask@ that invoke Haskeline will always
        -- use standard output since that's the hard-coded default.
        bylineOutput :: Maybe Handle,
        -- | The input handle to read from.  If 'Nothing' use standard
        -- input.
        bylineInput :: Maybe Handle,
        -- | Override the detected render mode.
        --
        -- If 'Nothing' use the render mode that is calculated based on
        -- the type of handle Byline writes to.
        bylineMode :: Maybe RenderMode
      }

-- | The default Byline settings.
--
-- @since 1.0.0.0
defaultBylineSettings :: Settings
defaultBylineSettings = Settings Nothing Nothing Nothing

-- | Like 'runBylineT' except you can override the settings.
--
-- @since 1.0.0.0
runBylineT' ::
  forall m a.
  (MonadIO m, MonadMask m) =>
  Settings ->
  BylineT m a ->
  m (Maybe a)
runBylineT' Settings {..} m = do
  compRef <- newIORef []
  let settings =
        Haskeline.setComplete
          (compFunc compRef)
          Haskeline.defaultSettings
  let behavior =
        maybe
          Haskeline.defaultBehavior
          Haskeline.useFileHandle
          bylineInput
  let hOut = fromMaybe stdout bylineOutput
  Haskeline.runInputTBehavior behavior settings (go compRef hOut)
  where
    compFunc :: CompRef IO -> Haskeline.CompletionFunc m
    compFunc compRef input = liftIO $
      readIORef compRef >>= \case
        [] -> Haskeline.completeFilename input
        fs -> runCompletionFunctions fs input
    go ::
      CompRef IO ->
      Handle ->
      Haskeline.InputT m (Maybe a)
    go compRef hOut = do
      mode <- maybe (liftIO (defaultRenderMode hOut)) pure bylineMode
      unBylineT m
        & evalPrimF mode hOut compRef
        & unEvalT
        & runMaybeT

-- | Internal transformer for evaluating primitive operations in the
-- 'Haskeline.InputT' transformer with EOF handling.
--
-- @since 1.0.0.0
newtype EvalT m a
  = EvalT
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
                maybe s (\d' -> s <> text "[" <> text d' <> text "] ") d
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
defaultRenderMode :: Handle -> IO RenderMode
defaultRenderMode hOut = do
  isTerm <- ANSI.hSupportsANSI hOut
  if isTerm
    then runMaybeT getMaxColors >>= \case
      Nothing -> pure Simple
      Just n
        | n < 256 -> pure Simple
        | n > 256 -> pure TermRGB
        | otherwise -> pure Term256
    else pure Plain
  where
    getMaxColors :: MaybeT IO Int
    getMaxColors = do
      term <- MaybeT (System.lookupEnv "TERM")
      lift (Terminfo.acquireDatabase term) >>= \case
        Left _ -> empty
        Right db ->
          hoistMaybe $
            Terminfo.queryNumTermCap db Terminfo.MaxColors
