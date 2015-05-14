{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-

This file is part of the package byline. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/byline/LICENSE. No part of the
byline package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module System.Console.Byline.Internal.Byline
       ( Byline (..)
       , Env    (..)
       , eof
       , liftOuter
       , liftInputT
       , runByline
       ) where


--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.IORef
import System.Console.Byline.Internal.Completion
import System.Console.Byline.Internal.Render
import System.IO (Handle, stdout)

--------------------------------------------------------------------------------
-- Import Haskeline, which is used to do the heavy lifting.
import qualified System.Console.Haskeline    as H
import qualified System.Console.Haskeline.IO as H

--------------------------------------------------------------------------------
data Env = Env
  { renderMode  :: RenderMode
  , outHandle   :: Handle
  , inputState  :: H.InputState
  , compFunc    :: IORef (Maybe CompletionFunc)
  }

--------------------------------------------------------------------------------
-- | Reader environment for Byline.
newtype Byline m a = Byline {unByline :: ReaderT Env (MaybeT m) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

--------------------------------------------------------------------------------
defRenderMode :: H.InputT IO RenderMode
defRenderMode = do
  termHint <- H.haveTerminalUI

  let mode = case termHint of -- FIXME: consider TERM and TERMINFO
               False -> Plain
               True  -> Simple

  return mode

--------------------------------------------------------------------------------
-- | Create the default reader environment.
defEnv :: H.InputState -> RenderMode -> IORef (Maybe CompletionFunc) -> Env
defEnv state mode comp =
  Env { renderMode = mode
      , outHandle  = stdout
      , inputState = state
      , compFunc   = comp
      }

--------------------------------------------------------------------------------
eof :: (Monad m) => Byline m a
eof = Byline $ lift (MaybeT $ return Nothing)

--------------------------------------------------------------------------------
liftOuter :: (Monad m) => m a -> Byline m a
liftOuter = Byline . lift . lift

--------------------------------------------------------------------------------
-- | Lift an 'InputT' action into 'Byline'.
liftInputT :: (MonadIO m) => H.InputT IO a -> Byline m a
liftInputT input = do
  state <- asks inputState
  liftIO (H.queryInput state $ H.withInterrupt input)

--------------------------------------------------------------------------------
runByline :: (MonadIO m) => Byline m a -> m (Maybe a)
runByline (Byline byline) = do
  comp <- liftIO (newIORef Nothing)

  let settings = H.setComplete (runCompletionFunction comp) H.defaultSettings
  state <- liftIO (H.initializeInput settings)
  mode  <- liftIO (H.queryInput state defRenderMode)

  output <- runMaybeT $ runReaderT byline (defEnv state mode comp)
  liftIO (H.closeInput state)

  -- FIXME: Use a bracket in here
  return output
