{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK hide #-}

{-

This file is part of the package byline. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/byline/LICENSE. No part of the
byline package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
-- | Internal module containing the @Byline@ monad transformer.
module System.Console.Byline.Internal.Byline
       ( Byline (..)
       , Env    (..)
       , eof
       , liftBase
       , liftInputT
       , runByline
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.IORef
import System.Environment (lookupEnv)
import System.IO (Handle, stdout)
import qualified System.Terminfo as Term
import qualified System.Terminfo.Caps as Term

--------------------------------------------------------------------------------
-- Import Haskeline, which is used to do the heavy lifting.
import qualified System.Console.Haskeline    as H
import qualified System.Console.Haskeline.IO as H

--------------------------------------------------------------------------------
-- Byline imports:
import System.Console.Byline.Internal.Completion
import System.Console.Byline.Internal.Render

--------------------------------------------------------------------------------
-- The following is a kludge to avoid the "redundant import" warning
-- when using GHC >= 7.10.x.  This should be removed after we decide
-- to stop supporting GHC < 7.10.x.
import Prelude

--------------------------------------------------------------------------------
-- | Reader environment for Byline.
data Env = Env
  { sayMode    :: RenderMode
  , askMode    :: RenderMode
  , outHandle  :: Handle
  , inputState :: H.InputState
  , compFunc   :: IORef (Maybe CompletionFunc)
  }

--------------------------------------------------------------------------------
-- | A monad transformer that encapsulates interactive actions.
newtype Byline m a = Byline {unByline :: ReaderT Env (MaybeT m) a}
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO)

--------------------------------------------------------------------------------
-- | Calculate the default rendering modes based on the terminal type.
defRenderMode :: H.InputT IO (RenderMode, RenderMode)
defRenderMode = do
  termHint  <- H.haveTerminalUI
  maxColors <- liftIO (runMaybeT getMaxColors)

  return $ case (termHint, maxColors) of
             (True, Just n) | n < 256   -> (Simple,  Simple)
                            | otherwise -> (Term256, Term256)
             (True, Nothing)            -> (Simple,  Plain)
             (False, _)                 -> (Plain,   Plain)
  where
    getMaxColors :: MaybeT IO Int
    getMaxColors = do
      term <- MaybeT (lookupEnv "TERM")
      db   <- liftIO (Term.acquireDatabase term)

      case db of
        Left _  -> MaybeT (return Nothing)
        Right d -> MaybeT (return $ Term.queryNumTermCap d Term.MaxColors)

--------------------------------------------------------------------------------
-- | Create the default reader environment.
defEnv :: H.InputState
       -> (RenderMode, RenderMode)
       -> IORef (Maybe CompletionFunc)
       -> Env
defEnv state (smode, amode) comp =
  Env { sayMode    = smode
      , askMode    = amode
      , outHandle  = stdout
      , inputState = state
      , compFunc   = comp
      }

--------------------------------------------------------------------------------
-- | Signal an EOF and terminate all Byline actions.
eof :: (Monad m) => Byline m a
eof = Byline $ lift (MaybeT $ return Nothing)

--------------------------------------------------------------------------------
-- | Lift an operation in the base monad into Byline.
liftBase :: (Monad m) => m a -> Byline m a
liftBase = Byline . lift . lift

--------------------------------------------------------------------------------
-- | Lift an 'InputT' action into 'Byline'.
liftInputT :: (MonadIO m) => H.InputT IO a -> Byline m a
liftInputT input = do
  state <- asks inputState
  liftIO (H.queryInput state $ H.withInterrupt input)

--------------------------------------------------------------------------------
-- | Execute 'Byline' actions and produce a result within the base monad.
--
-- /A note about EOF:/
--
-- If an End of File (EOF) is encountered during an input action then
-- this function will return @Nothing@.  This can occur when the user
-- manually enters an EOF character by pressing @Control-d@ or if
-- standard input is a file.
--
-- This decision was made to simplify the @Byline@ interface for
-- actions that read user input and is a typical strategy for terminal
-- applications.  If this isn't desirable, you may want to break your
-- actions up into groups and call 'runByline' multiple times.
runByline :: (MonadIO m, MonadMask m) => Byline m a -> m (Maybe a)
runByline (Byline byline) = do
  comp <- liftIO (newIORef Nothing)
  let settings = H.setComplete (runCompletionFunction comp) H.defaultSettings

  bracketOnError (liftIO $ H.initializeInput settings) -- Acquire.
                 (liftIO . H.cancelInput)              -- Release.
                 (go comp)                             -- Use.
  where
    go comp state = do
      modes  <- liftIO (H.queryInput state defRenderMode)
      output <- runMaybeT $ runReaderT byline (defEnv state modes comp)

      liftIO (H.closeInput state)
      return output
