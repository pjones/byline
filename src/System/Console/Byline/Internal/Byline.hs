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
       , liftInputT
       , runByline
       ) where


--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.Reader
import System.Console.Byline.Internal.Render
import System.IO (Handle, stdout)

--------------------------------------------------------------------------------
-- Import Haskeline, which is used to do the heavy lifting.
import qualified System.Console.Haskeline    as H
import qualified System.Console.Haskeline.IO as H

--------------------------------------------------------------------------------
data Env = Env
  { renderMode    :: RenderMode
  , outHandle     :: Handle
  , inputState    :: H.InputState
  , hlSettings    :: H.Settings IO
  , otherCompFunc :: Maybe (H.CompletionFunc IO)
  }

--------------------------------------------------------------------------------
-- | Reader environment for Byline.
newtype Byline m a = Byline {unByline :: ReaderT Env m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

--------------------------------------------------------------------------------
-- | Create the default reader environment.
defEnv :: H.InputState -> H.Settings IO -> H.InputT IO Env
defEnv state settings = do
  termHint <- H.haveTerminalUI

  let mode = case termHint of -- FIXME: consider TERM and TERMINFO
               False -> Plain
               True  -> Simple

  return Env { renderMode    = mode
             , outHandle     = stdout
             , hlSettings    = settings
             , inputState    = state
             , otherCompFunc = Nothing
             }

--------------------------------------------------------------------------------
-- | Lift an 'InputT' action into 'Byline'.
liftInputT :: (MonadIO m) => H.InputT IO a -> Byline m a
liftInputT input = do
  env <- ask

  -- let settings = case otherCompFunc env of
  --                  Nothing -> hlSettings env
  --                  Just f  -> setComplete f (hlSettings env)

  -- Byline $ lift (runInputT settings input)

  liftIO (H.queryInput (inputState env) $ H.withInterrupt input)

--------------------------------------------------------------------------------
runByline :: (MonadIO m) => Byline m a -> m a
runByline byline = do
  let settings = H.defaultSettings
  state <- liftIO (H.initializeInput settings)
  env   <- liftIO (H.queryInput state $ defEnv state settings)
  output <- runReaderT (unByline byline) env
  liftIO (H.closeInput state)
  return output
