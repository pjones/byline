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
import System.Console.Haskeline
import System.IO (Handle, stdout)

--------------------------------------------------------------------------------
data Env = Env
  { renderMode :: RenderMode
  , outHandle  :: Handle
  , hlSettings :: Settings IO
  }

--------------------------------------------------------------------------------
-- | Reader environment for Byline.
newtype Byline a = Byline {unByline :: ReaderT Env IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

--------------------------------------------------------------------------------
defEnv :: (MonadException m) => InputT m Env
defEnv = do
  termHint <- haveTerminalUI

  let mode = case termHint of -- FIXME: consider TERM and TERMINFO
               False -> Plain
               True  -> Simple

  return $ Env { renderMode = mode
               , outHandle  = stdout
               , hlSettings = defaultSettings
               }


--------------------------------------------------------------------------------
liftInputT :: InputT IO a -> Byline a
liftInputT input = do
  settings <- asks hlSettings
  liftIO $ runInputT settings input

--------------------------------------------------------------------------------
runByline :: (MonadException m) => Byline a -> m a
runByline byline = runInputT defaultSettings $ do
  env <- defEnv
  withInterrupt . liftIO $ runReaderT (unByline byline) env
