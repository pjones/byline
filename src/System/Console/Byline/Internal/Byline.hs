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
data Env m = Env
  { renderMode :: RenderMode
  , outHandle  :: Handle
  , hlSettings :: Settings m
  }

--------------------------------------------------------------------------------
-- | Reader environment for Byline.
newtype Byline m a = Byline {unByline :: ReaderT (Env m) m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Env m))

--------------------------------------------------------------------------------
-- | Create the default reader environment.
defEnv :: (MonadException m) => InputT m (Env m)
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
-- | Lift an 'InputT' action into 'Byline'.
liftInputT :: (MonadException m) => InputT m a -> Byline m a
liftInputT input = do
  settings <- asks hlSettings
  Byline (lift $ runInputT settings input)

--------------------------------------------------------------------------------
runByline :: (MonadException m) => Byline m a -> m a
runByline byline = runInputT defaultSettings $ do
  env <- defEnv
  withInterrupt . lift $ runReaderT (unByline byline) env
