{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the package byline. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/byline/LICENSE. No part of the
byline package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}


--------------------------------------------------------------------------------
module System.Console.Byline.Primary
       ( ReportType (..)
       , say
       , sayLn
       , ask
       , report
       , reportLn
       , withCompletionFunc
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.IO.Class
import qualified Control.Monad.Reader as Reader
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import System.Console.Byline.Internal.Byline
import System.Console.Byline.Internal.Color
import System.Console.Byline.Internal.Modifiers
import System.Console.Byline.Internal.Render
import System.Console.Byline.Internal.Stylized
import System.Console.Haskeline

--------------------------------------------------------------------------------
-- | Report types for the 'report' function.
data ReportType = Error    -- ^ Report errors with: @"error: "@
                | Warning  -- ^ Report warnings with: @"warning: "@

--------------------------------------------------------------------------------
-- | Output the stylized text to the output handle (default: stdout).
say :: Stylized -> Byline ()
say message = do
  env <- Reader.ask
  liftIO $ render (renderMode env) (outHandle env) message

--------------------------------------------------------------------------------
-- | Like 'say', but append a newline character.
sayLn :: Stylized -> Byline ()
sayLn message = say (message <> text "\n")

--------------------------------------------------------------------------------
-- | Read input after printing the given stylized text as a prompt.
ask :: Stylized -> Byline (Maybe Text)
ask prompt = do
  mode <- Reader.asks renderMode
  result <- liftInputT . getInputLine . T.unpack . renderText mode $ prompt
  return (T.pack <$> result)

--------------------------------------------------------------------------------
-- | Output stylized text with a prefix determined by 'ReportType'.
report :: ReportType -> Stylized -> Byline ()
report (Error) message   = say $ (text "error: "   <> fg red)    <> message
report (Warning) message = say $ (text "warning: " <> fg yellow) <> message

--------------------------------------------------------------------------------
-- | Like 'report', but append a newline character.
reportLn :: ReportType -> Stylized -> Byline ()
reportLn rt message = report rt (message <> text "\n")

--------------------------------------------------------------------------------
-- | FIXME: remove IO so that CompletionFunc can be in any monad.
withCompletionFunc :: CompletionFunc IO -> Byline a -> Byline a
withCompletionFunc comp byline =
  Byline $ Reader.local updateComp (unByline byline)

  where
    updateComp :: Env -> Env
    updateComp env = env { hlSettings = setComplete comp (hlSettings env) }
