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
       , askChar
       , askPassword
       , askUntil
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
say :: (MonadIO m) => Stylized -> Byline m ()
say message = do
  env <- Reader.ask
  liftIO $ render (renderMode env) (outHandle env) message

--------------------------------------------------------------------------------
-- | Like 'say', but append a newline character.
sayLn :: (MonadIO m) => Stylized -> Byline m ()
sayLn message = say (message <> text "\n")

--------------------------------------------------------------------------------
-- | Read input after printing the given stylized text as a prompt.
ask :: (MonadException m)
    => Stylized                 -- ^ The prompt.
    -> Maybe Text               -- ^ Optional default answer.
    -> Byline m (Maybe Text)
ask prompt defans = do
  let prompt' = case defans of
                  Nothing -> prompt
                  Just s  -> prompt <> text "[" <> text s <> "] "

  answer <- liftInputT . getInputLine =<< renderPrompt prompt'
  return (T.pack <$> answer)


--------------------------------------------------------------------------------
-- | Read a single character of input.  Like other functions,
-- 'askChar' will return 'Nothing' if the user issues a Ctrl-d/EOF.
askChar :: (MonadException m)
        => Stylized
        -> Byline m (Maybe Char)
askChar prompt = liftInputT . getInputChar =<< renderPrompt prompt

--------------------------------------------------------------------------------
-- | Read a password without echoing it to the terminal.  If a masking
-- character is given it will replace each typed character.
askPassword :: (MonadException m)
            => Stylized            -- ^ The prompt.
            -> Maybe Char          -- ^ Masking character.
            -> Byline m (Maybe Text)
askPassword prompt maskchr = do
  pass <- liftInputT . getPassword maskchr =<< renderPrompt prompt
  return (T.pack <$> pass)

--------------------------------------------------------------------------------
-- | Continue to prompt for a response until a confirmation function
-- returns a valid result.
--
-- The confirmation function receives the output from 'ask' and should
-- return a 'Left Stylized' to produce an error message (printed with
-- 'sayLn').  When an acceptable answer from 'ask' is received, the
-- confirmation function should return it with 'Right'.
askUntil :: (MonadException m)
         => Stylized                             -- ^ The prompt.
         -> Maybe Text                           -- ^ Optional default answer.
         -> (Maybe Text -> Either Stylized Text) -- ^ Confirmation function.
         -> Byline m (Maybe Text)
askUntil prompt defans confirm = go where
  go = do
    answer <- ask prompt defans
    case confirm answer of
      Left msg     -> sayLn msg >> go
      Right result -> return (Just result)

--------------------------------------------------------------------------------
-- | Output stylized text with a prefix determined by 'ReportType'.
report :: (MonadIO m) => ReportType -> Stylized -> Byline m ()
report (Error) message   = say $ (text "error: "   <> fg red)    <> message
report (Warning) message = say $ (text "warning: " <> fg yellow) <> message

--------------------------------------------------------------------------------
-- | Like 'report', but append a newline character.
reportLn :: (MonadIO m) => ReportType -> Stylized -> Byline m ()
reportLn rt message = report rt (message <> text "\n")

--------------------------------------------------------------------------------
-- | Run the given 'Byline' action with a different completion
-- function.
withCompletionFunc :: (Monad m) => CompletionFunc m -> Byline m a -> Byline m a
withCompletionFunc comp byline =
  Byline $ Reader.local updateComp (unByline byline)

  where
    -- updateComp :: Env m -> Env m
    updateComp env = env { otherCompFunc = Just comp }

--------------------------------------------------------------------------------
renderPrompt :: (Monad m) => Stylized -> Byline m String
renderPrompt prompt = do
    mode <- Reader.asks renderMode
    return $ T.unpack (renderText mode prompt)
