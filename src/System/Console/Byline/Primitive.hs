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
-- | Primitive operations such as printing messages and reading input.
module System.Console.Byline.Primitive
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
-- Library imports:
import Control.Monad.IO.Class
import qualified Control.Monad.Reader as Reader
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified System.Console.Haskeline as H

--------------------------------------------------------------------------------
--- Byline imports:
import System.Console.Byline.Color
import System.Console.Byline.Internal.Byline
import System.Console.Byline.Internal.Completion
import System.Console.Byline.Internal.Render
import System.Console.Byline.Modifiers
import System.Console.Byline.Stylized

--------------------------------------------------------------------------------
-- | Report types for the 'report' function.
data ReportType = Error    -- ^ Report errors with: @"error: "@
                | Warning  -- ^ Report warnings with: @"warning: "@

--------------------------------------------------------------------------------
-- | Output the stylized text to the output handle (default: stdout).
say :: (MonadIO m) => Stylized -> Byline m ()
say message = do
  env <- Reader.ask
  liftIO $ render (sayMode env) (outHandle env) message

--------------------------------------------------------------------------------
-- | Like 'say', but append a newline character.
sayLn :: (MonadIO m) => Stylized -> Byline m ()
sayLn message = say (message <> text "\n")

--------------------------------------------------------------------------------
-- | Read input after printing the given stylized text as a prompt.
ask :: (MonadIO m)
    => Stylized
       -- ^ The prompt.

    -> Maybe Text
    -- ^ Optional default answer that will be returned if the user
    -- presses return without providing any input (a zero-length
    -- string).

    -> Byline m Text
ask prompt defans = do
  let prompt' = case defans of
                  Nothing -> prompt
                  Just s  -> prompt <> text "[" <> text s <> "] "

  answer <- liftInputT . H.getInputLine =<< renderPrompt prompt'

  case answer of
    Nothing            -> eof
    Just s | null s    -> return (fromMaybe (T.pack s) defans)
           | otherwise -> return (T.pack s)

--------------------------------------------------------------------------------
-- | Read a single character of input.
askChar :: (MonadIO m)
        => Stylized
        -> Byline m Char
askChar prompt = do
  answer <- liftInputT . H.getInputChar =<< renderPrompt prompt
  case answer of
    Nothing -> eof
    Just c  -> return c

--------------------------------------------------------------------------------
-- | Read a password without echoing it to the terminal.  If a masking
-- character is given it will replace each typed character.
askPassword :: (MonadIO m)
            => Stylized
               -- ^ The prompt.

            -> Maybe Char
            -- ^ Optional masking character that will be printed each
            -- time the user presses a key.

            -> Byline m Text
askPassword prompt maskchr = do
  pass <- liftInputT . H.getPassword maskchr =<< renderPrompt prompt
  case pass of
    Nothing -> eof
    Just s  -> return (T.pack s)

--------------------------------------------------------------------------------
-- | Continue to prompt for a response until a confirmation function
-- returns a valid result.
--
-- The confirmation function receives the output from 'ask' and should
-- return a @Left Stylized@ to produce an error message (printed with
-- 'sayLn').  When an acceptable answer from 'ask' is received, the
-- confirmation function should return it with @Right@.
askUntil :: (MonadIO m)
         => Stylized                           -- ^ The prompt.
         -> Maybe Text                         -- ^ Optional default answer.
         -> (Text -> m (Either Stylized Text)) -- ^ Confirmation function.
         -> Byline m Text
askUntil prompt defans confirm = go where
  go = do
    answer <- ask prompt defans
    check  <- liftBase (confirm answer)

    case check of
      Left msg     -> sayLn msg >> go
      Right result -> return result

--------------------------------------------------------------------------------
-- | Output stylized text with a prefix determined by 'ReportType'.
report :: (MonadIO m) => ReportType -> Stylized -> Byline m ()
report Error message   = say $ (text "error: "   <> fg red)    <> message
report Warning message = say $ (text "warning: " <> fg yellow) <> message

--------------------------------------------------------------------------------
-- | Like 'report', but append a newline character.
reportLn :: (MonadIO m) => ReportType -> Stylized -> Byline m ()
reportLn rt message = report rt (message <> text "\n")

--------------------------------------------------------------------------------
-- | Run the given 'Byline' action with a different completion
-- function.
withCompletionFunc :: (MonadIO m) => CompletionFunc -> Byline m a -> Byline m a
withCompletionFunc comp byline = do
  compref <- Reader.asks compFunc
  current <- liftIO (readIORef compref)

  -- Temporally change the completion function.
  -- Exceptions will be dealt with in 'runByline'.
  liftIO (writeIORef compref (Just comp))
  output <- byline

  -- Reset the completion function and return the result.
  liftIO (writeIORef compref current)
  return output

--------------------------------------------------------------------------------
renderPrompt :: (Monad m) => Stylized -> Byline m String
renderPrompt prompt = do
    mode <- Reader.asks askMode
    return $ T.unpack (renderText mode prompt)
