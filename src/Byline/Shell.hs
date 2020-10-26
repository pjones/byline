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
--
-- Interactive shells.
module Byline.Shell
  ( -- * Shell-like Interfaces
    Shell (..),
    runShell,
    shellHelp,
    shellCompletion,

    -- * Re-exports
    module Byline.Completion,
  )
where

import           Byline
import           Byline.Completion
import qualified Data.Attoparsec.Text       as Atto
import           Data.Char
import qualified Data.Text                  as Text
import qualified Options.Applicative        as O
import qualified Options.Applicative.Common as O
import qualified Options.Applicative.Types  as O
import           Relude.Extra.Map

-- | A type that describes how to process user-entered shells.
--
-- @since 1.1.0.0
data Shell a = Shell
  { -- | Optparse-applicative parser preferences.  If you don't have
    -- any specific needs you can use 'O.defaultPrefs' to get the
    -- default parser preferences.
    shellPrefs  :: O.ParserPrefs,
    -- | The shell parser wrapped in a 'O.ParserInfo'.  This is
    -- generally created with the 'O.info' function.
    shellInfo   :: O.ParserInfo a,
    -- | The prompt to display.
    shellPrompt :: Stylized Text
  }

-- | Run a single iteration of the shell.
--
-- @since 1.1.0.0
runShell ::
  MonadByline m =>
  (a -> m ()) ->
  Shell a ->
  m ()
runShell dispatch Shell {..} = do
  input <- askLn shellPrompt Nothing
  words <- shellSplit input
  unless (null words) (go (map toString words))
  where
    go words = do
      case O.execParserPure shellPrefs shellInfo words of
        O.Success a ->
          dispatch a
        O.Failure help -> do
          let str = fst (O.renderFailure help "")
          sayLn (text $ toText str)
        O.CompletionInvoked _ ->
          pure ()

-- | Print a list of shell commands.
--
-- @since 1.1.0.0
shellHelp ::
  MonadByline m =>
  Shell a ->
  m ()
shellHelp Shell {..} = do
  let h = O.parserFailure shellPrefs shellInfo O.ShowHelpText mempty
      s = fst (O.renderFailure h "")
  sayLn (text $ toText s)

-- | A completion function for shell commands.
--
-- Adds completion for subcommand names and their flags.
--
-- @since 1.1.0.0
shellCompletion :: Applicative m => Shell a -> CompletionFunc m
shellCompletion shell input@(left, _) = do
  if Text.null left || Text.all (isSpace >>> not) left
    then completionFromList CompHead (keys commands) input
    else completionFromList CompTail flags input
  where
    -- Get a list of flags for the current subcommand.
    flags :: [Text]
    flags = fromMaybe [] $ do
      cmd <- Text.words left & viaNonEmpty head
      names <- lookup cmd commands
      pure $
        flip map names $ \case
          O.OptShort c -> toText ['-', c]
          O.OptLong s -> "--" <> toText s

    -- A map of command names and their flags.
    commands :: HashMap Text [O.OptName]
    commands =
      fromList $
        concat $
          O.mapParser
            (const nameAndFlags)
            (O.infoParser $ shellInfo shell)
      where
        nameAndFlags opt =
          case O.optMain opt of
            O.CmdReader _ cmds p -> (`map` cmds) $ \cmd ->
              ( toText cmd,
                maybe
                  []
                  ( O.infoParser
                      >>> O.mapParser (const optnames)
                      >>> concat
                  )
                  (p cmd)
              )
            _ -> mempty
        optnames opt =
          case O.optMain opt of
            O.OptReader ns _ _ -> ns
            O.FlagReader ns _  -> ns
            _                  -> mempty

-- | Internal function to split user input into words similar to what
-- a POSIX shell does.
shellSplit :: MonadByline m => Text -> m [Text]
shellSplit t =
  let input = Text.strip t
   in if Text.null input
        then pure []
        else case Atto.parseOnly go input of
          Left e -> do
            sayLn (("invalid input" <> fg red) <> ": " <> text (toText e))
            pure []
          Right ws ->
            pure ws
  where
    go :: Atto.Parser [Text]
    go = Atto.many1 (bare <|> quoted) <* expectEndOfInput

    expectEndOfInput :: Atto.Parser ()
    expectEndOfInput = (Atto.endOfInput <|>) $ do
      leftover <- Atto.many1 Atto.anyChar
      fail ("unexpected input: " <> leftover)

    -- A bare word (not wrapped in quotes).
    bare :: Atto.Parser Text
    bare = (Atto.<?> "unquoted word") $ do
      word <- Atto.many1 bareChar
      void (Atto.many1 Atto.space) <|> Atto.endOfInput
      pure (toText word)

    -- A run of characters that may have quoted characters.
    --
    -- Just like with the POSIX shell, the quotes don't have to be on
    -- the outsides of the final string.
    quoted :: Atto.Parser Text
    quoted = do
      prefix <- many bareChar
      quote <- Atto.satisfy (\c -> c == '\'' || c == '"') Atto.<?> "quote"
      (_, ScanState {..}) <-
        Atto.runScanner (ScanState [] False) (quoteScanner quote)
      when scanEscape (fail "expecting a character after a backslash")
      _ <- Atto.char quote Atto.<?> "closing quotation character"
      let str = toText prefix <> toText (reverse scanResult)
      end <-
        (Atto.many1 Atto.space $> True)
          <|> (Atto.endOfInput $> True)
          <|> pure False
      if end then pure str else (str <>) <$> quoted

    -- Parse a single character that might be escaped.
    bareChar :: Atto.Parser Char
    bareChar = do
      char <-
        Atto.satisfy
          ( \c ->
              not (isSpace c)
                && c /= '\''
                && c /= '"'
                && isPrint c
          )
      if char == '\\'
        then Atto.anyChar Atto.<?> "escaped character"
        else pure char

-- | State needed to scan input looking for a closing quote.
data ScanState = ScanState
  { scanResult :: [Char],
    scanEscape :: Bool
  }

-- | A scanning function that looks for a terminating quote.
quoteScanner ::
  -- | The quote character we are searching for.
  Char ->
  -- | The output of the last invocation.
  ScanState ->
  -- | The current input character.
  Char ->
  -- | 'Just' to continue, 'Nothing' to stop.
  Maybe ScanState
quoteScanner quote ScanState {..} input
  | scanEscape = Just (ScanState (input : scanResult) False)
  | input == '\\' = Just (ScanState scanResult True)
  | input == quote = Nothing
  | otherwise = Just (ScanState (input : scanResult) False)
