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
module Main
  ( main,
  )
where

import           Byline
import           Byline.Shell
import qualified Data.Text           as Text
import qualified Options.Applicative as O

data Command
  = Help
  | Echo [Text]
  | SetPrompt Text

parser :: O.Parser Command
parser =
  O.hsubparser $
    mconcat
      [ O.command "help" (O.info (pure Help) $ O.progDesc "This message"),
        O.command "echo" (O.info echoP $ O.progDesc "Print all arguments"),
        O.command "set-prompt" (O.info promptP $ O.progDesc "Change the prompt")
      ]
  where
    echoP =
      Echo
        <$> many
          ( O.strArgument $
              mconcat
                [ O.metavar "STR",
                  O.help "A string to print"
                ]
          )
    promptP =
      SetPrompt
        <$> O.strArgument
          ( mconcat
              [ O.metavar "STR",
                O.help "Set the prompt to STR"
              ]
          )

dispatch ::
  MonadByline m =>
  MonadState (Shell Command) m =>
  Command ->
  m ()
dispatch = \case
  Help -> do
    shell <- get
    shellHelp shell
  Echo ts ->
    sayLn (text $ Text.intercalate " " ts)
  SetPrompt prompt ->
    modify' (\s -> s {shellPrompt = text prompt})

main :: IO ()
main = do
  let shell =
        Shell
          { shellPrefs = O.defaultPrefs,
            shellInfo = O.info parser (O.progDesc "Simple shell"),
            shellPrompt = "byline> "
          }
  void $ runBylineT (go shell)
  where
    go :: MonadByline m => Shell Command -> m ()
    go shell = do
      sayLn (text "Starting shell, use ^D to exit")
      pushCompletionFunction (shellCompletion shell)
      (`evalStateT` shell) $ forever (get >>= runShell dispatch)
