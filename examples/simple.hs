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

import Byline
import qualified Byline.Exit as Exit
import qualified Data.Text as Text

main :: IO ()
main = void $
  runBylineT $ do
    -- Start with a simple message to standard output:
    sayLn ("I can use " <> ("color" & fg blue) <> "!")

    -- When not using any stylized modifiers you can use the `text'
    -- helper function to avoid "Ambiguous type variable":
    sayLn (text "This is some plain text")

    -- Get user input with a stylized prompt:
    let question =
          "What's your favorite "
            <> ("language" & bold & fg green)
            <> "? "
    language <- askLn question Nothing

    if Text.null language
      then Exit.die ("Cat got your tongue?" & fg (vivid magenta))
      else sayLn ("I see, you like " <> (text language & fg red) <> ".")

    -- Keep prompting until a confirmation function indicates that the
    -- user's input is sufficient:
    let question =
          "What's your "
            <> ("name" & fg green & underline)
            <> "? "
    name <- askUntil question Nothing (pure . atLeastThreeChars)
    sayLn $ "Hey there " <> text name & fg (rgb 108 113 196)

-- | Example confirmation function that requires the input to be three
-- or more characters long.
atLeastThreeChars :: Text -> Either (Stylized Text) Text
atLeastThreeChars input
  | Text.length input < 3 = Left ("You can do better." & bg red)
  | otherwise = Right input
