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
  ( Stylized,
    askLn,
    askUntil,
    bg,
    blue,
    bold,
    fg,
    green,
    red,
    rgb,
    runBylineT,
    sayLn,
    text,
    underline,
  )
import qualified Data.Text as Text

main :: IO ()
main = void $ runBylineT $ do
  -- Simple message to stdout:
  sayLn "Okay, let's kick this off"

  -- Now with some color:
  sayLn ("I can use " <> ("color" <> fg blue) <> " too!")

  -- Get user input with a stylized prompt:
  let question = "What's your favorite " <> ("language" <> bold) <> "? "
  language <- askLn question Nothing

  if Text.null language
    then sayLn "Cat got your tongue?"
    else sayLn ("I see, you like " <> (text language <> fg red) <> ".")

  -- Keep prompting until a confirmation function indicates that the
  -- user's input is sufficient:
  let question = "What's your " <> ("name" <> fg green <> underline) <> "? "
  name <- askUntil question Nothing (pure . atLeastThreeChars)
  sayLn $ "Hey there " <> text name <> fg (rgb 108 113 196)

-- | Example confirmation function that requires the input to be three
-- or more characters long.
atLeastThreeChars :: Text -> Either (Stylized Text) Text
atLeastThreeChars input
  | Text.length input < 3 = Left ("You can do better." <> bg red)
  | otherwise = Right input
