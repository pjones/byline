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
module Main (main) where

--------------------------------------------------------------------------------
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Console.Byline

--------------------------------------------------------------------------------
main :: IO ()
main = void $ runByline $ do

  -- Simple message to stdout:
  sayLn "Okay, let's kick this off"

  -- Now with some color:
  sayLn ("I can use " <> ("color" <> fg blue) <> " too!")

  -- Get user input with a stylized prompt:
  let question = "What's your favorite " <> ("language" <> bold) <> "? "
  language <- ask question Nothing

  if Text.null language
    then sayLn "Cat got your tongue?"
    else sayLn ("I see, you like " <> (text language <> fg red) <> ".")

  -- Keep prompting until a confirmation function indicates that the
  -- user's input is sufficient:
  name <- askUntil "What's your name? " Nothing atLeastThreeChars
  sayLn $ "Hey there " <> text name <> fg (rgb 108 113 196)

--------------------------------------------------------------------------------
-- | Example confirmation function that requires the input to be three
-- or more characters long.
atLeastThreeChars :: Text -> IO (Either Stylized Text)
atLeastThreeChars input = return $
  if Text.length input < 3
    then Left "You can do better."
    else Right input
