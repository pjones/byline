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
import Data.Text (Text)
import qualified Data.Text as Text
import System.Console.Byline

--------------------------------------------------------------------------------
main :: IO ()
main = runByline $ do

  -- Simple message to stdout:
  sayLn "Okay, let's kick this off"

  -- Now with some color:
  sayLn ("I can use " <> ("color" <> fg blue) <> " too!")

  -- Get user input with a stylized prompt:
  let question = "What's your favorite " <> ("language" <> bold) <> "? "
  language <- ask question Nothing

  case language of
    Nothing -> sayLn "Cat got your tongue?"
    Just s  -> sayLn ("I see, you like " <> text s <> ".")

  -- Keep prompting until a confirmation function indicates that the
  -- user's input is sufficient:
  name <- askUntil "What's your name? " Nothing atLeastThreeChars

  case name of
    Nothing -> sayLn "You must have sent an EOF"
    Just s  -> sayLn ("Hey there " <> text s)

--------------------------------------------------------------------------------
-- | Example confirmation function that requires the input to be three
-- or more characters long.
atLeastThreeChars :: Maybe Text -> Either Stylized Text
atLeastThreeChars input = case input of
  Nothing -> Left msg
  Just s
   | Text.length s < 3 -> Left "You can do better."
   | otherwise         -> Right s

  where
    msg = "Hey, you have to enter something, " <>
          ("please" <> fg green <> underline)  <> "."
