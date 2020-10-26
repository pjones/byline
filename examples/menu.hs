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

import           Byline.Menu
import qualified Data.List.NonEmpty as NonEmpty

-- | Menu items that we'll ask the user to choose from.
data Item
  = Fruit Text
  | Vegetable Text
  deriving (Show)

-- | How to display a menu item.
instance ToStylizedText Item where
  toStylizedText item = case item of
    Fruit name     -> text name <> (" (fruit)" <> fg red)
    Vegetable name -> text name <> (" (vegetable)" <> fg green)

-- | The list of menu items.
items :: NonEmpty Item
items =
  NonEmpty.fromList
    [ Fruit "Watermelon",
      Vegetable "Cucumber",
      Fruit "Kiwi",
      Vegetable "Asparagus"
    ]

-- | It's main!
main :: IO ()
main = do
  let menuConfig =
        menuBanner ("Pick a snack: " <> bold) $
          menu items
      prompt = "Which snack? " <> bold <> fg yellow
      onError = "Please pick a valid item!" <> fg red

  -- Display the menu and get back the item the user selected.  The
  -- user will be able to select an item using it's index, name, or
  -- using tab completion.
  answer <-
    runBylineT $
      askWithMenuRepeatedly menuConfig prompt onError

  putStrLn ("You picked: " ++ show answer)
