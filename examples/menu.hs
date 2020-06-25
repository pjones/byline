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

-- | Menu items that we'll ask the user to choose from.
data Item
  = Fruit Text
  | Vegetable Text
  deriving (Show)

-- | How to display a menu item.
displayItem :: Item -> Stylized Text
displayItem (Fruit name) = text name <> (" (fruit)" <> fg red)
displayItem (Vegetable name) = text name <> (" (vegetable)" <> fg green)

-- | The list of menu items.
items :: [Item]
items =
  [ Fruit "Watermelon",
    Vegetable "Cucumber",
    Fruit "Kiwi",
    Vegetable "Asparagus"
  ]

-- | It's main!
main :: IO ()
main = do
  let menuConfig = menuBanner "Pick a snack: " $ menu items displayItem
      prompt = "Which snack? "
      onError = "Please pick a valid item!" <> fg red

  -- Display the menu and get back the item the user selected.  The
  -- user will be able to select an item using it's index, name, or
  -- using tab completion.
  answer <-
    runBylineT Nothing $
      askWithMenuRepeatedly menuConfig prompt onError

  putStrLn ("You picked: " ++ show answer)
