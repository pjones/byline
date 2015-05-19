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
import System.Console.Byline

--------------------------------------------------------------------------------
data Item = Fruit Text | Vegetable Text deriving Show

--------------------------------------------------------------------------------
displayItem :: Item -> Stylized
displayItem (Fruit     name) = text name <> (" (fruit)"     <> fg red)
displayItem (Vegetable name) = text name <> (" (vegetable)" <> fg green)

--------------------------------------------------------------------------------
items :: [Item]
items = [ Fruit     "Watermelon"
        , Vegetable "Cucumber"
        , Fruit     "Kiwi"
        , Vegetable "Asparagus"
        ]

--------------------------------------------------------------------------------
main :: IO ()
main = do
  let menuConfig = banner "Pick a snack: " $ menu items displayItem
      prompt     = "Which snack? "
      onError    = "please pick a valid item!" <> fg red

  answer <- runByline $ askWithMenuRepeatedly menuConfig prompt onError
  putStrLn ("you picked: " ++ show answer)
