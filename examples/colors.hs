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

-- | Simple example.
example :: MonadByline m => m ()
example = do
  let colors = [black, red, yellow, green, blue, cyan, magenta, white]

  let w  = text "byline"
  let fgd = mconcat $ (w &) . fg <$> colors
  let fgv = mconcat $ (w &) . fg . vivid <$> colors
  let bgd = swapFgBg fgd 
  let bgv = swapFgBg fgv
  
  -- show foreground colors
  sayLn fgd
  sayLn fgv
  sayLn $ fgv & underline
  sayLn $ fgv & bold
  sayLn $ fgv & underline & bold
  
  -- show background colors
  sayLn bgd
  sayLn bgv
  sayLn $ bgv & underline
  sayLn $ bgv & bold
  sayLn $ bgv & underline & bold

-- | Main.
main :: IO ()
main = runBylineT example >> pure ()