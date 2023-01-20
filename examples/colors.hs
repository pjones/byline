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
import Control.Monad

-- | Simple example.
example :: MonadByline m => m ()
example = do
  let colors = [black, red, yellow, green, blue, cyan, magenta, white]
  let styles = [id, underline, bold, bold . underline]
  let intensity = [id, vivid]
  let fgbg = [id, swapFgBg]
  let mods = [ (text "byline" &) <$> [fb . fg (i c) . s | c <- colors] | fb <- fgbg, i <- intensity, s <- styles]
  mapM_ (sayLn . fold) mods

-- | Main.
main :: IO ()
main = Control.Monad.void (runBylineT example)