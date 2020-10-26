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
example :: MonadByline m => m Text
example = do
  sayLn ("Hey, I like " <> ("Haskell" <> fg magenta) <> "!")

  let question =
        "What's "
          <> ("your" <> bold)
          <> " favorite "
          <> ("language" <> fg green <> underline)
          <> "? "

  askLn question (Just "Haskell")

-- | Main.
main :: IO ()
main = runBylineT example >>= print
