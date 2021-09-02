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

import Byline.Simulation
import Test.Tasty
import Test.Tasty.HUnit

example :: MonadByline m => m Text
example = do
  sayLn ("Hey, I like " <> ("Haskell" & fg magenta) <> "!")

  let question =
        "What's "
          <> ("your" & bold)
          <> " favorite "
          <> ("language" & fg (vivid green) & underline)
          <> "? "

  askLn question (Just "Haskell")

main :: IO ()
main = defaultMain $ do
  testGroup
    "Byline"
    [ testCase "default input" $
        run "" example @?= Just "Haskell",
      testCase "user input" $
        run "Python" example @?= Just "Python",
      testCase "changing state" $ do
        runIdentity (runBylineT textThenDefault example) @?= Just "Current"
        runIdentity (runBylineT textThenDefault (example >> example)) @?= Just "Next"
    ]
  where
    run :: Text -> BylineT Identity Text -> Maybe Text
    run input action =
      let sim = pure (SimulatedInput input)
       in runBylineT sim action & runIdentity
    textThenDefault :: Monad m => SimulationFunction m
    textThenDefault = do
      -- The next input request will come from this function:
      modify (\s -> s {simulationFunction = pure (SimulatedInput "Next")})

      -- But this time we'll return different text:
      pure (SimulatedInput "Current")
