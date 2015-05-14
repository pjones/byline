{-

This file is part of the package byline. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/byline/LICENSE. No part of the
byline package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module System.Console.Byline.Internal.Completion
       ( CompletionFunc
       , Completion (..)
       , runCompletionFunction
       ) where

--------------------------------------------------------------------------------
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified System.Console.Haskeline.Completion as H

--------------------------------------------------------------------------------
data Completion = Completion
  { replacement :: Text
  , display     :: Text
  , isFinished  :: Bool
  } deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
type CompletionFunc = (Text, Text) -> IO (Text, [Completion])

--------------------------------------------------------------------------------
convertCompletion :: Completion -> H.Completion
convertCompletion (Completion r d i) =
  H.Completion { H.replacement = Text.unpack r
               , H.display     = Text.unpack d
               , H.isFinished  = i
               }

--------------------------------------------------------------------------------
runCompletionFunction :: IORef (Maybe CompletionFunc) -> H.CompletionFunc IO
runCompletionFunction compref (left, right) = do
  comp <- readIORef compref

  case comp of
    Nothing -> H.completeFilename (left, right)
    Just f  -> do (output, completions) <- f (Text.pack left, Text.pack right)
                  return (Text.unpack output, map convertCompletion completions)
