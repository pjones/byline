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
module Byline.Exit
  ( -- * Exiting with style
    die,

    -- * Re-exports
    module Byline,
  )
where

import Byline
import Byline.Internal.Eval (defaultRenderMode)
import Byline.Internal.Stylized (render)
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO
import Prelude hiding (die)

-- | Exit the current process after printing a pretty error message.
--
-- This function is similar to 'Exit.die' except that the name of the
-- current process along with a 'Stylized' error message is printed to
-- the standard error handle before exiting with a failure code.
--
-- @since 1.0.0.0
die :: (MonadIO m, ToStylizedText a) => a -> m b
die a = liftIO $ do
  name <- Environment.getProgName <&> toText

  let msg =
        mconcat
          [ text name,
            ": ",
            toStylizedText a,
            "\n"
          ]

  mode <- defaultRenderMode stderr
  render mode stderr msg
  IO.hFlush stderr
  Exit.exitFailure
