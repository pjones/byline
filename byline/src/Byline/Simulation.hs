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
module Byline.Simulation
  ( -- * Simulating User Interaction
    -- $use

    -- * Simulated Values
    Simulated (..),
    SimulationFunction,

    -- * Access to Simulation State
    SimulationState (..),

    -- * Simulation as a Monad Transformer
    BylineT,
    runBylineT,

    -- * Re-exports
    module Byline,
  )
where

import Byline hiding (BylineT, runBylineT)
import Byline.Internal.Simulation

-- $use
--
-- This module provides a monad transformer that can simulate an
-- interactive user session for testing 'MonadByline' code.
