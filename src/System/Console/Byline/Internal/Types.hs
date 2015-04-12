{-

This file is part of the package byline. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/byline/LICENSE. No part of the
byline package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}


--------------------------------------------------------------------------------
module System.Console.Byline.Internal.Types
       ( Status  (..)
       , OnlyOne (..)
       ) where

--------------------------------------------------------------------------------
import Data.Monoid

--------------------------------------------------------------------------------
data Status = On | Off

--------------------------------------------------------------------------------
instance Monoid Status where
  mempty = Off
  mappend Off Off = Off
  mappend Off On  = On
  mappend On  On  = On
  mappend On  Off = On

--------------------------------------------------------------------------------
newtype OnlyOne a = OnlyOne {unOne :: Maybe a}

--------------------------------------------------------------------------------
instance Monoid (OnlyOne a) where
  mempty = OnlyOne Nothing
  mappend _ b@(OnlyOne (Just _)) = b
  mappend a _                    = a
