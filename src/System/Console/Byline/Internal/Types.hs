{-# OPTIONS_HADDOCK hide #-}

{-

This file is part of the package byline. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/byline/LICENSE. No part of the
byline package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
-- | Internal types.
module System.Console.Byline.Internal.Types
       ( Status   (..)
       , OnlyOne  (..)
       , Modifier (..)
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Data.Monoid
import qualified Data.Semigroup as Semi

--------------------------------------------------------------------------------
-- Byline imports:
import System.Console.Byline.Color (Color)

--------------------------------------------------------------------------------
-- | Like @Bool@, but with a different @Monoid@ instance.
data Status = On | Off

--------------------------------------------------------------------------------
instance Semi.Semigroup Status where
  (<>) Off Off = Off
  (<>) Off On  = On
  (<>) On  On  = On
  (<>) On  Off = On

--------------------------------------------------------------------------------
instance Monoid Status where
  mempty = Off
  mappend = (Semi.<>)

--------------------------------------------------------------------------------
-- | Like @Maybe@, but with a different @Monoid@ instance.
newtype OnlyOne a = OnlyOne {unOne :: Maybe a}

--------------------------------------------------------------------------------
instance Semi.Semigroup (OnlyOne a) where
  (<>) _ b@(OnlyOne (Just _)) = b
  (<>) a _                    = a

--------------------------------------------------------------------------------
instance Monoid (OnlyOne a) where
  mempty = OnlyOne Nothing
  mappend = (Semi.<>)

--------------------------------------------------------------------------------
-- | Information about modifications made to stylized text.
data Modifier = Modifier
  { modColorFG   :: OnlyOne Color
  , modColorBG   :: OnlyOne Color
  , modBold      :: Status
  , modUnderline :: Status
  , modSwapFgBg  :: Status
  }

--------------------------------------------------------------------------------
instance Semi.Semigroup Modifier where
  (<>) (Modifier a b c d e) (Modifier a' b' c' d' e') =
    Modifier (a <> a') (b <> b') (c <> c') (d <> d') (e <> e')

--------------------------------------------------------------------------------
instance Monoid Modifier where
  mempty = Modifier mempty mempty mempty mempty mempty
  mappend = (Semi.<>)
