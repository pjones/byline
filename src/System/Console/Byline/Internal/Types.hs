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

--------------------------------------------------------------------------------
-- Byline imports:
import System.Console.Byline.Color (Color)

--------------------------------------------------------------------------------
-- | Like @Bool@, but with a different @Monoid@ instance.
data Status = On | Off

--------------------------------------------------------------------------------
instance Monoid Status where
  mempty = Off
  mappend Off Off = Off
  mappend Off On  = On
  mappend On  On  = On
  mappend On  Off = On

--------------------------------------------------------------------------------
-- | Like @Maybe@, but with a different @Monoid@ instance.
newtype OnlyOne a = OnlyOne {unOne :: Maybe a}

--------------------------------------------------------------------------------
instance Monoid (OnlyOne a) where
  mempty = OnlyOne Nothing
  mappend _ b@(OnlyOne (Just _)) = b
  mappend a _                    = a

--------------------------------------------------------------------------------
-- | Information about modifications made to stylized text.
data Modifier = Modifier
  { modColorFG   :: OnlyOne Color
  , modColorBG   :: OnlyOne Color
  , modBold      :: Status
  , modUnderline :: Status
  }

--------------------------------------------------------------------------------
instance Monoid Modifier where
  mempty = Modifier mempty mempty mempty mempty
  mappend (Modifier a b c d) (Modifier a' b' c' d') =
    Modifier (a <> a') (b <> b') (c <> c') (d <> d')
