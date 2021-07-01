{-# OPTIONS_HADDOCK hide #-}

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
module Byline.Internal.Types
  ( Color (..),
    Status (..),
    OnlyOne (..),
    Modifier (..),
  )
where

import qualified System.Console.ANSI as ANSI

-- | Opaque type for representing a color.
--
-- A color can be one of the eight standard terminal colors
-- constructed with one of the named color functions (e.g.,
-- 'Byline.black', 'Byline.red', etc.) or using the 'Byline.rgb'
-- function.
--
-- @since 1.0.0.0
data Color
  = ColorCode ANSI.ColorIntensity ANSI.Color
  | ColorRGB (Word8, Word8, Word8)
  deriving (Show, Eq)

-- | Like @Bool@, but with a different @Monoid@ instance.
--
-- @since 1.0.0.0
data Status = On | Off
  deriving (Show, Eq)

-- | @since 1.0.0.0
instance Semigroup Status where
  (<>) Off Off = Off
  (<>) Off On = On
  (<>) On On = On
  (<>) On Off = On

-- | @since 1.0.0.0
instance Monoid Status where
  mempty = Off

-- | Like @Maybe@, but with a different @Monoid@ instance.
--
-- @since 1.0.0.0
newtype OnlyOne a = OnlyOne {unOne :: Maybe a}
  deriving (Show, Eq)

-- | @since 1.0.0.0
instance Semigroup (OnlyOne a) where
  (<>) _ b@(OnlyOne (Just _)) = b
  (<>) a _ = a

-- | @since 1.0.0.0
instance Monoid (OnlyOne a) where
  mempty = OnlyOne Nothing

-- | Information about modifications made to stylized text.
--
-- @since 1.0.0.0
data Modifier = Modifier
  { modColorFG :: OnlyOne Color,
    modColorBG :: OnlyOne Color,
    modBold :: Status,
    modUnderline :: Status,
    modSwapFgBg :: Status
  }
  deriving (Show, Eq)

-- | @since 1.0.0.0
instance Semigroup Modifier where
  (<>) (Modifier a b c d e) (Modifier a' b' c' d' e') =
    Modifier (a <> a') (b <> b') (c <> c') (d <> d') (e <> e')

-- | @since 1.0.0.0
instance Monoid Modifier where
  mempty = Modifier mempty mempty mempty mempty mempty
