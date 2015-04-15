{-

This file is part of the package byline. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/byline/LICENSE. No part of the
byline package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}


--------------------------------------------------------------------------------
module System.Console.Byline.Internal.Stylized
       ( Stylized (..)
       , Modifier (..)
       , text
       ) where

--------------------------------------------------------------------------------
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import System.Console.Byline.Internal.Color
import System.Console.Byline.Internal.Types

--------------------------------------------------------------------------------
data Modifier = Modifier
  { modColorFG   :: OnlyOne Color
  , modColorBG   :: OnlyOne Color
  , modBold      :: Status
  , modUnderline :: Status
  }

--------------------------------------------------------------------------------
data Stylized = StylizedText Text Modifier
              | StylizedMod Modifier
              | StylizedList [Stylized]

--------------------------------------------------------------------------------
-- | Helper function to create stylized text.  If you enable the
-- 'OverloadedStrings' extension then you can create stylized text
-- directly without using this function.
text :: Text -> Stylized
text t = StylizedText t mempty

--------------------------------------------------------------------------------
instance Monoid Modifier where
  mempty = Modifier mempty mempty mempty mempty
  mappend (Modifier a b c d) (Modifier a' b' c' d') =
    Modifier (a <> a') (b <> b') (c <> c') (d <> d')

--------------------------------------------------------------------------------
instance Monoid Stylized where
  mempty = StylizedText mempty mempty

  -- StylizedText on LHS.
  mappend a@(StylizedText _ _) b@(StylizedText _ _) = StylizedList [a, b]
  mappend (StylizedText t m) (StylizedMod m')       = StylizedText t (m <> m')
  mappend a@(StylizedText _ _) (StylizedList b)     = StylizedList (a:b)

  -- StylizedMod on LHS.
  mappend (StylizedMod m) (StylizedText t m') = StylizedText t (m <> m')
  mappend (StylizedMod m) (StylizedMod m')    = StylizedMod (m <> m')
  mappend m@(StylizedMod _) (StylizedList l)  = StylizedList (map (m <>) l)

  -- StylizedList on LHS.
  mappend (StylizedList l) t@(StylizedText _ _) = StylizedList (l <> [t])
  mappend (StylizedList l) m@(StylizedMod _)    = StylizedList (map (<> m) l)
  mappend (StylizedList l) (StylizedList l')    = StylizedList (l <> l')

--------------------------------------------------------------------------------
instance IsString Stylized where
  fromString = text . T.pack
