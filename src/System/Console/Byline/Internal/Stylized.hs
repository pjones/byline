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
       , text
       , fg
       , bg
       , bold
       , underline
       ) where

--------------------------------------------------------------------------------
import Data.Monoid
import Data.Text (Text)
import System.Console.Byline.Internal.Color

--------------------------------------------------------------------------------
data Status = On | Off

--------------------------------------------------------------------------------
newtype OnlyOne a = OnlyOne {unOne :: Maybe a}

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
text :: Text -> Stylized
text t = StylizedText t mempty

--------------------------------------------------------------------------------
fg :: Color -> Stylized
fg c = StylizedMod (mempty {modColorFG = OnlyOne (Just c)})

--------------------------------------------------------------------------------
bg :: Color -> Stylized
bg c = StylizedMod (mempty {modColorBG = OnlyOne (Just c)})

--------------------------------------------------------------------------------
bold :: Stylized
bold = StylizedMod (mempty {modBold = On})

--------------------------------------------------------------------------------
underline :: Stylized
underline = StylizedMod (mempty {modUnderline = On})

--------------------------------------------------------------------------------
instance Monoid Status where
  mempty = Off
  mappend Off Off = Off
  mappend Off On  = On
  mappend On  On  = On
  mappend On  Off = On

--------------------------------------------------------------------------------
instance Monoid (OnlyOne a) where
  mempty = OnlyOne Nothing
  mappend _ b = b

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
