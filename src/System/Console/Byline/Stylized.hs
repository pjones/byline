{-

This file is part of the package byline. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/byline/LICENSE. No part of the
byline package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
-- | The stylized type and constructors.
module System.Console.Byline.Stylized
       ( Stylized
       , Modifier
       , text
       , mapStylized
       , modStylized
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Byline imports:
import System.Console.Byline.Internal.Types

--------------------------------------------------------------------------------
-- | Stylized text.  Construct text with modifiers using string
-- literals and the @OverloadedStrings@ extension and/or the 'text'
-- function.
data Stylized = StylizedText Text Modifier
              | StylizedMod Modifier
              | StylizedList [Stylized]

--------------------------------------------------------------------------------
-- | Helper function to create stylized text.  If you enable the
-- @OverloadedStrings@ extension then you can create stylized text
-- directly without using this function.
--
-- This function is also helpful for producing stylized text from an
-- existing @Text@ value.
text :: Text -> Stylized
text t = StylizedText t mempty

--------------------------------------------------------------------------------
-- | Map a function over stylized text.  The 'Modifier' type is
-- opaque so this function might not be very useful outside of the
-- Byline internals.
mapStylized :: ((Text, Modifier) -> a) -> Stylized -> [a]
mapStylized f (StylizedText t m) = [ f (t, m) ]
mapStylized _ (StylizedMod    _) = [ ] -- No op.
mapStylized f (StylizedList   l) = concatMap (mapStylized f) l

--------------------------------------------------------------------------------
-- | Constructor to modify stylized text.  This function is only
-- useful to internal Byline functions.
modStylized :: Modifier -> Stylized
modStylized = StylizedMod

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
