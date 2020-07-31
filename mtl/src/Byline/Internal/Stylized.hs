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
module Byline.Internal.Stylized
  ( Stylized (..),
    ToStylizedText (..),
    text,
    fg,
    bg,
    bold,
    underline,
    swapFgBg,
    RenderMode (..),
    render,
    renderText,
  )
where

import Byline.Internal.Color (Color)
import qualified Byline.Internal.Color as Color
import Byline.Internal.Types (Modifier (..), OnlyOne (..), Status (..))
import qualified Data.Text.IO as Text
import qualified System.Console.ANSI as ANSI

-- | A stylized value.  Construct text with modifiers using string
-- literals and the @OverloadedStrings@ extension and/or the 'text'
-- function.
--
-- @since 1.0.0.0
data Stylized a
  = -- | Something to stylize.
    Stylized Modifier a
  | -- | Modify the next stylized value.
    StylizedMod Modifier
  | -- | Multiple stylized values.
    StylizedList [Stylized a]
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | @since 1.0.0.0
instance Semigroup (Stylized a) where
  -- StylizedText on LHS.
  (<>) a@(Stylized _ _) b@(Stylized _ _) = StylizedList [a, b]
  (<>) (Stylized m t) (StylizedMod m') = Stylized (m <> m') t
  (<>) a@(Stylized _ _) (StylizedList b) = StylizedList (a : b)
  -- StylizedMod on LHS.
  (<>) (StylizedMod m) (Stylized m' t) = Stylized (m <> m') t
  (<>) (StylizedMod m) (StylizedMod m') = StylizedMod (m <> m')
  (<>) m@(StylizedMod _) (StylizedList l) = StylizedList (map (m <>) l)
  -- StylizedList on LHS.
  (<>) (StylizedList l) t@(Stylized _ _) = StylizedList (l <> [t])
  (<>) (StylizedList l) m@(StylizedMod _) = StylizedList (map (<> m) l)
  (<>) (StylizedList l) (StylizedList l') = StylizedList (l <> l')

-- | @since 1.0.0.0
instance Monoid (Stylized a) where
  mempty = StylizedList []

-- | @since 1.0.0.0
instance IsString (Stylized Text) where
  fromString = text . toText

-- | A class for types that can be converted to 'Stylized' text.
class ToStylizedText a where
  toStylizedText :: a -> Stylized Text

-- | @since 1.0.0.0
instance ToStylizedText (Stylized Text) where
  toStylizedText = id

-- | Helper function to create stylized text.  If you enable the
-- @OverloadedStrings@ extension then you can create stylized text
-- directly without using this function.
--
-- This function is also helpful for producing stylized text from an
-- existing @Text@ value.
--
-- @since 1.0.0.0
text :: Text -> Stylized Text
text = Stylized mempty

-- | Set the foreground color.  For example:
--
-- @
--     "Hello World!" <> fg magenta
-- @
--
-- @since 1.0.0.0
fg :: Color -> Stylized Text
fg c = StylizedMod (mempty {modColorFG = OnlyOne (Just c)})

-- | Set the background color.
--
-- @since 1.0.0.0
bg :: Color -> Stylized Text
bg c = StylizedMod (mempty {modColorBG = OnlyOne (Just c)})

-- | Produce bold text.
--
-- @since 1.0.0.0
bold :: Stylized Text
bold = StylizedMod (mempty {modBold = On})

-- | Produce underlined text.
--
-- @since 1.0.0.0
underline :: Stylized Text
underline = StylizedMod (mempty {modUnderline = On})

-- | Produce swapped foreground/background text.
--
-- @since 1.0.0.0
swapFgBg :: Stylized Text
swapFgBg = StylizedMod (mempty {modSwapFgBg = On})

-- | How to render stylized text.
--
-- @since 1.0.0.0
data RenderMode
  = -- | Text only, no modifiers.
    Plain
  | -- | Allow up to 8 colors.
    Simple
  | -- | Allow up to 216 colors.
    Term256
  | -- | A terminal that supports full RGB colors.
    TermRGB

-- | Instructions for formatting stylized text after the 'RenderMode'
-- has already been considered.
--
-- @since 1.0.0.0
data RenderInstruction
  = RenderText Text
  | RenderSGR [ANSI.SGR]

-- | Send stylized text to the given handle.
--
-- @since 1.0.0.0
render :: RenderMode -> Handle -> Stylized Text -> IO ()
render mode h stylized = mapM_ go (renderInstructions mode stylized)
  where
    go :: RenderInstruction -> IO ()
    go (RenderText t) = Text.hPutStr h t
    go (RenderSGR s) = ANSI.hSetSGR h s

-- | Render all modifiers as escape characters and return the
-- resulting text.  The text produced from this function is formatted
-- for output by Haskeline.
--
-- @since 1.0.0.0
renderText :: RenderMode -> Stylized Text -> Text
renderText mode stylized = foldMap go (renderInstructions mode stylized)
  where
    go :: RenderInstruction -> Text
    go = \case
      RenderText t -> t
      RenderSGR s ->
        -- NOTE: The \STX character below is not a real terminal
        -- escape character.  Instead it is intercepted by Haskeline.
        -- See: https://github.com/judah/haskeline/wiki/ControlSequencesInPrompt
        toText (ANSI.setSGRCode s) <> "\STX"

-- | Internal function to turn stylized text into render instructions.
--
-- @since 1.0.0.0
renderInstructions :: RenderMode -> Stylized Text -> [RenderInstruction]
renderInstructions mode = \case
  Stylized m t -> renderMod mode (t, m)
  StylizedMod _ -> []
  StylizedList xs -> concatMap (renderInstructions mode) xs
  where
    renderMod :: RenderMode -> (Text, Modifier) -> [RenderInstruction]
    renderMod mode (t, m) =
      case mode of
        Plain ->
          -- Only render text, no modifiers.
          [RenderText t]
        Simple ->
          -- Terminal supports basic 16 colors.
          let color l = ANSI.SetColor l ANSI.Dull . Color.colorAsANSI
           in renderToSGR t m color
        Term256 ->
          -- Terminal supports the 256-color palette.
          let color l = ANSI.SetPaletteColor l . Color.colorAsIndex256
           in renderToSGR t m color
        TermRGB ->
          -- Super terminal!
          let color l c = case Color.colorAsRGB c of
                Left ac -> ANSI.SetColor l ANSI.Dull ac
                Right rgb -> ANSI.SetRGBColor l rgb
           in renderToSGR t m color
    renderToSGR ::
      Text ->
      Modifier ->
      (ANSI.ConsoleLayer -> Color -> ANSI.SGR) ->
      [RenderInstruction]
    renderToSGR t m f =
      [ RenderSGR (modToSGR m f),
        RenderText t,
        RenderSGR [ANSI.Reset]
      ]

-- | Convert a modifier into a series of ANSI.SGR codes.
--
-- @since 1.0.0.0
modToSGR ::
  -- | The modifier to render as an SGR code.
  Modifier ->
  -- | A function that knows how to render colors.
  (ANSI.ConsoleLayer -> Color -> ANSI.SGR) ->
  -- | The resulting SGR codes.
  [ANSI.SGR]
modToSGR mod colorF =
  catMaybes
    [ colorF ANSI.Foreground <$> getColor modColorFG,
      colorF ANSI.Background <$> getColor modColorBG,
      ANSI.SetConsoleIntensity <$> getIntensity,
      ANSI.SetUnderlining <$> getUnderlining,
      ANSI.SetSwapForegroundBackground <$> getSwapForegroundBackground
    ]
  where
    getColor :: (Modifier -> OnlyOne Color) -> Maybe Color
    getColor f = unOne (f mod)
    getIntensity :: Maybe ANSI.ConsoleIntensity
    getIntensity = case modBold mod of
      Off -> Nothing
      On -> Just ANSI.BoldIntensity
    getUnderlining :: Maybe ANSI.Underlining
    getUnderlining = case modUnderline mod of
      Off -> Nothing
      On -> Just ANSI.SingleUnderline
    getSwapForegroundBackground :: Maybe Bool
    getSwapForegroundBackground = case modSwapFgBg mod of
      Off -> Nothing
      On -> Just True
