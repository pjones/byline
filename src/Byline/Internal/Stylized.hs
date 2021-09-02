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
  | -- | Multiple stylized values.
    StylizedList [Stylized a]
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | @since 1.0.0.0
instance Semigroup (Stylized a) where
  -- StylizedText on LHS.
  (<>) a@(Stylized _ _) b@(Stylized _ _) = StylizedList [a, b]
  (<>) a@(Stylized _ _) (StylizedList b) = StylizedList (a : b)
  -- StylizedList on LHS.
  (<>) (StylizedList l) t@(Stylized _ _) = StylizedList (l <> [t])
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
-- directly without using this function.  However, if you are not
-- using any of the other stylized modifiers then this function can be
-- helpful for avoiding "Ambiguous type variable" compile errors.
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
fg :: Color -> Stylized Text -> Stylized Text
fg c (Stylized m t) = Stylized (m {modColorFG = OnlyOne (Just c)}) t
fg c (StylizedList l) = StylizedList (map (fg c) l)

-- | Set the background color.
--
-- @since 1.0.0.0
bg :: Color -> Stylized Text -> Stylized Text
bg c (Stylized m t) = Stylized (m {modColorBG = OnlyOne (Just c)}) t
bg c (StylizedList l) = StylizedList (map (bg c) l)

-- | Produce bold text.
--
-- @since 1.0.0.0
bold :: Stylized Text -> Stylized Text
bold (Stylized m t) = Stylized (m {modBold = On}) t
bold (StylizedList l) = StylizedList (map bold l)

-- | Produce underlined text.
--
-- @since 1.0.0.0
underline :: Stylized Text -> Stylized Text
underline (Stylized m t) = Stylized (m {modUnderline = On}) t
underline (StylizedList l) = StylizedList (map underline l)

-- | Produce swapped foreground/background text.
--
-- @since 1.0.0.0
swapFgBg :: Stylized Text -> Stylized Text
swapFgBg (Stylized m t) = Stylized (m {modSwapFgBg = On}) t
swapFgBg (StylizedList l) = StylizedList (map swapFgBg l)

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
          let color l c = case c of
                Color.ColorCode ai ac -> ANSI.SetColor l ai ac
                rgb -> ANSI.SetColor l ANSI.Dull (Color.colorAsANSI rgb)
           in renderToSGR t m color
        Term256 ->
          -- Terminal supports the 256-color palette.
          let color l = ANSI.SetPaletteColor l . Color.colorAsIndex256
           in renderToSGR t m color
        TermRGB ->
          -- Super terminal!
          let color l c = case Color.colorAsRGB c of
                Left (ai,ac) -> ANSI.SetColor l ai ac
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
