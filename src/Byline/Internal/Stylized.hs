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

-- | Stylized text.  Construct text with modifiers using string
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
fg :: Color -> Stylized a
fg c = StylizedMod (mempty {modColorFG = OnlyOne (Just c)})

-- | Set the background color.
--
-- @since 1.0.0.0
bg :: Color -> Stylized a
bg c = StylizedMod (mempty {modColorBG = OnlyOne (Just c)})

-- | Produce bold text.
--
-- @since 1.0.0.0
bold :: Stylized a
bold = StylizedMod (mempty {modBold = On})

-- | Produce underlined text.
--
-- @since 1.0.0.0
underline :: Stylized a
underline = StylizedMod (mempty {modUnderline = On})

-- | Produce swapped foreground/background text.
--
-- @since 1.0.0.0
swapFgBg :: Stylized a
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

-- | Instructions for formatting stylized text after the 'RenderMode'
-- has already been considered.
--
-- @since 1.0.0.0
data RenderInstruction
  = RenderText Text
  | RenderSGR [ANSI.SGR]

-- | Send stylized text to the given handle.  This works on Windows
-- thanks to the @ansi-terminal@ package.
--
-- @since 1.0.0.0
render :: RenderMode -> Handle -> Stylized Text -> IO ()
render mode h stylized = mapM_ go (renderInstructions mode stylized)
  where
    go :: RenderInstruction -> IO ()
    go (RenderText t) = Text.hPutStr h t
    go (RenderSGR s) = ANSI.hSetSGR h s

-- | Render all modifiers as escape characters and return the
-- resulting text.  On most terminals, sending this text to stdout
-- will correctly render the modifiers.  However, this won't work on
-- Windows consoles so you'll want to send 'Plain' as the
-- 'RenderMode'.
--
-- @since 1.0.0.0
renderText :: RenderMode -> Stylized Text -> Text
renderText mode stylized = foldMap go (renderInstructions mode stylized)
  where
    go :: RenderInstruction -> Text
    go (RenderText t) = t
    go (RenderSGR _) = mempty

-- | Internal function to turn stylized text into render instructions.
--
-- @since 1.0.0.0
renderInstructions :: RenderMode -> Stylized Text -> [RenderInstruction]
renderInstructions mode = \case
  Stylized m t -> renderMod (t, m)
  StylizedMod _ -> []
  StylizedList xs -> concatMap (renderInstructions mode) xs
  where
    renderMod :: (Text, Modifier) -> [RenderInstruction]
    renderMod (t, m) =
      case mode of
        -- Only rendering text.
        Plain -> [RenderText t]
        -- Render text with modifiers.  The only difference between
        -- 'Simple' and 'Term256' is handled by 'modToText'.
        Simple -> modToList (t, m)
        Term256 -> modToList (t, m)
    modToList :: (Text, Modifier) -> [RenderInstruction]
    modToList (t, m) =
      [ RenderSGR (modToSGR m),
        RenderText (modToText mode m),
        RenderText t,
        RenderSGR [ANSI.Reset]
      ]

-- | Convert a modifier into a series of ANSI.SGR codes.
--
-- @since 1.0.0.0
modToSGR :: Modifier -> [ANSI.SGR]
modToSGR m =
  catMaybes
    [ ANSI.SetColor ANSI.Foreground ANSI.Dull <$> modColor modColorFG,
      ANSI.SetColor ANSI.Background ANSI.Dull <$> modColor modColorBG,
      ANSI.SetConsoleIntensity <$> modIntensity,
      ANSI.SetUnderlining <$> modUnderlining,
      ANSI.SetSwapForegroundBackground <$> modSwapForegroundBackground
    ]
  where
    modColor :: (Modifier -> OnlyOne Color) -> Maybe ANSI.Color
    modColor f = Color.colorAsANSI <$> unOne (f m)
    modIntensity :: Maybe ANSI.ConsoleIntensity
    modIntensity = case modBold m of
      Off -> Nothing
      On -> Just ANSI.BoldIntensity
    modUnderlining :: Maybe ANSI.Underlining
    modUnderlining = case modUnderline m of
      Off -> Nothing
      On -> Just ANSI.SingleUnderline
    modSwapForegroundBackground :: Maybe Bool
    modSwapForegroundBackground = case modSwapFgBg m of
      Off -> Nothing
      On -> Just True

-- | Convert modifiers into direct escape sequences for modifiers
-- that can't be converted into ANSI.SGR codes (e.g., RGB colors).
--
-- See: <http://en.wikipedia.org/wiki/ANSI_escape_code#Colors>
--
-- @since 1.0.0.0
modToText :: RenderMode -> Modifier -> Text
modToText Plain _ = mempty
modToText Simple _ = mempty
modToText Term256 m =
  mconcat $
    catMaybes
      [ escape ANSI.Foreground <$> modColor modColorFG,
        escape ANSI.Background <$> modColor modColorBG
      ]
  where
    modColor :: (Modifier -> OnlyOne Color) -> Maybe (Word8, Word8, Word8)
    modColor f = case unOne (f m) of
      Just (Color.ColorRGB c) -> Just c
      _ -> Nothing
    -- Produce the correct CSI escape.
    escape :: ANSI.ConsoleLayer -> (Word8, Word8, Word8) -> Text
    escape ANSI.Foreground c = mconcat ["\ESC[38;5;", colorIndex c, "m"]
    escape ANSI.Background c = mconcat ["\ESC[48;5;", colorIndex c, "m"]
    -- Return the 216-color index for (r, g, b).
    colorIndex :: (Word8, Word8, Word8) -> Text
    colorIndex c = show (Color.nearestColor c Color.term256Locations)
