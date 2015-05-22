{-# LANGUAGE OverloadedStrings #-}
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
-- | Functions for turning stylized text into text or terminal commands.
module System.Console.Byline.Internal.Render
       ( RenderMode (..)
       , render
       , renderText
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Control.Applicative
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word
import System.Console.ANSI as ANSI
import System.IO (Handle, hPutStr)

--------------------------------------------------------------------------------
-- Byline imports:
import System.Console.Byline.Internal.Color as C
import System.Console.Byline.Internal.Types
import System.Console.Byline.Stylized

--------------------------------------------------------------------------------
-- The following is a kludge to avoid the "redundant import" warning
-- when using GHC >= 7.10.x.  This should be removed after we decide
-- to stop supporting GHC < 7.10.x.
import Prelude

--------------------------------------------------------------------------------
-- | How to render stylized text.
data RenderMode = Plain   -- ^ Text only, no modifiers.
                | Simple  -- ^ Allow up to 8 colors.
                | Term256 -- ^ Allow up to 216 colors.

--------------------------------------------------------------------------------
-- | Instructions for formatting stylized text after the 'RenderMode'
-- has already been considered.
data RenderInstruction = RenderText Text
                       | RenderSGR [SGR]

--------------------------------------------------------------------------------
-- | Send stylized text to the given handle.  This works on Windows
-- thanks to the @ansi-terminal@ package.
render :: RenderMode -> Handle -> Stylized -> IO ()
render mode h stylized = mapM_ go (renderInstructions mode stylized)
  where
    go :: RenderInstruction -> IO ()
    go (RenderText t) = hPutStr h (Text.unpack t)
    go (RenderSGR  s) = hSetSGR h s

--------------------------------------------------------------------------------
-- | Render all modifiers as escape characters and return the
-- resulting text.  On most terminals, sending this text to stdout
-- will correctly render the modifiers.  However, this won't work on
-- Windows consoles so you'll want to send 'Plain' as the 'RenderMode'.
renderText :: RenderMode -> Stylized -> Text
renderText mode stylized = Text.concat $ map go (renderInstructions mode stylized)
  where
    go :: RenderInstruction -> Text
    go (RenderText t) = t
    go (RenderSGR  _) = Text.empty

--------------------------------------------------------------------------------
-- | Internal function to turn stylized text into render instructions.
renderInstructions :: RenderMode -> Stylized -> [RenderInstruction]
renderInstructions mode = concat . mapStylized renderMod
  where
    renderMod :: (Text, Modifier) -> [RenderInstruction]
    renderMod (t, m) =
      case mode of
        -- Only rendering text.
        Plain -> [ RenderText t ]

        -- Render text with modifiers.  The only difference between
        -- 'Simple' and 'Term256' is handled by 'modToText'.
        _     -> [ RenderSGR  (modToSGR m)
                 , RenderText (modToText mode m)
                 , RenderText t
                 , RenderSGR [Reset]
                 ]

--------------------------------------------------------------------------------
-- | Convert a modifier into a series of SGR codes.
modToSGR :: Modifier -> [SGR]
modToSGR m =
  catMaybes [ SetColor Foreground Dull <$> modColor modColorFG
            , SetColor Background Dull <$> modColor modColorBG
            , SetConsoleIntensity      <$> modIntensity
            , SetUnderlining           <$> modUnderlining
            ]

  where
    modColor :: (Modifier -> OnlyOne C.Color) -> Maybe ANSI.Color
    modColor f = C.colorAsANSI <$> unOne (f m)

    modIntensity :: Maybe ConsoleIntensity
    modIntensity = case modBold m of
      Off -> Nothing
      On  -> Just BoldIntensity

    modUnderlining :: Maybe Underlining
    modUnderlining = case modUnderline m of
      Off -> Nothing
      On  -> Just SingleUnderline

--------------------------------------------------------------------------------
-- | Convert modifiers into direct escape sequences for modifiers
-- that can't be converted into SGR codes (e.g., RGB colors).
--
-- See: <http://en.wikipedia.org/wiki/ANSI_escape_code#Colors>
modToText :: RenderMode -> Modifier -> Text
modToText Plain _   = Text.empty
modToText Simple _  = Text.empty
modToText Term256 m =
  Text.concat $ catMaybes [ escape Foreground <$> modColor modColorFG
                          , escape Background <$> modColor modColorBG
                          ]

  where
    modColor :: (Modifier -> OnlyOne C.Color) -> Maybe (Word8, Word8, Word8)
    modColor f = case unOne (f m) of
                   Just (ColorRGB c) -> Just c
                   _                 -> Nothing

    -- Produce the correct CSI escape.
    escape :: ConsoleLayer -> (Word8, Word8, Word8) -> Text
    escape Foreground c = Text.concat ["\ESC[38;5;", colorIndex c, "m"]
    escape Background c = Text.concat ["\ESC[48;5;", colorIndex c, "m"]

    -- Return the 216-color index for (r, g, b).
    colorIndex :: (Word8, Word8, Word8) -> Text
    colorIndex c = Text.pack $ show (nearestColor c term256Locations)
