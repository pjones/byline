{-

This file is part of the package byline. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/byline/LICENSE. No part of the
byline package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}


--------------------------------------------------------------------------------
module System.Console.Byline.Internal.Render
       ( RenderMode (..)
       , render
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import System.Console.ANSI as ANSI
import System.Console.Byline.Internal.Color as C
import System.Console.Byline.Internal.Stylized
import System.Console.Byline.Internal.Types
import System.IO (Handle, hPutStr)

--------------------------------------------------------------------------------
data RenderMode = Plain  -- ^ Text only, no modifiers.
                | Simple -- ^ Allow up to 8 colors.
                | Full   -- ^ Allow all colors and modifiers.

--------------------------------------------------------------------------------
render :: RenderMode -> Handle -> Stylized -> IO ()
render mode h = mapStylized $
  case mode of
    Plain  -> renderPlain
    Simple -> renderSimple
    Full   -> renderFull

  where
    renderPlain :: (Text, Modifier) -> IO ()
    renderPlain = hPutStr h . T.unpack . fst

    renderSimple :: (Text, Modifier) -> IO ()
    renderSimple (t, m) = do
      hSetSGR h (modToSGR m)
      hPutStr h (T.unpack t)
      hSetSGR h [Reset]

    renderFull :: (Text, Modifier) -> IO ()
    renderFull = undefined

--------------------------------------------------------------------------------
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
mapStylized :: ((Text, Modifier) -> IO ()) -> Stylized -> IO ()
mapStylized f (StylizedText t m) = f (t, m)
mapStylized f (StylizedMod m)    = f (T.empty, m)
mapStylized f (StylizedList l)   = sequence_ $ map (mapStylized f) l
