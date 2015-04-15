{-

This file is part of the package byline. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/byline/LICENSE. No part of the
byline package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
-- | FIXME: Holy crap this needs some major refactoring!
module System.Console.Byline.Internal.Render
       ( RenderMode (..)
       , render
       , renderText
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad (void)
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid
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
render mode h stylized  =
  case mode of
    Plain  -> mapStylized void renderPlain  stylized
    Simple -> mapStylized void renderSimple stylized
    Full   -> mapStylized void renderFull   stylized

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
-- | Render into a 'Text' value.  Won't work on Windows.
renderText :: RenderMode -> Stylized -> Text
renderText mode stylized = runIdentity $
  case mode of
    Plain  -> mapStylized (fmap mconcat) renderPlain stylized
    Simple -> mapStylized (fmap mconcat) renderSimple stylized
    Full   -> mapStylized (fmap mconcat) renderFull stylized

  where
    renderPlain :: (Text, Modifier) -> Identity Text
    renderPlain = return . fst

    renderSimple :: (Text, Modifier) -> Identity Text
    renderSimple (t, m) = return $ mconcat
      [ T.pack (setSGRCode $ modToSGR m)
      , t
      , T.pack (setSGRCode [Reset])
      ]

    renderFull :: (Text, Modifier) -> Identity Text
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
mapStylized :: (Monad m) => (m [a] -> m a) -> ((Text, Modifier) -> m a) -> Stylized -> m a
mapStylized _ g (StylizedText t m) = g (t, m)
mapStylized _ g (StylizedMod m)    = g (T.empty, m)
mapStylized f g (StylizedList l)   = f (sequence $ map (mapStylized f g) l)
