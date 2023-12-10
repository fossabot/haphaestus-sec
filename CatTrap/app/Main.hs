{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import Text.XML.Light.Input (parseXMLDoc)
import qualified Text.XML.Light.Types as X
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as Txt
import Control.Monad (forM_, mapM)

import Graphics.Layout.CSS (CSSBox(..), finalizeCSS')
import Graphics.Layout.CSS.Font (placeholderFont)
import Graphics.Layout (LayoutItem, boxLayout,
                        layoutGetBox, layoutGetChilds, layoutGetInner)
import Graphics.Layout.Box (zeroBox)
import qualified Graphics.Layout.Box as B

import Stylist.Tree (StyleTree(..))
import Stylist (PropertyParser(..))
import Data.CSS.Syntax.Tokens (Token(..), tokenize)

import SDL hiding (rotate)
import Foreign.C.Types (CInt)
import Data.Function (fix)
import Control.Monad (unless)

main :: IO ()
main = do
    SDL.initializeAll

    let wcfg = defaultWindow {
            windowInitialSize = V2 640 480,
            windowResizable = True
          }
    w <- createWindow "CatTrap" wcfg
    renderer <- createRenderer w (-1) defaultRenderer

    args <- getArgs
    source <- readFile $ case args of
        (filename:_) -> filename
        [] -> "styletree.xml"
    let xml = fromJust $ parseXMLDoc source
    let styles = xml2styles temp xml
    let layout = finalizeCSS' placeholderFont styles

    fix $ \loop -> do
        events <- fmap eventPayload <$> pollEvents
        rendererDrawColor renderer $= V4 255 255 255 255
        clear renderer

        V2 x y <- get $ windowSize w
        let (display:_) = boxLayout zeroBox {
            B.size = B.Size (fromIntegral x) (fromIntegral y)
          } layout False
        renderDisplay renderer display

        present renderer
        unless (QuitEvent `elem` events) loop

xml2styles :: CSSBox Nil -> X.Element -> StyleTree (CSSBox Nil)
xml2styles parent el = StyleTree {
    style = self',
    children = [xml2styles self' child | X.Elem child <- X.elContent el]
  } where self' = foldl (applyStyle parent) temp $ X.elAttribs el

applyStyle parent style (X.Attr (X.QName name _ _) val) =
    fromMaybe style $ longhand parent style (Txt.pack name) $
        filter (/= Whitespace) $ tokenize $ Txt.pack val

data Nil = Nil deriving Eq
instance PropertyParser Nil where
    temp = Nil
    inherit _ = Nil
    longhand _ _ _ _ = Nothing

renderDisplay :: Renderer -> LayoutItem Double Double ((Double, Double), Nil)
        -> IO ()
renderDisplay renderer display = do
    let ((x, y), _) = layoutGetInner display
    let box = layoutGetBox display

    rendererDrawColor renderer $= V4 255 0 0 255
    drawBox renderer x y (B.width box) (B.height box)
    rendererDrawColor renderer $= V4 0 255 0 255
    drawBox renderer
        (x + B.left (B.margin box)) (y + B.top (B.margin box))
        (B.width box - B.left (B.margin box) - B.right (B.margin box))
        (B.height box - B.top (B.margin box) - B.bottom (B.margin box))
    rendererDrawColor renderer $= V4 0 0 255 255
    drawBox renderer
        (x + B.left (B.margin box) + B.left (B.border box))
        (y + B.top (B.margin box) + B.top (B.border box))
        (B.inline (B.size box) + B.left (B.padding box) + B.right (B.padding box))
        (B.block (B.size box) + B.top (B.padding box) + B.bottom (B.padding box))
    rendererDrawColor renderer $= V4 255 255 0 255
    drawBox renderer
        (x + B.left (B.margin box) + B.left (B.border box) + B.left (B.padding box))
        (y + B.top (B.margin box) + B.top (B.border box) + B.top (B.padding box))
        (B.inline $ B.size box) (B.block $ B.size box)

    mapM (renderDisplay renderer) $ layoutGetChilds display
    return ()

drawBox :: Renderer -> Double -> Double -> Double -> Double -> IO ()
drawBox renderer x y width height = do
    fillRect renderer $ Just $ Rectangle
        (P $ V2 (c x) (c y)) (V2 (c width) (c height))

c :: (Enum a, Enum b) => a -> b
c = toEnum . fromEnum
