{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import System.Environment (getArgs)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as Txt
import qualified Data.ByteString as BS
import System.Directory (getCurrentDirectory)
import qualified System.Directory as Dir

import Graphics.Layout.CSS (CSSBox(..), finalizeCSS')
import Graphics.Layout.CSS.Font (placeholderFont)
import Graphics.Layout (LayoutItem, boxLayout, glyphsPerFont,
                        layoutGetBox, layoutGetChilds, layoutGetInner)
import Graphics.Layout.Box (zeroBox)
import qualified Graphics.Layout.Box as B

import Network.URI.Fetch.XML (Page(..), fetchDocument, applyCSScharset)
import Network.URI.Fetch (newSession, fetchURL)
import Network.URI.Charset (charsets)
import Network.URI (URI(..), nullURI, parseURIReference)
import Data.FileEmbed (makeRelativeToProject, embedStringFile)
import Data.HTML2CSS (el2stylist)

import Text.XML as X (Document(..), Element(..), Node(..), Prologue(..))
import Stylist.Tree (StyleTree(..), preorder, treeMap)
import Stylist (PropertyParser(..), cssPriorityAgent, cssPriorityUser)
import qualified Data.CSS.Style as Style
import qualified Data.CSS.Syntax.StyleSheet as CSS
import qualified Data.CSS.Preprocessor.Text as CSSTxt
import Data.CSS.Preprocessor.Conditions as CSSCond
        (ConditionalStyles, conditionalStyles, loadImports, Datum(..), resolve)
import qualified Data.CSS.Preprocessor.PsuedoClasses as CSSPseudo

import Control.Concurrent.MVar (putMVar, newEmptyMVar, tryReadMVar)
import Control.Concurrent (forkIO)
import Control.DeepSeq (NFData(..), ($!!))

import SDL hiding (rotate)
import Foreign.C.Types (CInt)
import Data.Function (fix)
import Control.Monad (unless)
import Control.Exception (evaluate)
import qualified Graphics.Text.Font.Choose as FC

initReferer :: IO (Page (CSSCond.ConditionalStyles (CSSBox Nil)))
initReferer = do
    cwd <- getCurrentDirectory
    return $ Page {
        -- Default to URIs being relative to CWD.
        pageURL = URI {uriScheme = "file:", uriPath = cwd,
            uriAuthority = Nothing, uriQuery = "", uriFragment = ""},
        -- Blank values:
        css = conditionalStyles nullURI "temp",
        domain = "temp",
        html = Document {
            documentPrologue = Prologue [] Nothing [],
            documentRoot = Element "temp" M.empty [],
            documentEpilogue = []
        },
        pageTitle = "", pageMIME = "", apps = [],
        backStack = [], forwardStack = [], visitedURLs = S.empty,
        initCSS = conditionalStyles,
        appName = "cattrap"
    }

stylize' style = preorder inner
  where
    inner parent _ el = Style.cascade style el [] $
            Style.inherit $ fromMaybe Style.temp parent

resolveCSS manager page = do
    let agentStyle = cssPriorityAgent (css page) `CSS.parse`
            $(makeRelativeToProject "app/useragent.css" >>= embedStringFile)
    userStyle <- loadUserStyles agentStyle
    CSSCond.loadImports loadURL lowerVars lowerToks userStyle []
  where
    loadURL url = do
        response <- fetchURL manager ["text/css"] url
        let charsets' = map Txt.unpack charsets
        return $ case response of
            ("text/css", Left text) -> text
            ("text/css", Right bytes) -> applyCSScharset charsets' $ BS.toStrict bytes
            (_, _) -> ""

loadUserStyles styles = do
    dir <- Dir.getXdgDirectory Dir.XdgConfig "rhapsode"
    exists <- Dir.doesDirectoryExist dir
    loadDirectory dir exists
  where
    loadDirectory _ False = return styles
    loadDirectory dir True = do
        files <- Dir.listDirectory dir
        loadFiles (cssPriorityUser styles) files
    loadFiles style (file:files) = do
        source <- readFile file
        CSS.parse style (Txt.pack source) `loadFiles` files
    loadFiles style [] = return style
-- FIXME: Support more media queries!
resolve' = CSSCond.resolve lowerVars lowerToks
lowerVars _ = CSSCond.B False
lowerToks _ = CSSCond.B False

main :: IO ()
main = do
    FC.init
    SDL.initializeAll

    let wcfg = defaultWindow {
            windowInitialSize = V2 1280 480,
            -- Simplify moving layout/download out-of-thread
            windowResizable = False
          }
    w <- createWindow "CatTrap" wcfg
    renderer <- createRenderer w (-1) defaultRenderer

    args <- getArgs
    let url = case args of
            (url:_) -> url
            [] -> "https://git.argonaut-constellation.org/~alcinnz/CatTrap"
    sess <- newSession
    ref <- initReferer
    xml <- fetchDocument sess ref $ fromMaybe nullURI $ parseURIReference url
    let pseudoFilter = CSSPseudo.htmlPsuedoFilter Style.queryableStyleSheet
    css' <- resolveCSS sess xml
    let css = CSSPseudo.inner $ resolve' pseudoFilter css'
    let styles = CSSTxt.resolve $ treeMap Style.innerParser $
            stylize' css $ el2stylist $ X.documentRoot $ html xml
    let layout = finalizeCSS' placeholderFont styles
    V2 x y <- get $ windowSize w
    pages' <- forkCompute $ boxLayout zeroBox {
            B.size = B.Size (fromIntegral x) (fromIntegral y)
          } layout False

    fix $ \loop -> do
        events <- fmap eventPayload <$> pollEvents
        rendererDrawColor renderer $= V4 255 255 255 255
        clear renderer

        pages <- tryReadMVar pages'
        case pages of
            Just (display:_) -> do
                evaluate $ glyphsPerFont display 
                renderDisplay renderer display
            _ -> return ()

        present renderer
        unless (QuitEvent `elem` events) loop
    SDL.quit
    -- FC.fini -- FIXME: Need to free all Haskell data before freeing FontConfig's

data Nil = Nil deriving Eq
instance PropertyParser Nil where
    temp = Nil
    inherit _ = Nil
    longhand _ _ _ _ = Nothing
instance NFData Nil where rnf Nil = ()


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

forkCompute dat = do
    ret <- newEmptyMVar
    forkIO $ putMVar ret $!! dat
    return ret
