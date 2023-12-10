module Main where

import Text.HTML.DOM as HTML
import Text.XML as X
import Data.HTML2CSS (html2css, el2stylist)
import Network.URI (nullURI)

import Data.CSS.Preprocessor.Conditions as CSSCond
import qualified Data.CSS.Preprocessor.PsuedoClasses as CSSPseudo
import qualified Data.CSS.Style as Style
import Stylist.Tree (StyleTree(..), preorder, treeMap)
import qualified Data.CSS.Preprocessor.Text as CSSTxt
import Data.Maybe (fromMaybe)

import Graphics.Layout.CSS.Font (placeholderFont)
import Graphics.Layout.CSS (finalizeCSS', CSSBox)
import Graphics.Layout (LayoutItem, boxLayout, glyphsPerFont)
import Graphics.Layout.Box (Length, Size(..), PaddedBox(..), zeroBox)

import Control.Exception (evaluate)
import qualified Graphics.Text.Font.Choose as FC

import Control.Concurrent.MVar (putMVar, newEmptyMVar, readMVar)
import Control.Concurrent (forkIO)
import Control.DeepSeq (NFData(..), ($!!))
--import System.Mem (performGC)

resolve' = CSSCond.resolve lowerVars lowerToks
lowerVars _ = CSSCond.B False
lowerToks _ = CSSCond.B False

stylize' style = preorder inner
  where
    inner parent _ el = Style.cascade style el [] $
            Style.inherit $ fromMaybe Style.temp parent

main :: IO ()
main = do
    FC.init
    doc <- HTML.readFile "test.html"
    let css' :: CSSCond.ConditionalStyles (Style.VarParser (CSSTxt.TextStyle
                (CSSBox Nil)))
        css' = html2css doc nullURI $ CSSCond.conditionalStyles nullURI "temp"
    css' `seq` print "Parsed page with CSS!"
    let pseudoFilter = CSSPseudo.htmlPsuedoFilter Style.queryableStyleSheet
    let css = CSSPseudo.inner $ resolve' pseudoFilter css'
    let styles = CSSTxt.resolve $ treeMap Style.innerParser $
            stylize' css $ el2stylist $ X.documentRoot doc
    styles `seq` print "Styled page!"
    let layout :: LayoutItem Length Length Nil
        layout = finalizeCSS' placeholderFont styles
    layout `seq` print "Laying out page!"
    res <- forkCompute $ boxLayout zeroBox { size = Size 1280 480 } layout False
    res' <- readMVar res
    case res' of
        (page:_) -> do
            print "Gathering atlas"
            evaluate $ glyphsPerFont page
            return ()
        _ -> return ()
    --performGC
    --FC.fini -- FIXME: GC still left FontConfig references...
    return ()

data Nil = Nil deriving Eq
instance Style.PropertyParser Nil where
    temp = Nil
    inherit _ = Nil
    longhand _ _ _ _ = Nothing
instance NFData Nil where rnf Nil = ()

forkCompute dat = do
    ret <- newEmptyMVar
    forkIO $ putMVar ret $!! dat
    return ret
