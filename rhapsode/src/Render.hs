{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Render(retreiveStyles, renderDoc, c_renderDoc) where

import qualified Data.ByteString.Lazy as B
import qualified Text.XML as XML
import           Data.Text as Txt (pack, unpack, Text(..), intercalate)

import qualified Data.Map as M
import System.Directory as Dir
import Data.FileEmbed

import Data.Maybe (fromMaybe, maybeToList)
import Text.Read (readMaybe)

--- External Rhapsode subcomponents
import qualified Data.CSS.Syntax.StyleSheet as CSS
import qualified Data.CSS.Style as Style
import           Data.CSS.StyleTree
import qualified Data.CSS.Syntax.Tokens as CSSTok
import qualified Data.CSS.Preprocessor.Conditions as CSSCond
import           Data.CSS.Preprocessor.Assets
import qualified Data.CSS.Preprocessor.PsuedoClasses as CSSPseudo
import qualified Data.CSS.Preprocessor.Text as CSSTxt
import           Stylist (cssPriorityAgent, cssPriorityUser, attrTest, elementPath)
import           Stylist.Tree (treeFind)
import           Data.HTML2CSS (el2stylist)

import           Network.URI
import           Network.URI.Fetch
import           Network.URI.Charset
import           Network.URI.Fetch.XML (applyCSScharset)

--- For CSS assets
import           Data.List (nub, elem)
import           Control.Concurrent.Async (forConcurrently)
import           System.IO.Temp
import           Control.Exception (catch)

--- For psuedoclasses
import qualified Data.Set as Set
import qualified Data.CSS.Syntax.Selector as CSSSel

-- Internal Rhapsode Subcomponents
import SpeechStyle
import SSML

-- C API
import Types
import Foreign.StablePtr
import Foreign.C.String
import Data.ByteString (useAsCString)

renderDoc :: Style.QueryableStyleSheet (Style.VarParser (CSSTxt.TextStyle SpeechStyle)) -> XML.Element -> B.ByteString
renderDoc style html = renderElLBS $ styleToSSML $ CSSTxt.resolve $
        treeMap Style.innerParser $ stylize' style $ el2stylist html

stylize' style = preorder inner
  where
    inner parent _ el = Style.cascade style el [] $
            Style.inherit $ fromMaybe Style.temp parent

renderElLBS el = XML.renderLBS XML.def $ XML.Document {
        XML.documentPrologue = XML.Prologue [] Nothing [],
        XML.documentRoot = el,
        XML.documentEpilogue = []
    }

retreiveStyles :: Session -> CSSCond.ConditionalStyles (CSSTxt.TextStyle SpeechStyle) -> IO (CSSCond.ConditionalStyles (CSSTxt.TextStyle SpeechStyle))
retreiveStyles manager authorStyle = do
    let agentStyle = cssPriorityAgent authorStyle `CSS.parse`
            $(makeRelativeToProject "useragent.css" >>= embedStringFile)
    userStyle <- loadUserStyles agentStyle
    CSSCond.loadImports loadURL lowerVars lowerToks userStyle []
  where
    loadURL url = do
        response <- fetchURL manager ["text/css"] url
        let charsets' = map unpack charsets
        return $ case response of
            ("text/css", Left text) -> text
            ("text/css", Right bytes) -> applyCSScharset charsets' $ B.toStrict bytes
            (_, _) -> ""

resolve' :: CSS.StyleSheet s => s -> CSSCond.ConditionalStyles (CSSTxt.TextStyle SpeechStyle) -> s
resolve' = CSSCond.resolve lowerVars lowerToks
lowerVars "speech" = CSSCond.B True
lowerVars "-rhapsode" = CSSCond.B True
lowerVars _ = CSSCond.B False
lowerToks _ = CSSCond.B False

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

parsePath ('.':anchor) = [] : parsePath anchor
parsePath (c:anchor) | n:path' <- parsePath anchor, c >= '0' && c <= '9' = (c:n):path'
parsePath [] = [[]]
parsePath _ = []

targetSel "" = [CSSTok.Ident "main"]
targetSel "#" = [CSSTok.Colon, CSSTok.Ident "root"]
targetSel ('#':'.':anchor) =
    CSSTok.Colon : CSSTok.Ident "root" : concat [ selLayer n | n <- parsePath anchor]
  where
    selLayer n = [
        CSSTok.Delim '>',
        CSSTok.Colon, CSSTok.Function "nth-child",
        CSSTok.Number (pack n) (CSSTok.NVInteger $ fromMaybe 0 $ readMaybe n),
        CSSTok.RightParen]
targetSel ('#':id) = [CSSTok.Hash CSSTok.HUnrestricted $ Txt.pack id]
targetSel _ = []

targetWithinSel _ "#" = []
targetWithinSel _ ('#':'.':anchor) = map (fromMaybe 0 . readMaybe) $ parsePath anchor
targetWithinSel tree ('#':id)
    | (el:_) <- treeFind tree $ attrTest Nothing "id" $ CSSSel.Include $ Txt.pack id =
        elementPath el
targetWithinSel _ _ = []

testVisited :: Set.Set Text -> URI -> String -> Bool
testVisited hist base val = uriToText url `Set.member` hist
  where
    url = fromMaybe nullURI (parseURIReference val) `relativeTo` base
    uriToText uri = pack $ uriToString id uri ""

rhapsodePseudoFilter url hist tree =
    -- Note: not all links must have an href tag, but it's not a bad approximation visited links must.
    -- Doing it this way is easier to implement in Haskell Stylist.
    CSSPseudo.addTest "visited" Nothing "href" (CSSSel.PropertyFunc $ testVisited hist url) $
    CSSPseudo.addRewrite "link" "[src], [href], details > summary, tr:first-of-type th" $
    CSSPseudo.addRewrite' "target" (targetSel $ uriFragment url) $
    CSSPseudo.addContains "target-within" (targetWithinSel tree $ uriFragment url) $
    CSSPseudo.htmlPsuedoFilter Style.queryableStyleSheet

--------
---- Download assets
--------

downloadAssets session mimes (StyleAssets _ assets) = do
    dir <- Dir.getXdgDirectory Dir.XdgCache "rhapsode"
    Dir.removeDirectoryRecursive dir `catch` ignoreError -- Clear cache.
    Dir.createDirectoryIfMissing True dir

    -- Ensure core audio cues are saved with predictable names for UI layer to use.
    let assets' = nub $ (u "about:link.wav":u "about:bulletpoint.wav":assets)
    fetchURLs session mimes assets' $ filterMIMEs mimes $ saveDownload nullURI dir
  where
    ignoreError :: IOError -> IO ()
    ignoreError _ = return ()
    u = fromMaybe (URI "about:" Nothing "invalid" "" "") . parseAbsoluteURI

filterMIMEs mimes cb download@(_, mime, _)
    | mime `elem` mimes = cb download
    | otherwise = return nullURI

--------
---- C API
--------

foreign export ccall c_renderDoc :: StablePtr Session -> StablePtr (Page RhapsodeCSS) -> Bool -> IO CString -- Hard to C bindings without IO

c_renderDoc c_session c_page rewriteURLs = do
    session <- deRefStablePtr c_session
    page <- deRefStablePtr c_page
    css' <- retreiveStyles session $ css page
    let html' = XML.documentRoot $ html page
    let pseudoFilter = rhapsodePseudoFilter (pageURL page) (visitedURLs page) (el2stylist html')
    qCSS <- if rewriteURLs then do
        assets <- downloadAssets session [
                "audio/vnd.wav"
            ] $ resolve' (StyleAssets ["cue-before", "cue-after", "cue"] []) css'
        let URIRewriter _ qCSS' =  resolve' (URIRewriter assets pseudoFilter) css'
        return $ CSSPseudo.inner qCSS'
        else return $ CSSPseudo.inner $ resolve' pseudoFilter css'
    let ssml = renderDoc qCSS $ XML.documentRoot $ html page
    B.toStrict ssml `useAsCString` \cstr -> do
        str <- peekCString cstr
        newCString str
