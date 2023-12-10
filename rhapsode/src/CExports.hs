{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module CExports where

import Types
import Network.URI.Fetch (Session)

import Render (c_renderDoc)
import Links (c_extractLinks)

import qualified Text.XML as XML
import           Text.XML (Document(..), Prologue(..), Element(..))
import qualified Data.Text as Txt
import qualified Data.Text.Lazy as Txt (fromStrict)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B

import           Network.URI
import           Network.URI.Fetch
import           Network.URI.Fetch.XML (fetchDocument, loadVisited, readStrict)

import System.IO
import System.Directory
import System.FilePath ((</>))
import Data.FileEmbed
import Data.CSS.Preprocessor.Conditions (conditionalStyles)

-- Types I can export to C
import Foreign.StablePtr
import Foreign.C.String
import Foreign.Marshal.Array
import Data.Maybe (fromMaybe)

-- FIXME: Segfaults, was intended for a playlist feature
--foreign export ccall c_fetchURLs :: StablePtr Session -> PtrPage -> CArray CString -> IO (CArray PtrPage)
--
--c_fetchURLs c_session c_referer c_srcs = do
--    session <- deRefStablePtr c_session
--    referer <- deRefStablePtr $ castPtrToStablePtr c_referer
--    nil <- newCString ""
--    srcCStrs <- peekArray0 nil c_srcs
--    srcs <- forConcurrently (pairs srcCStrs) $ \(c_mime, c_uri) -> do
--        mime <- peekCString c_mime
--        uri <- peekCString c_uri
--        return $ pair (words mime) <$> parseURIReference uri
--    ret <- fetchURLs session (html referer) (url referer) $ catMaybes srcs
--    c_ret <- forM ret $ \(url, html, css) ->
--        newStablePtr $ Page url css html
--    newArray0 nullPtr $ map castStablePtrToPtr c_ret
--
--pairs (a:b:c) = (a, b):pairs c
--pairs _ = []
--pair a b = (a, b)

-- FIXME: Segfaults, was intended for the sake of easy concurrency.
foreign export ccall c_docLinksAndRendering :: StablePtr Session -> StablePtr (Page RhapsodeCSS) -> Bool -> CString -> IO (CArray CString)

c_docLinksAndRendering c_session c_page rewriteUrls c_v2jProfile = do
    c_links <- c_extractLinks c_page c_v2jProfile
    ssml <- c_renderDoc c_session c_page rewriteUrls
    -- (c_links, ssml) <- c_extractLinks c_page `concurrently` c_renderDoc c_session c_page rewriteUrls
    nil <- newCString ""
    links <- peekArray0 nil c_links
    newArray0 nil (ssml : links)

-- Since I have XML Conduit here...
ssmlHasMark :: Txt.Text -> XML.Element -> Bool
ssmlHasMark ident (XML.Element "mark" attrs _) = Just ident == M.lookup "name" attrs
ssmlHasMark ident (XML.Element _ _ childs) = or [ssmlHasMark ident el | XML.NodeElement el <- childs]

foreign export ccall c_ssmlHasMark :: CString -> CString -> IO Bool

c_ssmlHasMark c_ident c_ssml = do
    ident <- peekCString c_ident
    ssml <- peekCString c_ssml
    case XML.parseText XML.def $ Txt.fromStrict $ Txt.pack ssml of
        Left _ -> return False
        Right doc -> return $ ssmlHasMark (Txt.pack ident) $ XML.documentRoot doc

foreign export ccall c_initialReferer :: IO (StablePtr (Page RhapsodeCSS))

c_initialReferer = do
    cwd <- getCurrentDirectory
    hist <- loadVisited "rhapsode"
    newStablePtr $ Page {
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
        backStack = [], forwardStack = [], visitedURLs = hist,
        initCSS = conditionalStyles,
        appName = "rhapsode"
    }

foreign export ccall c_freePage :: StablePtr (Page RhapsodeCSS) -> IO ()

c_freePage = freeStablePtr

--------
---- Network requests
--------
foreign export ccall c_newSession :: IO (StablePtr Session)
foreign export ccall c_freeSession :: StablePtr Session -> IO ()

c_newSession = do
    sess <- newSession
    newStablePtr $ sess {
        aboutPages = map lazify $(makeRelativeToProject "about" >>= embedDir)
    }
  where lazify (a, b) = (a, B.fromStrict b)
c_freeSession = freeStablePtr


foreign export ccall c_fetchURL :: StablePtr Session -> CString -> StablePtr (Page RhapsodeCSS) -> CString -> IO (StablePtr (Page RhapsodeCSS))

c_fetchURL c_session c_mimes c_referer c_uri = do
    session <- deRefStablePtr c_session
    mimes <- peekCString c_mimes
    referer <- deRefStablePtr c_referer
    uri <- peekCString c_uri
    let uri' = nullURI `fromMaybe` parseURIReference uri `relativeTo` pageURL referer
    doc <- fetchDocument session referer uri'
    newStablePtr doc

foreign export ccall c_enableLogging :: StablePtr Session -> IO (StablePtr Session)

c_enableLogging c_session = do
    ret <- deRefStablePtr c_session >>= enableLogging
    freeStablePtr c_session
    newStablePtr ret

foreign export ccall c_writeLog :: CString -> StablePtr Session -> IO ()

c_writeLog c_path c_session = do
    path <- peekCString c_path
    withFile path AppendMode (\logfile -> deRefStablePtr c_session >>= writeLog logfile)

foreign export ccall c_lastVisited :: CString -> IO CString
c_lastVisited def = do
    path <- (</> "history.gmni") <$> getXdgDirectory XdgData "rhapsode"
    exists <- doesFileExist path
    if not exists then return def else do
        file <- readStrict path
        case map words $ reverse $ lines file of
            (_:url:_):_ -> newCString url
            _ -> return def
