{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Links(extractLinks, linkToText, Link(..), c_extractLinks) where

import Text.XML
import qualified Data.Map as M
import Network.MIME.Info as MIME
import Network.URI
import Data.Text (Text, unpack, append, pack, replace, strip)
import qualified Data.Text.Lazy as LTxt
import qualified Data.Text.Foreign as FTxt
import Data.Maybe
import Text.Read (readMaybe)

-- FIXME: Expose this API from HURL XML...
import Network.URI.Fetch.XML.Table (splitTable)

import Types
import Foreign.StablePtr
import Foreign.C.String
import Foreign.Marshal.Array
import Control.Monad (forM)
import Control.Exception (catch, IOException)

import System.Directory -- For locating links.xml, suggestions.gmni
import System.FilePath
import System.IO (hPrint, stderr, hGetContents) -- For error reporting, Voice2Json

-- For suggestions.gmni
import qualified Data.Set as Set
import Data.List (nub, intercalate)
import Control.Concurrent (forkIO)

-- For Voice2Json
import Data.Char
import System.Process
import Data.Aeson
import qualified Data.Aeson.KeyMap as HM
import qualified Data.ByteString.Lazy as LBS

data Link = Link {
    label :: Text,
    title :: Text,
    href :: URI
}

linkToText :: Link -> Text
linkToText (Link label' title' href') =
    rmWs label' +++ "\t" +++ rmWs title' +++ "\t" +++ pack (show href')

rmWs text = strip $ replace "\t" " " $ replace "\n" " " text

extractLinks :: Document -> [Link]
extractLinks (Document prologue root misc) =
    extractMisc (prologueBefore prologue) ++ extractMisc (prologueAfter prologue) ++
    extractEl [] root ++ extractMisc misc

extractMisc :: [Miscellaneous] -> [Link]
extractMisc (MiscInstruction (Instruction target dat):misc)
    | Just uri <- parseURIReference $ unpack target = Link dat "" uri : extractMisc misc
extractMisc (_:misc) = extractMisc misc
extractMisc [] = []

extractEl path el@(Element (Name "details" _ _) _ childs) =
   [Link (nodesText summary' $ nodesText childs "") "+" nullURI {
        uriFragment = '#':'.':intercalate "." (map show $ reverse path)
    } | NodeElement summary@(Element (Name "summary" _ _) _ summary') <- childs] ++
    extractNodes (0:path) childs
-- Special case for showing Appstream metadata of compatible apps.
-- Fallback for incompatible package manager UIs.
extractEl _ (Element "{https://specifications.freedesktop.org/metainfo/1.0}url" attrs childs)
    | Just label <- "{https://specifications.freedesktop.org/metainfo/1.0}type" `M.lookup` attrs,
      Just url <- parseAbsoluteURI $ unpack $ nodesText childs "" = [Link label "" url]
extractEl _ el@(Element _ attrs [])
    | Just "alternate" <- "rel" `M.lookup` attrs', Just typ <- "type" `M.lookup` attrs',
            Just val <- "href" `M.lookup` attrs', Just uri <- parseURIReference $ unpack val =
--        let Application name _ title _ = mimeInfo $ unpack typ
--        in [Link (pack name) (pack title) uri] -- FIXME `mimeInfo` freezes...
        [Link typ "" uri]
  where attrs' = M.mapKeys nameLocalName attrs
extractEl _ el@(Element (Name "link" _ _) attrs [])
    | Just "stylesheet" <- "rel" `M.lookup` attrs', Nothing <- "title" `M.lookup` attrs',
            Just val <- "href" `M.lookup` attrs', Just uri <- parseURIReference $ unpack val =
        let Application name _ title _ = mimeInfo "text/css"
        in [Link (pack name) (pack title) uri]
  where attrs' = M.mapKeys nameLocalName attrs
extractEl path (Element (Name "table" _ _) _ childs) =
    extractTable path (splitTable childs) ++ extractNodes (0:path) childs
extractEl path el@(Element _ _ children) =
    extractElAttr el "href" ++
    extractElAttr el "longdesc" ++
    extractElAttr el "src" ++
    extractNodes (0:path) children

extractElAttr (Element _ attrs children) attr
        | Just val <- attr `M.lookup` attrs',
            Just uri <- parseURIReference $ unpack val = [Link label' title' uri]
        | otherwise = []
    where
        label' = nodesText children $ M.findWithDefault "" "rel" attrs'
        title' = fromMaybe "" $ M.lookup "title" attrs'
        attrs' = M.mapKeys nameLocalName attrs

extractTable path (thead, _, _) = extractTable' path [el | NodeElement el <- thead]
extractTable' path (Element (Name "thead" _ _) _ childs:_) =
    extractTable' path [el | NodeElement el <- childs]
extractTable' path (Element (Name "tr" _ _) _ childs:_) = extractTR path 0 childs
extractTable' path (_:els) = extractTable' path els
extractTable' _ [] = []

extractTR path count (NodeElement (Element (Name name _ _) attrs childs):nodes)
    | name `elem` ["th", "td"] =
        extractTH path count ordering childs : extractTR path count' nodes
  where
    count' = count + fromMaybe 1 colspan
    colspan = readMaybe =<< unpack <$> M.lookup "colspan" attrs'
    ordering = M.lookup "aria-sort" attrs'
    attrs' = M.mapKeys nameLocalName attrs
extractTR path count (_:nodes) = extractTR path count nodes
extractTR _ _ [] = []
extractTH path count ordering nodes = Link {
        label = nodesText nodes "",
        title = pack $ show count,
        href = nullURI {
            uriFragment = '#':'-':'a':'r':'g':'o':'-':'%':
                intercalate "." [show n | n <- path] ++ o ordering : show count
        }
      }
    where
        o (Just "ascending") = '>'
        o _ = '<'

extractNodes p@(n:path) (NodeElement el:nodes) = extractEl p el ++ extractNodes (succ n:path) nodes
extractNodes path (NodeInstruction instruct:nodes) =
    extractMisc [MiscInstruction instruct] ++ extractNodes path nodes
extractNodes path (_:nodes) = extractNodes path nodes
extractNodes _ [] = []

(+++) = append
nodesText :: [Node] -> Text -> Text
nodesText (NodeElement (Element _ attrs children):nodes) def =
    nodesText children def +++ nodesText nodes def
nodesText (NodeContent text:nodes) def = text +++ nodesText nodes def
nodesText (_:nodes) def = nodesText nodes def
nodesText [] def = def

linksFromPage :: Page RhapsodeCSS -> [Link]
linksFromPage Page {
        pageURL = url',
        pageTitle = title',
        html = html',
        apps = apps',
        backStack = back', forwardStack = forward'
    } =
        [link' n desc $ URI "app:" Nothing id "" "" | Application n _ desc id <- apps'] ++
        extractLinks html'

head' (a:_) = [a]
head' [] = []
link' l t h = Link (pack l) (pack t) h

readBookmarks :: IO Document
readBookmarks = do
    dir <- getXdgDirectory XdgData "rhapsode"
    let file = dir </> "links.xml"
    exists <- doesFileExist file

    if exists then return () else do
        -- Copy defaults into userdir
        dirs <- getXdgDirectoryList XdgDataDirs
        files' <- forM dirs $ \dir' -> do
            let file' = dir' </> "rhapsode" </> "links.xml"
            exists' <- doesFileExist file'
            return $ if exists' then Just file' else Nothing
        case catMaybes files' of
            [] -> return ()
            (file':_) -> copyFileWithMetadata file' file

    exists' <- doesFileExist file
    if exists' then Text.XML.readFile def file `catch` handleInvalid else nodoc
  where
    handleInvalid err@(InvalidXMLFile _ _) = hPrint stderr err >> nodoc
    nodoc = return $ Document (Prologue [] Nothing []) (Element "empty" M.empty []) []

-- | Write out a file of most frequently encountered unvisited links.
-- Hopefully this'll help surfers rely less on YouTube, et al's hueristics.
updateSuggestions :: Page RhapsodeCSS -> IO ()
updateSuggestions page = do
    let links = extractLinks $ html page
    let domain = maybe "" show $ uriAuthority $ pageURL page

    dir <- getXdgDirectory XdgData "rhapsode"
    let path = dir </> "suggestions.gmni"
    exists <- doesFileExist path
    suggestions <- if not exists then return [] else do
        file <- readStrict path
        return [line' | line <- lines file, line'@(_:uri':_) <- [words line], not (pack uri' `Set.member` visitedURLs page)]

    let suggestions' = suggestions ++ nub [["=>", uri', domain] | link <- links,
            let uri' = uriToString id (href link) "", not (pack uri' `Set.member` visitedURLs page)]

    createDirectoryIfMissing True dir
    Prelude.writeFile path $ unlines $ map unwords suggestions'

-------
--- Voice2Json language models
-------

-- | Output links to a Voice2Json sentences.ini grammar.
outputSentences _ "" = return ()
outputSentences links dir = do
        Prelude.writeFile (dir </> "sentences.ini") $ unlines sentences
        createProcess (proc "voice2json" ["--profile", dir, "train-profile"]){ std_err = NoStream, std_out = NoStream }
        return ()
      `catch` \(_ :: IOException) -> return () -- Assume the UI has already warned Voice2Json isn't available.
    where
      sentences = "[links]" : [
            unwords $ words $ map validChar line -- Enforce valid sentences.ini syntax.
            | line@(_:_) <- map (unpack . label) links ++ map (unpack . title) links ++ map (show . href) links
        ]
      -- | Can this character appear in a sentences.ini rule without escaping?
      validChar ch | not (isAscii ch) || isSpace ch || isAlphaNum ch = ch
      validChar _ = ' '

------
--- C API
------
foreign export ccall c_extractLinks :: StablePtr (Page RhapsodeCSS) -> CString -> IO (CArray CString)

c_extractLinks c_page c_v2jProfile = do
    page <- deRefStablePtr c_page
    v2jProfile <- peekCString c_v2jProfile
    forkIO $ updateSuggestions page -- background process for useful navigation aid.
    bookmarks <- readBookmarks
    let links = linksFromPage page ++ extractLinks bookmarks
    forkIO $ outputSentences links v2jProfile
    ret <- forM links $ \link -> do
        c_label <- text2cstring $ strip $ label link
        c_title <- text2cstring $ strip $ title link
        c_href <- newCString $ uriToString id (href link) ""
        return [c_label, c_title, c_href]
    nil <- newCString " "
    newArray0 nil $ concat ret

text2cstring txt = FTxt.withCStringLen txt $ \s -> (peekCStringLen s >>= newCString)

------
--- C helper functions
------

foreign export ccall c_formatLink :: CString -> CString -> CString -> IO CString

c_formatLink c_label c_title c_url = do
    label <- ctext c_label
    title <- ctext c_title
    url <- ctext c_url

    sfx <- getXdgDirectory XdgCache "rhapsode"
    let bulletpoint_el = audio (sfx </> "bulletpoint.wav")
    let label_el = prosody [("pitch", "low")] label
    let title_el = prosody [("volume", "soft")] title
    let link_el = audio (sfx </> "link.wav")
    let url_el = style "punctuation" "all" $ style "capital_letters" "pitch" $ prosody [("rate", "fast")] url
    let root = el "speak" [] $ map NodeElement [bulletpoint_el, label_el, title_el, link_el, url_el]

    let ssml = renderText def $ Document (Prologue [] Nothing []) root []
    newCString $ LTxt.unpack ssml
  where
    ctext cstr = pack <$> peekCString cstr
    el name attrs childs = Element name (M.fromList attrs) childs
    audio src = el "audio" [("src", pack src)] []
    prosody attrs txt = el "prosody" attrs [NodeContent txt]
    style field mode inner = el "tts:style" [("field", field), ("mode", mode)] [NodeElement inner]

foreign export ccall c_dading :: IO CString

c_dading = do
    sfx <- getXdgDirectory XdgCache "rhapsode"
    let link_el = audio (sfx </> "link.wav")
    let root = el "speak" [] [NodeElement link_el]
    let ssml = renderText def $ Document (Prologue [] Nothing []) root []
    newCString $ LTxt.unpack ssml
  where
    el name attrs childs = Element name (M.fromList attrs) childs
    audio src = el "audio" [("src", pack src)] []

--- For Voice2JSON

foreign export ccall c_dataDir :: CString -> IO CString

c_dataDir c_subdir = do
    subdir <- peekCString c_subdir
    cache <- getXdgDirectory XdgData "rhapsode"
    newCString (cache </> subdir)

foreign export ccall c_recognizeIntent :: CString -> IO CString

c_recognizeIntent c_profile = do
    profile <- peekCString c_profile
    (_, Just pipe, _, _) <- createProcess (proc "voice2json" [
        "--profile", profile,
        "transcribe-stream",
        "-c", "1"]){std_out = CreatePipe}
    (_, Just out, _, _) <- createProcess (proc "voice2json" [
        "--profile", profile,
        "recognize-intent"]){std_in = UseHandle pipe, std_out = CreatePipe}
    intent <- LBS.hGetContents out
    let transcript = case decode intent of
            Just (Object obj) | Just (String txt) <- "text" `HM.lookup` obj -> unpack txt
            _ -> ""
    newCString transcript
