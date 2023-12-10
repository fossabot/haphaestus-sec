{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Network.URI.Fetch.XML(Page(..), loadVisited,
    fetchDocument, pageForText, applyCSScharset, readStrict) where

import           Data.Text.Lazy (fromStrict)
import qualified Data.Text as Txt
import           Data.Text (Text)
import qualified Data.Text.IO as Txt
import           Data.Text.Encoding
import qualified Data.Text.Lazy as LTxt
import qualified Data.ByteString.Lazy as B
import qualified Text.HTML.DOM as HTML
import qualified Text.XML as XML
import           Text.XML (Document(..))
import           Network.URI
import           Network.URI.Fetch
import           Network.URI.Charset
import qualified Data.Map as M
import qualified Data.Set as Set
import           Data.Set (Set(..))
import           Data.List (intercalate)
import           Data.Time.Clock

-- For alternative styles
import qualified Data.CSS.Syntax.Tokens as CSSTok
import Stylist.Parse

import System.IO
import System.IO.Temp
import Data.Default.Class
import System.Directory
import System.FilePath ((</>))
import Data.FileEmbed
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Network.URI.Fetch.XML.Table -- Apply table sorting here...
import Data.HTML2CSS (html2css)

data Page styles = Page {
    pageURL :: URI,
    css :: styles,
    initCSS :: URI -> String -> styles,
    domain :: String,
    html :: Document,
    pageTitle :: String,
    pageMIME :: String,
    apps :: [Application],
    backStack :: [(String, URI)],
    forwardStack :: [(String, URI)],
    -- Probably don't need an MVar here, but let's be safe!
    visitedURLs :: Set Text,
    appName :: String
}

loadVisited :: String -> IO (Set Text)
loadVisited appname = do
    dir <- getXdgDirectory XdgData appname
    let path = dir </> "history.gmni"
    exists <- doesFileExist path

    if exists then do
        file <- readStrict path
        let hist = Set.fromList [Txt.pack uri | _:uri:_ <- map words $ lines file]
        return hist
    else return Set.empty

readStrict path = Txt.unpack <$> Txt.readFile path

utf8' bytes = convertCharset "utf-8" $ B.toStrict bytes
aCCEPT = ["text/xml", "application/xml", "text/html", "text/gemini",
    "text/csv", "text/tab-separated-values", "text/css", "text/*", "*/*"]

fetchDocument http referer URI { uriScheme = "action:", uriPath = "nocache" } =
    fetchDocument http { cachingEnabled = False } referer $ pageURL referer
fetchDocument http referer URI { uriScheme = "action:", uriPath = "novalidate" } =
    fetchDocument http { validateCertificates = False } referer $ pageURL referer
fetchDocument http referer URI { uriScheme = "action:", uriPath = "history/back" } =
        fetchURL' http aCCEPT (pageURL referer') >>= parseDocument' referer' http False
    where referer' = shiftHistory referer (-1)
fetchDocument http referer URI { uriScheme = "action:", uriPath = "history/forward" } =
        fetchURL' http aCCEPT (pageURL referer') >>= parseDocument' referer' http False
    where referer' = shiftHistory referer 1
fetchDocument http referer URI {
        uriScheme = "action:", uriPath = 'h':'i':'s':'t':'o':'r':'y':'/':x
    } | Just x' <- readMaybe x, referer' <- shiftHistory referer x' =
        fetchURL' http aCCEPT (pageURL referer') >>= parseDocument' referer http False
fetchDocument http referer URI { uriScheme = "app:", uriPath = appID } = do
    dispatchByApp http Application {
        name = "", icon = nullURI, description = "",
        appId = appID
      } (pageMIME referer) $ pageURL referer
    return referer -- TODO play an error or success sound
fetchDocument http referer@Page { pageURL = uri0 } uri@URI { uriFragment = anchor }
    | uri { uriFragment = "" } == uri0 { uriFragment = "" } = return referer {
        html = applySortDoc anchor $ html referer,
        pageURL = uri
    }
fetchDocument http referer uri = fetchURL' http aCCEPT uri >>= parseDocument' referer http True

shiftHistory :: Page style -> Integer -> Page style
shiftHistory self 0 = self
shiftHistory self@Page { backStack = (title, url):bs } delta | delta < 0 =
    shiftHistory self {
        backStack = bs,
        forwardStack = (pageTitle self, pageURL self):forwardStack self,
        pageTitle = title,
        pageURL = url
    } $ succ delta
shiftHistory self@Page { forwardStack = (title, url):fs } delta | delta > 0 =
    shiftHistory self {
        forwardStack = fs,
        backStack = (pageTitle self, pageURL self):backStack self,
        pageTitle = title,
        pageURL = url
    } $ pred delta
shiftHistory self _ = self -- Error case.

parseDocument' ref@Page {visitedURLs = hist} sess saveHist resp@(URI {uriFragment = anch}, mime, _) = do
    page <- parseDocument ref {domain = "document"} sess resp >>= logHistory hist
    apps' <- appsForMIME sess mime
    return $ attachHistory page {
        pageMIME = mime,
        apps = apps',
        html = applySortDoc anch $ html page
    }
  where
    attachHistory x@Page { pageTitle = title, pageURL = url }
        | saveHist = x { backStack = (title, url):backStack ref, forwardStack = forwardStack ref }
        | otherwise = x
parseDocument :: StyleSheet s => Page s -> Session -> (URI, String, Either Text B.ByteString)
        -> IO (Page s)
parseDocument ref sess (uri, "html/x-error\t", resp) =
    parseDocument ref { domain = "error" } sess (uri, "text/html", resp)
parseDocument p _ (uri, "text/html", Left text) =
    pageForDoc p uri $ HTML.parseLT $ fromStrict text
parseDocument p _(uri, "text/html", Right bytes) =
    pageForDoc p uri $ HTML.parseLBS bytes
parseDocument p _
        (uri, 't':'e':'x':'t':'/':'g':'e':'m':'i':'n':'i':';':'l':'a':'n':'g':'=':lang, Left text) =
    pageForDoc p uri $ parseGemini (Just lang) text
parseDocument p _
        (uri, 't':'e':'x':'t':'/':'g':'e':'m':'i':'n':'i':';':'l':'a':'n':'g':'=':lang, Right bytes) =
    pageForDoc p uri $ parseGemini (Just lang) $ utf8' bytes
parseDocument p _ (uri, "text/gemini", Left text) =
    pageForDoc p uri $ parseGemini Nothing text
parseDocument p _ (uri, "text/gemini", Right bytes) =
    pageForDoc p uri $ parseGemini Nothing $ utf8' bytes
parseDocument a b (a', b'@"text/css", Right bytes) =
    parseDocument a b (a', b', Left $ applyCSScharset (map Txt.unpack charsets) $ B.toStrict bytes)
parseDocument referer@Page {pageURL = uri', initCSS = css', appName = name} _
    (uri, "text/css", Left text)
  | URI {uriAuthority = Just host} <- pageURL referer = do
    -- Save this per-domain setting
    dir <- (</> "domain") <$> getXdgDirectory XdgConfig name
    createDirectoryIfMissing True dir
    Txt.writeFile (dir </> uriRegName host) $
        CSSTok.serialize $ map absolutizeCSS $ CSSTok.tokenize text

    return ret
  | otherwise = return ret
 where
  ret = referer {
        css = parseForURL (css' uri' "document") uri text
    }
  absolutizeCSS (CSSTok.Url text) | Just rel <- parseRelativeReference $ Txt.unpack text =
    CSSTok.Url $ Txt.pack $ uriToStr' $ relativeTo rel uri'
  absolutizeCSS tok = tok
parseDocument ref _ (uri, "text/csv", Left body) =
    pageForDoc ref uri $ parseDelimitedToTable ',' body
parseDocument ref _ (uri, "text/tab-separated-values", Left body) =
    pageForDoc ref uri $ parseDelimitedToTable '\t' body
parseDocument ref _ (uri, "text/csv", Right body) =
    pageForDoc ref uri $ parseDelimitedToTable ',' $ utf8' body
parseDocument ref _ (uri, "text/tab-separated-values", Right body) =
    pageForDoc ref uri $ parseDelimitedToTable '\t' $ utf8' body

parseDocument ref sess (uri, mime, body) | mime' /= mime = parseDocument ref sess (uri, mime', body)
    where mime' = takeWhile (/= ';') mime
parseDocument p _ (uri, _, Left text)
    | Right doc <- XML.parseText def $ fromStrict text = pageForDoc p uri doc
    | otherwise = pageForText p uri text
parseDocument p _ (uri, _, Right bytes)
    | Right doc <- XML.parseLBS def bytes = pageForDoc p uri doc
parseDocument p _ (uri, 't':'e':'x':'t':'/':_, Right bytes) =
    -- charset wasn't specified, so assume utf-8.
    pageForText p uri $ utf8' bytes
parseDocument p sess resp@(uri, mime, _) = do
    dir <- getCurrentDirectory -- TODO find Downloads directory.
    ret <- saveDownload nullURI {
        uriScheme = "file:",
        uriAuthority = Just (URIAuth "" "" "")
    } dir resp >>= dispatchByMIME sess mime
    pageForDoc p uri $ HTML.parseLT $ LTxt.pack $ fromMaybe "Unsupported filetype" ret

pageForText referer uri txt = pageForDoc referer uri XML.Document {
        XML.documentPrologue = XML.Prologue [] Nothing [],
        XML.documentRoot = XML.Element {
            XML.elementName = "pre",
            XML.elementAttributes = M.empty,
            XML.elementNodes = [XML.NodeContent txt]
        },
        XML.documentEpilogue = []
    }

pageForDoc :: StyleSheet s => Page s -> URI -> Document -> IO (Page s)
pageForDoc referer@Page {initCSS = css', appName = appname, domain = d} uri doc = do
    -- See if the user has configured an alternate stylesheet for this domain.
    let authorStyle = return $ html2css doc uri $ css' uri d
    styles <- case uriAuthority uri of
        Nothing -> authorStyle
        Just host -> do
            dir <- getXdgDirectory XdgConfig appname
            let path = dir </> "domain" </> uriRegName host
            hasAltStyle <- doesFileExist path
            if not hasAltStyle then authorStyle else parse (css' uri d) <$> Txt.readFile path

    return referer {pageURL = uri, html = doc, css = styles}

logHistory hist ret@Page {pageURL = url', html = doc, appName = name} = do
    dir <- getXdgDirectory XdgData name
    createDirectoryIfMissing True dir
    now <- getCurrentTime
    let title = Txt.unpack $ getTitle $ XML.documentRoot doc
    appendFile (dir </> "history.gmni") $ '\n' : intercalate " " [
        "=>", uriToStr' url', show now, title
      ]

    return ret { pageTitle = title, visitedURLs = Set.insert (Txt.pack $ uriToStr' url') hist}
  where
    getTitle (XML.Element "title" _ childs) = Txt.concat [txt | XML.NodeContent txt <- childs]
    getTitle (XML.Element "h1" _ childs) = Txt.concat [txt | XML.NodeContent txt <- childs]
    getTitle (XML.Element _ _ childs)
        | title:_ <- [getTitle el | XML.NodeElement el <- childs] = title
        | otherwise = ""

uriToStr' :: URI -> String
uriToStr' uri = uriToString id uri ""

--------
---- CSS charset sniffing
--------
applyCSScharset (charset:charsets) bytes
        | cssCharset (CSSTok.tokenize text) == Txt.pack charset = text
        | otherwise = applyCSScharset charsets bytes
    where
        text = convertCharset charset bytes
applyCSScharset _ bytes = convertCharset "utf-8" bytes
cssCharset toks | (CSSTok.AtKeyword "charset":toks') <- skipCSSspace toks,
        (CSSTok.String charset:_) <- skipCSSspace toks' = charset
    | otherwise = ""
skipCSSspace (CSSTok.Whitespace:toks) = skipCSSspace toks
skipCSSspace toks = toks

--------
---- Gemini implementation
--------
-- Copied from css-syntax.
pattern (:.) :: Char -> Txt.Text -> Txt.Text
pattern x :. xs <- (Txt.uncons -> Just (x, xs))

infixr 5 :.

el name text = XML.Element name M.empty [XML.NodeContent text]

parseGemini :: Maybe String -> Txt.Text -> XML.Document
parseGemini lang txt = XML.Document {
        XML.documentPrologue = XML.Prologue [] Nothing [],
        XML.documentRoot = XML.Element {
            XML.elementName = "body",
            XML.elementAttributes = M.fromList [
                ("lang", Txt.pack lang') | Just langs <- [lang], lang' <- [csv langs]],
            XML.elementNodes = map XML.NodeElement $ parseGemini' $ Txt.lines txt
        },
        XML.documentEpilogue = []
    }

csv (',':_) = ""
csv (c:rest) = c:csv rest
csv "" = ""

parseGemini' :: [Txt.Text] -> [XML.Element]
parseGemini' (('#':.'#':.'#' :. '#':.'#':.'#':.line):lines) =
    el "h6" line : parseGemini' lines
parseGemini' (('#':.'#':.'#' :. '#':.'#':.line):lines) =
    el "h5" line : parseGemini' lines
parseGemini' (('#':.'#':.'#' :. '#':.line):lines) =
    el "h4" line : parseGemini' lines
parseGemini' (('#':.'#':.'#':.line):lines) = el "h3" line : parseGemini' lines
parseGemini' (('#':.'#':.line):lines) = el "h2" line : parseGemini' lines
parseGemini' (('#':.line):lines) = el "h1" line : parseGemini' lines
-- Not properly structured, but still sounds fine...
parseGemini' (('*':.line):lines) = el "li" line : parseGemini' lines
parseGemini' (('>':.line):lines) = el "blockquote" line : parseGemini' lines

parseGemini' (('=':.'>':.line):lines)
    | (url:text@(_:_)) <- Txt.words line = (el "a" $ Txt.unwords text) {
            XML.elementAttributes = M.insert "href" url M.empty
        } : parseGemini' lines
    | otherwise = (el "a" $ Txt.strip line) {
            XML.elementAttributes = M.insert "href" (Txt.strip line) M.empty
        } : parseGemini' lines
parseGemini' (('`':.'`':.'`':.line):lines) = el "p" line : go lines
    where
        go (('`':.'`':.'`':._):lines) = parseGemini' lines
        go (_:lines) = go lines
        go [] = []
parseGemini' ("```":lines) = go [] lines
    where
        go texts (('`':.'`':.'`':._):lines) =
            el "pre" (Txt.unlines texts) : parseGemini' lines
        go texts (line:lines) = go (texts ++ [line]) lines
        go texts [] = []

parseGemini' (line:lines) = el "p" line : parseGemini' lines
parseGemini' [] = []

--------
---- TSV, CSV, etc
--------

parseDelimitedValues _ "" row rows = reverse (reverse row : rows)
parseDelimitedValues delim ('\r':.cs) row rows = parseDelimitedValues delim cs row rows
parseDelimitedValues delim ('\n':.cs) row rows = parseDelimitedValues delim cs [] (reverse row : rows)
parseDelimitedValues delim (c:.'"':.cs) row rows | c == delim =
        let (value, cs') = inner cs in parseDelimitedValues delim cs' (value:row) rows
    where
        inner (x:.y:.cs) | x == delim && y == delim = let (a, b) = inner cs in (delim `Txt.cons` a, b)
        inner (c:.cs) | c == delim = ("", cs)
            | otherwise = let (a, b) = inner cs in (c `Txt.cons` a, b)
        inner "" = ("", "")
parseDelimitedValues delim (c:.cs) row rows | c == delim =
    let (value, cs') = Txt.break (`elem` ['\r', '\n', delim]) cs
    in parseDelimitedValues delim cs' (value:row) rows
parseDelimitedValues delim cs row rows =
    let (value, cs') = Txt.break (`elem` ['\r', '\n', delim]) cs
    in parseDelimitedValues delim cs (value:row) rows

escapeDelimitedValues delim source = map (map inner) $ parseDelimitedValues delim source [] []
    where
        inner = Txt.strip . Txt.replace "\\\\" "\\" . Txt.replace "\\n" "\n" .
            Txt.replace "\\t" "\t" . Txt.replace "\\r" "\r"

parseDelimitedToTable delim source
    | (head:body) <- filter (not . null) $ escapeDelimitedValues delim source =
        XML.Document {
            XML.documentPrologue = XML.Prologue [] Nothing [],
            XML.documentRoot = XML.Element {
                XML.elementName = "table",
                XML.elementAttributes = M.empty,
                XML.elementNodes = rowToTr "th" head : map (rowToTr "td") body
            },
            XML.documentEpilogue = []
        }
    | otherwise = XML.Document { -- Empty TSV/CSV/etc
        XML.documentPrologue = XML.Prologue [] Nothing [],
        XML.documentRoot = XML.Element "table" M.empty [],
        XML.documentEpilogue = []
    }
rowToTr tagname values = XML.NodeElement $ XML.Element "tr" M.empty $ map inner values
    where
        inner = XML.NodeElement . XML.Element tagname M.empty . singleton . XML.NodeContent
        singleton a = [a]
