{-# LANGUAGE OverloadedStrings #-}
module Network.URI.XDG.AppStream(
    Component, loadDatabase, xmlForID, buildMIMEIndex,
    App(..), Icon(..), IconCache, scanIconCache, appsForMIME
) where

import qualified Data.Map as M
import qualified Text.XML as XML
import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy as LBS
import System.Directory
import System.FilePath ((</>), takeBaseName)
import Control.Exception (catch)
import Control.Monad (forM)
import Data.List (isSuffixOf, sortOn, elemIndex)
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import System.Process (callProcess)
import Data.Text (Text)
import qualified Data.Text as Txt
import Text.Read (readMaybe)
import Data.Char (isDigit)

----
-- Load in the XML files
----
type Component = M.Map Text [XML.Element]
cachedir = ".cache/nz.geek.adrian.hurl/appstream/"

loadDatabase :: [String] -> IO (M.Map Text Component)
loadDatabase locales = do
    -- Handle YAML files for Debian-derivatives
    sharePaths' <- yaml2xml "/usr/share/app-info/yaml/" "share" `catch` handleListError
    cachePaths' <- yaml2xml "/var/cache/app-info/yaml/" "cache" `catch` handleListError

    -- Read in the XML files.
    sharePaths <- listDirectory "/usr/share/app-info/xml/" `catch` handleListError
    cachePaths <- listDirectory "/var/cache/app-info/xml/" `catch` handleListError
    xmls <- forM (sharePaths ++ sharePaths' ++ cachePaths ++ cachePaths') $ \path -> do
        text <- LBS.readFile path
        let decompressor = if ".gz" `isSuffixOf` path then decompress else id
        return $ rightToMaybe $ XML.parseLBS XML.def $ decompressor text

    -- Index components by ID and their subelements by name
    let components = concat $ map getComponents $ catMaybes xmls
    let componentsByID = list2map [(getText "id" comp, comp) | comp <- components]
    let mergeComponents' = filterMergeAttrs . localizeComponent locales . mergeComponents
    let componentByID = M.filter M.null $ M.map mergeComponents' componentsByID
    return componentByID

yaml2xml :: FilePath -> String -> IO [FilePath]
yaml2xml source destSubDir = do
    home <- getHomeDirectory
    let destDir = home </> cachedir </> destSubDir ++ ".xml.gz"

    paths <- listDirectory source
    forM paths $ \path -> do
        let dest = destDir </> takeBaseName path
        destExists <- doesPathExist dest

        srcTime <- getModificationTime path
        destTime <- if destExists then getModificationTime path else return srcTime
        if srcTime >= destTime
            then callProcess "appstreamcli" ["convert", "--format=xml", path, dest]
            else return ()

    listDirectory destDir

getComponents :: XML.Document -> [Component]
getComponents XML.Document {
        XML.documentRoot = XML.Element {
            XML.elementNodes = nodes
        }
    } = mapMaybe getComponent nodes
getComponent :: XML.Node -> Maybe Component
getComponent (XML.NodeElement XML.Element {
        XML.elementName = XML.Name "component" _ _,
        XML.elementAttributes = attrs,
        XML.elementNodes = nodes
    }) = Just $ list2map (
        [(key, txt2el name val) | (name@(XML.Name key _ _), val) <- M.toList attrs] ++
        [(key, node) | XML.NodeElement node@(XML.Element (XML.Name key _ _) _ _) <- nodes]
    )
  where txt2el name txt = XML.Element name M.empty [XML.NodeContent txt]
getComponent _ = Nothing

mergeComponents :: [Component] -> Component
mergeComponents comps = mergeComponents' $ reverse $ sortOn (getInt "priority") comps
mergeComponents' [] = M.empty
mergeComponents' (comp:comps) = let base = mergeComponents' comps in
    case getText "merge" comp of
        "append" -> M.unionWith (++) comp base
        "replace" -> M.union comp base
        "remove-component" -> M.empty
        _ -> comp

localizeComponent :: [String] -> Component -> Component
localizeComponent locales comp = let locales' = map Txt.pack locales in
    let locale = bestXMLLocale locales' $ comp2xml comp in
    M.filter null $ M.map (mapMaybe $ filterElByLocale locale) comp

filterMergeAttrs :: Component -> Component
filterMergeAttrs comp = "priority" `M.delete` M.delete "merge" comp

----
-- Lookup by ID
----

xmlForID :: M.Map Text Component -> Text -> Maybe XML.Element
xmlForID comps id = comp2xml <$> M.lookup id comps

elementOrder :: [Text]
elementOrder = [
        "id", "pkgname", "source_pkgname", "name",
        "project_license", "summary", "description",
        "url", "project_group", "icon",
        "mimetypes", "categories", "keywords",
        "screenshots",
        "compulsory_for_desktop", "provides",
        "developer_name", "launchable", "releases",
        "languages", "bundle", "suggests",
        "content_rating", "agreement"
    ]

comp2xml :: Component -> XML.Element
comp2xml comp = XML.Element "component" M.empty $ map XML.NodeElement $ comp2els comp
comp2els :: Component -> [XML.Element]
comp2els comp = concat (
        map (\k -> M.findWithDefault [] k comp) elementOrder ++
        (map snd $ M.toList $ M.filterWithKey (\k v -> k `notElem` elementOrder) comp)
    )

----
-- Lookup by MIME
----

buildMIMEIndex :: M.Map Text Component -> M.Map Text [Component]
buildMIMEIndex comps = list2map [(mime, comp) | (_, comp) <- M.toList comps, mime <- getMIMEs comp]

getMIMEs :: Component -> [Text]
getMIMEs comp = let nodes = concat $ map (XML.elementNodes) $ getEls "mimetypes" comp
    in filter Txt.null $ map node2txt nodes

--

data App = App {
    ident :: Text,
    name :: Text,
    summary :: Text,
    icons :: [Icon]
}
data Icon = Icon {
    source :: Text,
    width :: Maybe Int,
    height :: Maybe Int,
    url :: Text
}

appsForMIME :: IconCache -> M.Map Text [Component] -> Text -> [App]
appsForMIME iconcache comps mime = mapMaybe (comp2app iconcache) $ M.findWithDefault [] mime comps

comp2app :: IconCache -> Component -> Maybe App
comp2app iconcache comp
    | getText "type" comp == "desktop-application" = Just $ App {
        ident = getText "id" comp,
        name = getText "name" comp,
        summary = getText "summary" comp,
        icons = sortOn rankIcon $ concat $ map (el2icon iconcache) $ getEls "icon" comp
    }
    | otherwise = Nothing
  where rankIcon icon = source icon `elemIndex` ["stock", "cached", "local", "remote"]

el2icon :: IconCache -> XML.Element -> [Icon]
el2icon iconcache el@(XML.Element _ attrs _)
    | Just "cached" <- "type" `M.lookup` attrs =
        [Icon "cached" size size $ Txt.append "file://" $ Txt.pack path
        | (size, path) <- lookupCachedIcons iconcache $ el2txt el]
el2icon _ el@(XML.Element _ attrs _) = [Icon {
        source = M.findWithDefault "" "type" attrs,
        width = parseIntAttr "width",
        height = parseIntAttr "height",
        url = iconURL el
    }]
  where parseIntAttr attr = M.lookup attr attrs >>= readMaybe . Txt.unpack

iconURL el@(XML.Element _ attrs _) = case "type" `M.lookup` attrs of
    Just "stock" -> "icon:" `Txt.append` val -- URI scheme NOT implemented
    Just "cached" -> "file:///{usr/share,var/cache}/app-info/icons/*/*/" `Txt.append` val
    Just "local" -> "file://" `Txt.append` val
    Just "remote" -> val
    _ -> "about:blank"
  where val = el2txt el

-- AppStream icon cache
type IconCache = [FilePath]
scanIconCache :: IO IconCache
scanIconCache = do
    sharePaths <- listDirectory "/usr/share/app-info/icons/" `catch` handleListError
    varPaths <- listDirectory "/var/cache/app-info/icons/" `catch` handleListError
    paths <- forM (sharePaths ++ varPaths) (\x -> listDirectory x `catch` handleListError)
    return (concat paths ++ sharePaths ++ varPaths)

lookupCachedIcons :: IconCache -> Text -> [(Maybe Int, FilePath)]
lookupCachedIcons iconcache icon = [(size $ takeBaseName dir, dir </> Txt.unpack icon) | dir <- iconcache]
    where size dirname = readMaybe $ takeWhile isDigit dirname

----
-- Supporting utilities
----
handleListError :: IOError -> IO [a]
handleListError _ = return []

-- It's not worth importing Data.Either.Combinators for this.
rightToMaybe :: Either l r -> Maybe r
rightToMaybe (Left _) = Nothing
rightToMaybe (Right x) = Just x

list2map :: Ord a => [(a, b)] -> M.Map a [b]
list2map = foldr insertEntry M.empty
    where insertEntry (key, value) = M.insertWith (++) key [value]

-- XML Utils

el2txt :: XML.Element -> Text
el2txt el = Txt.concat $ map node2txt $ XML.elementNodes el
node2txt :: XML.Node -> Text
node2txt (XML.NodeElement el) = el2txt el
node2txt (XML.NodeContent txt) = txt
node2txt _ = ""

getEls :: Text -> Component -> [XML.Element]
getEls key comp = M.findWithDefault [emptyEl] key comp
getEl :: Text -> Component -> XML.Element
getEl key comp | ret:_ <- getEls key comp = ret
    | otherwise = emptyEl
getText :: Text -> Component -> Text
getText key comp = el2txt $ getEl key comp
getInt :: Text -> Component -> Integer
getInt key comp = fromMaybe 0 $ readMaybe $ Txt.unpack $ getText key comp
emptyEl :: XML.Element
emptyEl = XML.Element "placeholder" M.empty []

bestXMLLocale :: [Text] -> XML.Element -> Text
bestXMLLocale locales (XML.Element _ attrs nodes)
    | Just locale <- "xml:lang" `M.lookup` attrs = locale
    | locale:_ <- sortOn rankLocale [bestXMLLocale locales el
            | XML.NodeElement el <- nodes] = locale
    | otherwise = ""
  where rankLocale locale = locale `elemIndex` locales

filterElByLocale :: Text -> XML.Element -> Maybe XML.Element
filterElByLocale locale el@(XML.Element _ attrs nodes)
    | Just locale' <- "xml:lang" `M.lookup` attrs, locale' /= locale = Nothing
    | otherwise = Just $ el {XML.elementNodes = filterNodesByLocale locale nodes}
filterNodesByLocale :: Text -> [XML.Node] -> [XML.Node]
filterNodesByLocale locale (XML.NodeElement el:nodes)
    | Just el' <- filterElByLocale locale el = XML.NodeElement el' : filterNodesByLocale locale nodes
    | otherwise = filterNodesByLocale locale nodes
filterNodesByLocale locale (node:nodes) = node : filterNodesByLocale locale nodes
filterNodesByLocale _ [] = []
