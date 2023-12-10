{-# LANGUAGE OverloadedStrings #-}
module Network.URI.XDG.MimeInfo(readMimeInfo) where

import Network.URI.Fetch (Application(..))
import Network.URI

import Text.XML as XML
import Data.Text (Text, append, unpack, pack)
import qualified Data.Map as M

import System.Environment (lookupEnv)
import System.FilePath ((</>), (<.>))
import System.Directory (doesFileExist)
import System.IO (hPrint, stderr)
import Control.Monad (forM)
import Control.Exception (catch)
import Data.Maybe (catMaybes, maybeToList, fromMaybe, mapMaybe)

import System.Directory (getHomeDirectory)

readMimeInfo :: [String] -> String -> IO Application
readMimeInfo locales mime = do
    dirs <- lookupEnv "XDG_DATA_DIRS"
    homedir <- lookupEnv "XDG_DATA_HOME"
    cwd <- getHomeDirectory
    let dirs' = fromMaybe' (cwd </> ".local/share/") homedir :
            split ':' (fromMaybe' "/usr/local/share/:/usr/share/" dirs)

    files <- forM dirs' $ \dir -> do
        let file = dir </> "mime" </> mime <.> "xml"
        exists <- doesFileExist file
        if exists then (Just <$> XML.readFile def file) `catch` handleBadXML else return Nothing

    return $ case catMaybes files of
        file:_ -> readMimeInfo' locales mime $ documentRoot file
        [] -> Application {
            name = mime,
            icon = URI "xdg-icon:" Nothing (replace '/' '-' mime </> genericIcon mime) "" "",
            description = "",
            appId = mime
          }

readMimeInfo' locales mime el = Application {
        name = readEl "comment" Nothing mime,
        icon = nullURI {
            uriScheme = "xdg-icon:",
            uriPath = readEl "icon" (Just "name") (replace '/' '-' mime) </>
                readEl "generic-icon" (Just "name") (genericIcon mime)
        },
        description = readEl "expanded-acronym" Nothing $ readEl "acronym" Nothing mime,
        appId = mime
    }
  where
    readEl key attr fallback
        | (val:_) <- [v | l <- locales ++ [""], v <- maybeToList $ lookup l els] = unpack val
        | otherwise = fallback
      where els = readEl' (pack key) attr $ elementNodes el
    readEl' key Nothing (NodeElement (Element name attrs childs):sibs)
        | key == nameLocalName name = (lang attrs, nodesText childs) : readEl' key Nothing sibs
    readEl' key attr'@(Just attr) (NodeElement (Element name attrs _):sibs)
        | key == nameLocalName name, Just val <- Name key namespace Nothing `M.lookup` attrs =
            (lang attrs, val) : readEl' key attr' sibs
    readEl' key attr (_:sibs) = readEl' key attr sibs
    readEl' _ _ [] = []

    namespace = Just "http://www.freedesktop.org/standards/shared-mime-info"
    lang = unpack . fromMaybe "" . M.lookup "{http://www.w3.org/XML/1998/namespace}lang"

(+++) = append
nodesText :: [Node] -> Text
nodesText (NodeElement (Element _ attrs children):nodes) = nodesText children +++ nodesText nodes
nodesText (NodeContent text:nodes) = text +++ nodesText nodes
nodesText (_:nodes) = nodesText nodes
nodesText [] = ""

genericIcon mime = let (group, _) = break (== '/') mime in  group ++ "-x-generic"

handleBadXML err@(InvalidXMLFile _ _) = hPrint stderr err >> return Nothing

fromMaybe' a (Just "") = a
fromMaybe' _ (Just a) = a
fromMaybe' a Nothing = a

split b (a:as) | a == b = [] : split b as
        | (head':tail') <- split b as = (a:head') : tail'
        | otherwise = [a:as]
split _ [] = [[]]

replace old new (c:cs) | c == old = new:replace old new cs
    | otherwise = c:replace old new cs
replace _ _ [] = []
