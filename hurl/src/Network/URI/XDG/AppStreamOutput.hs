{-# LANGUAGE OverloadedStrings #-}
module Network.URI.XDG.AppStreamOutput(serializeXML, outputApps, testLocalIcons) where

import qualified Text.XML as XML
import qualified Data.Map as M
import Data.Text (Text, append, pack)
import qualified Data.Text as Txt
import Data.Text.Lazy (unpack)
import Network.URI.XDG.AppStream

import Data.List (stripPrefix)
import Control.Monad (forM)
import System.Directory (doesFileExist)
import Data.Maybe (catMaybes)

outputApps apps = serializeXML $ el "p" $ map outputApp apps
outputApp (App ident' name' summary' icons') =
    el' "a" [("href", "appstream://" `append` ident'), ("title", summary')] [
        el "picture" [
            el' (if i == 0 then "img" else "source") [
                ("src", url'),
                ("alt", name' `append` " logo " `append` int2txt width' `append` "x" `append` int2txt height'),
                ("sizes", int2txt width' `append` "w")] []
            | (i, Icon _ width' height' url') <- zip [0..] icons'
        ],
        XML.Element "caption" M.empty [XML.NodeContent name']]

testLocalIcons icons = do
    icons' <- forM icons $ \icon -> case "file://" `stripPrefix` Txt.unpack (url icon) of
        Just path -> do
            exists <- doesFileExist path
            return $ if exists then Just icon else Nothing
        Nothing -> return $ Just icon
    return $ catMaybes icons'

-- Generic XML/Text utilities
serializeXML el = unpack $ XML.renderText XML.def XML.Document {
        XML.documentPrologue = XML.Prologue [] Nothing [],
        XML.documentRoot = el,
        XML.documentEpilogue = []
    }

el' name attrs children = XML.Element {
        XML.elementName = XML.Name name Nothing Nothing,
        XML.elementAttributes = M.fromList attrs,
        XML.elementNodes = map XML.NodeElement children
    }
el name children = el' name [] children

int2txt (Just n) = pack $ show n
int2txt Nothing = "?"
