{-# LANGUAGE OverloadedStrings #-}
-- | Bindings from `xml-conduit` to `haskell-stylist`.
module Data.HTML2CSS(
        html2css, -- parsing
        el2styletree, els2stylist, el2stylist -- application
    ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as Txt
import Data.Maybe

import qualified Text.XML as XML
import Stylist.Parse
import Stylist
import Stylist.Tree
import Data.CSS.Syntax.Tokens
import Network.URI

---- Parsing
-- | Converts a parsed XML or HTML file to a `ConditionalStyles` `StyleSheet`.
html2css :: StyleSheet s => XML.Document -> URI -> s -> s
html2css xml url self = html2css' (XML.documentRoot xml) url self

html2css' :: StyleSheet s => XML.Element -> URI -> s -> s
html2css' (XML.Element (XML.Name "style" _ _) attrs children) url self
    | M.lookup "type" attrs `notElem` [Nothing, Just "text/css"] = self -- Unsupported stylesheet.
    | Just media <- "media" `M.lookup` attrs =
        fst $ addAtRule self "media" (tokenize media ++
            LeftCurlyBracket : tokContent url children ++ [RightCurlyBracket])
   | otherwise = parseForURL self url $ strContent children
html2css' (XML.Element (XML.Name "link" _ _) attrs _) baseURL self
    | M.lookup "type" attrs `elem` [Nothing, Just "text/css"],
        Just "stylesheet" <- "rel" `M.lookup` attrs,
        Just link <- "href" `M.lookup` attrs,
        Just url <- parseURIReference $ Txt.unpack link =
            fst $ addAtRule self "import" (
                Url (Txt.pack $ uriToString' $ relativeTo url baseURL) :
                fromMaybe [] (tokenize <$> M.lookup "media" attrs) ++
                [Semicolon])
html2css' (XML.Element _ _ children) url self =
    L.foldl' (\s el -> html2css' el url s) self [el | XML.NodeElement el <- children]


strContent :: [XML.Node] -> Txt.Text
strContent (XML.NodeContent text : rest) = text `Txt.append` strContent rest
-- We do want to read in comments for CSS, just not for display.
strContent (XML.NodeComment text : rest) = text `Txt.append` strContent rest
strContent (XML.NodeElement (XML.Element _ _ children):rest) =
    strContent children `Txt.append` strContent rest
strContent (_:rest) = strContent rest
strContent [] = ""

tokContent :: URI -> [XML.Node] -> [Token]
tokContent baseURL = map absolutizeUrl . tokenize . strContent
    where
        absolutizeUrl (Url link) | Just url <- parseURIReference $ Txt.unpack link =
                Url $ Txt.pack $ uriToString' $ relativeTo url baseURL

uriToString' uri = uriToString id uri ""

---- Styling

el2styletree el = StyleTree (Left el) $ mapMaybe node2styletree $ XML.elementNodes el
node2styletree (XML.NodeElement el) = Just $ el2styletree el
node2styletree (XML.NodeContent txt) = Just $ StyleTree (Right [
    ("content", [String txt]), ("display", [Ident "inline"])]) []
node2styletree _ = Nothing

previous' (Just ElementNode {name = "", previous = prev'}) = previous' prev'
previous' prev' = prev'

els2stylist = preorder els2stylist'
els2stylist' parent previous (Left (XML.Element (XML.Name name ns _) attrs _)) =
    ElementNode {
        name = name, namespace = fromMaybe "" ns,
        attributes = L.sort [
            Attribute n (fromMaybe "" ns) $ Txt.unpack v | (XML.Name n ns _, v) <- M.toList attrs
        ],
        parent = parent, previous = previous' previous
    }
els2stylist' parent previous (Right attrs) = ElementNode {
        name = "", namespace = "",
        attributes = [Attribute "style" "" $ Txt.unpack $ Txt.concat style],
        parent = parent, previous = previous' previous
    } where style = concat [[prop, ": ", serialize v, "; "] | (prop, v) <- attrs]

el2stylist = els2stylist . el2styletree
