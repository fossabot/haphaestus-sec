{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-}
module Network.URI.Fetch.XML.Table(applySort, applySortDoc, splitTable) where

import Text.XML
import Data.Text as Txt
import qualified Data.Map as M

import Data.Maybe
import qualified Data.List as L
import Text.Read (readMaybe)

-- For smarter comparisons...
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.Clock (UTCTime)
import Data.Char (isDigit)

applySortDoc :: String -> Document -> Document
applySortDoc anchor doc@Document {documentRoot = el} = doc {documentRoot = applySort anchor el}

applySort :: String -> Element -> Element
applySort ('#':'-':'a':'r':'g':'o':'-':'%':anchor) el
    | (id', ord:col) <- L.break (`L.elem` ['<', '>']) anchor, Just col' <- readMaybe col =
        applySort' id' (ord == '<') col' el
applySort _ el = el

applySort' :: String -> Bool -> Int -> Element -> Element
applySort' ('.':id') asc col el@Element { elementNodes = childs }
    | (ix, subpath) <- L.break (== '.') id', Just ix' <- readMaybe ix =
        el { elementNodes = setAt ix' (rewriteNode subpath) childs }
    | otherwise = el
  where
    rewriteNode p (NodeElement child) = NodeElement $ applySort' p asc col child
    rewriteNode _ x = x
applySort' "" asc col el = applySort'' asc col el

applySort' id' asc col el@Element { elementAttributes = attrs, elementNodes = childs }
    | Just actual <- "id" `M.lookup` M.mapKeys nameLocalName attrs, pack id' == actual =
        applySort'' asc col el
    | otherwise = el { elementNodes = L.map searchNode childs }
  where
    searchNode (NodeElement child) = NodeElement $ applySort' id' asc col child
    searchNode x = x

applySort'' asc col el
    | Just sortable <- table2sorttable el = el {
        elementNodes = annotateTHead header asc col ++
            (L.concatMap (L.map NodeElement . markup) $ L.sortBy compareRows sortable)
            ++ footer
      }
    | otherwise = el
  where
    compareRows (TableRow a _) (TableRow b _)
        | asc = compareAs (a !! col) (b !! col) (comparators !! col)
        | otherwise = compareAs (b !! col) (a !! col) (comparators !! col)
    (header, _, footer) = splitTable $ elementNodes el
    comparators = tableHeadComparators header

data TableRow = TableRow { keys :: [Text], markup :: [Element] }

table2sorttable Element {
        elementName = Name "table" _ _,
        elementAttributes = attrs,
        elementNodes = childs
    } | "-argo-unsortable" `notElem` attrs, (_, body, _) <- splitTable childs =
        trs2sorttable body
table2sorttable _ = Nothing

splitTable :: [Node] -> ([Node], [Element], [Node])
splitTable (NodeElement el@Element { elementName = Name "caption" _ _}:els) =
    let (header, body, footer) = splitTable els in (NodeElement el:header, body, footer)
splitTable (NodeElement el@Element { elementName = Name "colgroup" _ _}:els) =
    let (header, body, footer) = splitTable els in (NodeElement el:header, body, footer)
splitTable (NodeElement el@Element { elementName = Name "thead" _ _}:els) =
    let (body, footer) = splitTableBody els in ([NodeElement el], body, footer)
splitTable (NodeElement el@Element { elementName = Name "tr" _ _, elementNodes = childs}:els)
    | L.all (== "th") [nameLocalName $ elementName el | NodeElement el <- childs] =
        let (body, footer) = splitTableBody els in ([NodeElement el], body, footer)
splitTable els@(NodeElement _:_) =
    let (body, footer) = splitTableBody els in ([], body, footer)
splitTable (_:els) = splitTable els
splitTable [] = ([], [], [])

splitTableBody :: [Node] -> ([Element], [Node])
splitTableBody (NodeElement el@Element { elementName = Name "tbody" _ _, elementNodes = childs }:els) =
    ([el | NodeElement el@Element { elementName = Name "tr" _ _ } <- childs], els)
splitTableBody (NodeElement el@Element { elementName = Name "tr" _ _ }:els) =
    let (body, footer) = splitTableBody els in (el:body, footer)
splitTableBody els@(NodeElement _:_) = ([], els)
splitTableBody (_:els) = splitTableBody els
splitTableBody [] = ([], [])

tableHeadComparators :: [Node] -> [Text]
tableHeadComparators = Prelude.map (fromMaybe "alphanumeric") . tableHeadComparators'
tableHeadComparators' :: [Node] -> [Maybe Text]
tableHeadComparators' (NodeElement el@Element { elementName = Name name _ _, elementNodes = childs}:els)
    | name == "thead" = tableHeadComparators' childs `mergeRight` tableHeadComparators' els
    | name `L.elem` ["colgroup", "tr"] = tableRowComparators childs `mergeRight` tableHeadComparators' els
    | otherwise = tableHeadComparators' els
tableHeadComparators' [] = []
tableRowComparators :: [Node] -> [Maybe Text]
tableRowComparators (NodeElement el@(Element (Name "col" _ _) attrs _):els) =
    let colspan = fromMaybe 1 (M.lookup "span" attrs >>= readMaybe . unpack)
    in Prelude.replicate colspan (M.lookup "-argo-sortas" attrs) ++ tableRowComparators els
tableRowComparators (NodeElement el@(Element (Name n _ _) attrs _):els) | n `L.elem` ["td", "th"] =
    let colspan = fromMaybe 1 (M.lookup "colspan" attrs >>= readMaybe . unpack)
    in Prelude.replicate colspan (M.lookup "-argo-sortas" attrs) ++ tableRowComparators els
tableRowComparators (_:els) = tableRowComparators els
tableRowComparators [] = []
mergeRight :: [Maybe a] -> [Maybe a] -> [Maybe a]
mergeRight (_:as) (Just b:bs) = Just b : mergeRight as bs
mergeRight (a:as) (_:bs) = a : mergeRight as bs
mergeRight [] bs = bs
mergeRight as [] = as

annotateTHead (NodeElement el@Element { elementName = Name "thead" _ _, elementNodes = childs }:nodes) a c =
    NodeElement el { elementNodes = annotateTHead childs a c } : nodes
annotateTHead (NodeElement el@Element { elementName = Name "tr" _ _, elementNodes = childs }:nodes) a c =
    NodeElement el { elementNodes = annotateTR childs a c 0 } : nodes
annotateTHead (child:childs) a c = child:annotateTHead childs a c
annotateTHead [] _ _ = []

annotateTR (NodeElement el@Element { elementName = Name n _ _, elementAttributes = attrs }:nodes) asc col count
    | n `L.elem` ["th", "td"], count >= col =
        NodeElement el { elementAttributes = M.insert "aria-sort" asc' attrs }:nodes
    | n `L.elem` ["th", "td"] = NodeElement el:annotateTR nodes asc col (count + colspan)
  where
    colspan = fromMaybe 1 (readMaybe =<< unpack <$> M.lookup "colspan" attrs')
    attrs' = M.mapKeys nameLocalName attrs
    asc' | asc = "ascending"
        | otherwise = "descending"
annotateTR (node:nodes) a c n = node:annotateTR nodes a c n
annotateTR [] _ _ _ = []

trs2sorttable els@(el@Element { elementName = Name "tr" _ _, elementNodes = childs }:_)
    | Just keys' <- tds2keys [el | NodeElement el <- childs],
      Just (group, rest) <- groupTrs els 1,
      Just rest' <- trs2sorttable rest = Just (TableRow keys' group : rest')
trs2sorttable [] = Just []
trs2sorttable _ = Nothing

tds2keys :: [Element] -> Maybe [Text]
tds2keys (el@Element {elementName = Name n _ _, elementAttributes = attrs, elementNodes = childs }:els)
    | n `L.elem` ["td", "th"], Just key <- "-argo-sortkey" `M.lookup` attrs, Just rest <- tds2keys els =
        Just (Prelude.replicate colspan key ++ rest)
    | n `L.elem` ["td", "th"], Just rest <- tds2keys els =
        Just (Prelude.replicate colspan (nodesText childs) ++ rest)
  where
    colspan | Just n <- "colspan" `M.lookup` M.mapKeys nameLocalName attrs,
            Just m <- readMaybe $ unpack n = m
        | otherwise = 1
tds2keys [] = Just []
tds2keys _ = Nothing

groupTrs (el@Element {elementName = Name "tr" _ _}:els) n
    | rowRowspan n el <= 1 = Just (el:[], els)
    | Just (tail, rest) <- groupTrs els $ pred n = Just (el:tail, rest)
groupTrs (_:els) n = groupTrs els n
groupTrs _ _ = Nothing

rowRowspan n Element {elementName = Name "tr" _ _, elementNodes = childs } =
    Prelude.maximum (n : [n |
            NodeElement (Element (Name name _ _) attrs _) <- childs,
            name `L.elem` ["td", "th"],
            rowspan <- maybeToList ("rowspan" `M.lookup` M.mapKeys nameLocalName attrs),
            n <- maybeToList $ readMaybe $ unpack rowspan])


--- Utils

(+++) = append
nodesText :: [Node] -> Text
nodesText (NodeElement (Element _ attrs children):nodes) = nodesText children +++ nodesText nodes
nodesText (NodeContent text:nodes) = text +++ nodesText nodes
nodesText (_:nodes) = nodesText nodes
nodesText [] = ""

setAt :: Int -> (a -> a) -> [a] -> [a]
setAt i a ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (x:xs) = a x : xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []

pattern (:.) :: Char -> Txt.Text -> Txt.Text
pattern x :. xs <- (Txt.uncons -> Just (x, xs))

infixr 5 :.

compareAs :: Text -> Text -> Text -> Ordering
--- Hueristic that readily handles both numbers & text
compareAs (a:.as) (b:.bs) "alphanumeric"
    | isDigit a && isDigit b =
        let (a', as') = Txt.break (not . isDigit) as
            (b', bs') = Txt.break (not . isDigit) bs
        in if Txt.length a' == Txt.length b' && a == b
        then compareAs as bs "alphanumeric"
        else if Txt.length a' == Txt.length b' then a `compare` b
        else Txt.length a' `compare` Txt.length b'
    | a == b = compareAs as bs "alphanumeric"
    | otherwise = a `compare` b
compareAs as bs "text" = as `compare` bs
compareAs as bs "number" = readInt as `compare` readInt bs
    where
        readInt :: Text -> Maybe Float
        readInt = readMaybe . Prelude.filter (`L.elem` '-':'.':['0'..'9']) . unpack
compareAs as bs fmt = readTime as `compare` readTime bs
    where
        readTime :: Text -> Maybe UTCTime
        readTime = parseTimeM True defaultTimeLocale (unpack fmt) . unpack
