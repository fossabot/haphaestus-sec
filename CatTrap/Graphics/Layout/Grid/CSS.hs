{-# LANGUAGE OverloadedStrings #-}
-- | Infrastructure for parsing & desugaring grid-layout related CSS properties.
module Graphics.Layout.Grid.CSS(CSSGrid(..), Axis(..), CSSCell(..), Placement(..),
                                finalizeGrid, Areas, parseASCIIGrid) where

import Stylist (PropertyParser(..), parseOperands)
import Data.CSS.Syntax.Tokens (Token(..), NumericValue(..))

import Data.Text (Text)
import qualified Data.Text as Txt
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe, isNothing)
import Data.List (nub)
import qualified Data.HashMap.Lazy as HM

import Graphics.Layout.CSS.Length
import Graphics.Layout.Box
import Graphics.Layout.Grid
import Graphics.Layout

-- | Mapping from area identifiers to bounding boxes.
type Areas = HM.HashMap Text ((Int, Int), (Int, Maybe Int))

-- | Converts a grid to lookup table start & indices for row & columns.
-- Exported for the sake of testing
parseASCIIGrid :: [[Text]] -> Int -> Areas -> Maybe Areas
parseASCIIGrid (row:rows) i prev
    | names == nub names, and [span == fst rec && isNothing (snd $ snd rec)
            | (name, span) <- row', Just rec <- [name `HM.lookup` prev]] =
        parseASCIIGrid rows (succ i) $ HM.mapWithKey closeAreas $ HM.union prev $
            HM.fromList [(name, (span, (i, Nothing))) | (name, span) <- row']
    | otherwise = Nothing
  where
    names = map fst row'
    row' = parseAsciiRow $ enumerate row
    parseAsciiRow ((j, cell):cells) =
        let (self, cells') = span (\z -> snd z == cell) cells
        in (cell, (j, succ j + length self)):parseAsciiRow cells'
    parseAsciiRow [] = []
    enumerate = zip [0..]
    closeAreas name (a, (b, Nothing)) | name `notElem` names = (a, (b, Just i))
    closeAreas _ ret = ret
parseASCIIGrid [] _ ret = Just ret

-- | Parsed CSS Grid properties
data CSSGrid = CSSGrid {
    -- | Parsed CSS grid-auto-columns
    autoColumns :: Unitted,
    -- | Parsed grid-auto-flow
    autoFlow :: Axis,
    -- | Whether grid-auto-flow: dense was specified.
    autoFlowDense :: Bool,
    -- | Parsed CSS grid-auto-rows
    autoRows :: Unitted,
    -- | Parsed CSS grid-template-areas
    templateAreas :: Areas,
    -- | Parsed CSS grid-template-columns
    templateColumns :: [([Text], Unitted)],
    -- | Parsed CSS grid-template-rows
    templateRows :: [([Text], Unitted)],
    -- | Parsed CSS row-gap & column-gap
    cssGap :: Size Unitted Unitted,
    -- | Parsed CSS justify-items & align-items
    alignItems :: Size Alignment Alignment
}
-- | A grid axis.
data Axis = Row | Col deriving Eq
-- | Parsed CSS grid item properties.
data CSSCell = CSSCell {
    -- | Parsed CSS grid-column-start
    columnStart :: Placement,
    -- | Parsed CSS grid-column-end
    columnEnd :: Placement,
    -- | Parsed CSS grid-row-start
    rowStart :: Placement,
    -- | Parsed CSS grid-row-end
    rowEnd :: Placement,
    -- | Parsed CSS align-self & justify-self
    alignSelf :: Size (Maybe Alignment) (Maybe Alignment)
}
-- | Identifies a cell in the CSS grid.
data Placement = Autoplace | Named Text | Numbered Int (Maybe Text) |
        Span Int (Maybe Text)

instance PropertyParser CSSGrid where
    temp = CSSGrid {
        autoColumns = auto,
        autoFlow = Row,
        autoFlowDense = False,
        autoRows = auto,
        templateAreas = HM.empty,
        templateColumns = [],
        templateRows = [],
        cssGap = Size (0,"px") (0,"px"),
        alignItems = Size Start Start -- FIXME: Should be stretch, unsupported.
    }
    inherit _ = temp
    priority _ = []

    longhand _ s "grid-auto-columns" toks | Just x <- parseFR toks = Just s {autoColumns=x}
    longhand _ s "grid-auto-rows" toks | Just x <- parseFR toks = Just s { autoRows = x }

    longhand _ self "grid-auto-flow" [Ident "row"] = Just self {
        autoFlow = Row, autoFlowDense = False
      }
    longhand _ self "grid-auto-flow" [Ident "column"] = Just self {
        autoFlow = Col, autoFlowDense = False
      }
    longhand _ self "grid-auto-flow" [Ident "row", Ident "dense"] = Just self {
        autoFlow = Row, autoFlowDense = True
      }
    longhand _ self "grid-auto-flow" [Ident "column", Ident "dense"] = Just self {
        autoFlow = Col, autoFlowDense = True
      }

    -- FIXME Parse & validate the ASCII-art grid into rectangles.
    longhand _ self "grid-template-areas" [Ident "none"] =
        Just self { templateAreas = HM.empty }
    longhand _ self "grid-template-areas" [Ident "initial"] =
        Just self { templateAreas = HM.empty }
    longhand _ self "grid-template-areas" toks
        | all isString toks, let grid = [Txt.words x | String x <- toks],
            validate grid, Just areas <- parseASCIIGrid grid 0 HM.empty =
                Just self { templateAreas = areas }
      where
        isString (String _) = True
        isString _ = False
        validate grid@(row:rows) =
            all isValidName (concat grid) && all (\x -> length row == length x) rows
        validate [] = False
        isValidName name = Txt.all (\c -> isAlphaNum c || c == '-') name

    longhand _ self "grid-template-columns" toks | Just x <- parseTemplate toks =
        Just self { templateColumns = x }
    longhand _ self "grid-template-rows" toks | Just x <- parseTemplate toks =
        Just self { templateRows = x}

    longhand _ self "row-gap" toks | Just x <- parseLength toks =
        Just self { cssGap = (cssGap self) { inline = x } }
    longhand _ self "column-gap" toks | Just x <- parseLength toks =
        Just self { cssGap = (cssGap self) { block = x } }

    longhand _ self "justify-items" [Ident "start"] =
        Just self { alignItems = (alignItems self) { inline = Start } }
    longhand _ self "justify-items" [Ident "flex-start"] =
        Just self { alignItems = (alignItems self) { inline = Start } }
    longhand _ self "justify-items" [Ident "self-start"] =
        Just self { alignItems = (alignItems self) { inline = Start } }
    longhand _ self "justify-items" [Ident "left"] =
        Just self { alignItems = (alignItems self) { inline = Start } }
    longhand _ self "justify-items" [Ident "center"] =
        Just self { alignItems = (alignItems self) { inline = Mid } }
    longhand _ self "justify-items" [Ident "end"] =
        Just self { alignItems = (alignItems self) { inline = End } }
    longhand _ self "justify-items" [Ident "flex-end"] =
        Just self { alignItems = (alignItems self) { inline = End } }
    longhand _ self "justify-items" [Ident "self-end"] =
        Just self { alignItems = (alignItems self) { inline = End } }
    longhand _ self "justify-items" [Ident "right"] =
        Just self { alignItems = (alignItems self) { inline = End } }
    longhand parent self "justify-items" (Ident "unsafe":toks) =
        longhand parent self "justify-items" toks
    longhand _ self "justify-items" [Ident "normal"] = -- FIXME Should be stretch, unsupported.
        Just self { alignItems = (alignItems self) { inline = Start } }
    longhand _ self "justify-items" [Ident "initial"] = -- FIXME Should be stretch, unsupported.
        Just self { alignItems = (alignItems self) { inline = Start } }

    longhand _ self "align-items" [Ident "start"] =
        Just self { alignItems = (alignItems self) { block = Start } }
    longhand _ self "align-items" [Ident "flex-start"] =
        Just self { alignItems = (alignItems self) { block = Start } }
    longhand _ self "align-items" [Ident "self-start"] =
        Just self { alignItems = (alignItems self) { block = Start } }
    longhand _ self "align-items" [Ident "center"] =
        Just self { alignItems = (alignItems self) { block = Mid } }
    longhand _ self "align-items" [Ident "end"] =
        Just self { alignItems = (alignItems self) { block = End } }
    longhand _ self "align-items" [Ident "flex-end"] =
        Just self { alignItems = (alignItems self) { block = End } }
    longhand _ self "align-items" [Ident "self-end"] =
        Just self { alignItems = (alignItems self) { block = End } }
    longhand parent self "align-items" (Ident "unsafe":toks) =
        longhand parent self "align-items" toks
    longhand _ self "align-items" [Ident "normal"] = -- FIXME Should be stretch, unsupported.
        Just self { alignItems = (alignItems self) { block = Start } }
    longhand _ self "align-items" [Ident "initial"] = -- FIXME Should be stretch, unsupported.
        Just self { alignItems = (alignItems self) { block = Start } }

    longhand _ _ _ _ = Nothing

instance PropertyParser CSSCell where
    temp = CSSCell {
        columnStart = Autoplace,
        columnEnd = Autoplace,
        rowStart = Autoplace,
        rowEnd = Autoplace,
        alignSelf = Size Nothing Nothing
    }
    inherit _ = temp
    priority _ = []

    longhand _ self "grid-column-start" toks | Just x <- placement toks =
        Just self { columnStart = x}
    longhand _ s "grid-column-end" toks | Just x <- placement toks = Just s {columnEnd=x}
    longhand _ s "grid-row-start" toks | Just x <- placement toks = Just s {rowStart = x}
    longhand _ s "grid-row-end" toks | Just x <- placement toks = Just s { rowEnd = x }

    longhand _ self "align-self" [Ident "start"] =
        Just self { alignSelf = (alignSelf self) { block = Just Start } }
    longhand _ self "align-self" [Ident "self-start"] =
        Just self { alignSelf = (alignSelf self) { block = Just Start } }
    longhand _ self "align-self" [Ident "flex-start"] =
        Just self { alignSelf = (alignSelf self) { block = Just Start } }
    longhand _ self "align-self" [Ident "center"] =
        Just self { alignSelf = (alignSelf self) { block = Just Mid } }
    longhand _ self "align-self" [Ident "end"] =
        Just self { alignSelf = (alignSelf self) { block = Just End } }
    longhand _ self "align-self" [Ident "self-end"] =
        Just self { alignSelf = (alignSelf self) { block = Just End } }
    longhand _ self "align-self" [Ident "flex-end"] =
        Just self { alignSelf = (alignSelf self) { block = Just End } }
    longhand _ self "align-self" [Ident "normal"] = -- FIXME should be stretch, unsupported
        Just self { alignSelf = (alignSelf self) { block = Just Start } }
    longhand parent self "align-self" (Ident "unsafe":toks) =
        longhand parent self "align-self" toks
    longhand _ self "align-self" [Ident "auto"] =
        Just self { alignSelf = (alignSelf self) { block = Nothing } }
    longhand _ self "align-self" [Ident "initial"] =
        Just self { alignSelf = (alignSelf self) { block = Nothing } }

    longhand _ self "justify-self" [Ident "start"] =
        Just self { alignSelf = (alignSelf self) { inline = Just Start } }
    longhand _ self "justify-self" [Ident "self-start"] =
        Just self { alignSelf = (alignSelf self) { inline = Just Start } }
    longhand _ self "justify-self" [Ident "flex-start"] =
        Just self { alignSelf = (alignSelf self) { inline = Just Start } }
    longhand _ self "justify-self" [Ident "left"] =
        Just self { alignSelf = (alignSelf self) { inline = Just Start } }
    longhand _ self "justify-self" [Ident "center"] =
        Just self { alignSelf = (alignSelf self) { inline = Just Mid } }
    longhand _ self "justify-self" [Ident "end"] =
        Just self { alignSelf = (alignSelf self) { inline = Just End } }
    longhand _ self "justify-self" [Ident "self-end"] =
        Just self { alignSelf = (alignSelf self) { inline = Just End } }
    longhand _ self "justify-self" [Ident "flex-end"] =
        Just self { alignSelf = (alignSelf self) { inline = Just End } }
    longhand _ self "justify-self" [Ident "right"] =
        Just self { alignSelf = (alignSelf self) { inline = Just End } }
    longhand _ self "justify-self" [Ident "normal"] = -- FIXME should be stretch, unsupported
        Just self { alignSelf = (alignSelf self) { inline = Just Start } }
    longhand parent self "justify-self" (Ident "unsafe":toks) =
        longhand parent self "justify-self" toks
    longhand _ self "justify-self" [Ident "auto"] =
        Just self { alignSelf = (alignSelf self) { inline = Nothing } }
    longhand _ self "justify-self" [Ident "initial"] =
        Just self { alignSelf = (alignSelf self) { inline = Nothing } }

    longhand _ _ _ _ = Nothing

    shorthand _ "grid-column" toks = case break (== Delim '/') toks of
        (a, Delim '/':b) | Just _ <- placement a, Just _ <- placement b ->
            [("grid-column-start", a), ("grid-column-end", b)]
        _ | Just _ <- placement toks ->
            [("grid-column-start", toks), ("grid-column-end", toks)]
        _ -> []
    shorthand self "grid-gap" toks = case parseOperands toks of
        [a] | Just _ <- longhand self self "grid-row-gap" a ->
            [("grid-row-gap", a), ("grid-column-gap", a)]
        [a, b] | Just _ <- longhand self self "grid-row-gap" a,
            Just _ <- longhand self self "grid-column-gap" b ->
                [("grid-row-gap", a), ("grid-column-gap", b)]
        _ -> []
    shorthand _ "grid-row" toks = case break (== Delim '/') toks of
        (a, Delim '/':b) | Just _ <- placement a, Just _ <- placement b ->
            [("grid-row-start", a), ("grid-row-end", b)]
        _ | Just _ <- placement toks ->
            [("grid-row-start", toks), ("grid-row-end", toks)]
        _ -> []
    shorthand _ "grid-template" toks@[Ident "none"] =
        [("grid-template-columns", toks), ("grid-template-rows", toks),
         ("grid-template-areas", toks)]
    shorthand self "grid-template" toks
        | (rows, Delim '/':cols) <- break (== Delim '/') toks,
            Just _ <- longhand self self "grid-template-rows" rows,
            Just _ <- longhand self self "grid-template-columns" cols =
                [("grid-template-rows", rows), ("grid-template-columns", cols),
                 ("grid-template-areas", [Ident "none"])]
        | (rowsTemplate, Delim '/':cols) <- break (== Delim '/') toks,
            Just (areas, rows) <- splitTemplate rowsTemplate,
            Just _ <- longhand self self "grid-template-cols" cols,
            Just _ <- longhand self self "grid-template-areas" areas =
                [("grid-template-rows", concat rows),
                 ("grid-template-columns", cols), ("grid-template-areas", areas)]
      where
        splitTemplate (LeftSquareBracket:t)
            | (names, RightSquareBracket:t') <- break (== RightSquareBracket) t,
              all isIdent names, Just (areas, row:rows) <- splitTemplate t' =
                Just (areas,
                    (LeftSquareBracket:names ++ RightSquareBracket:row):rows)
        splitTemplate (x@(String _):toks)
            | Just (areas, rows) <- splitTemplate' toks = Just (x:areas, rows)
        splitTemplate _ = Nothing
        splitTemplate' (x:LeftSquareBracket:t)
            | (names, RightSquareBracket:t') <- break (== RightSquareBracket) t,
              all isIdent names, Just _ <- parseFR' [x],
              Just (areas, rows) <- splitTemplate t' =
                Just (areas,
                    (x:LeftSquareBracket:names ++ [RightSquareBracket]):rows)
        splitTemplate' (x:toks)
            | Just _ <- parseFR' [x], Just (areas, rows) <- splitTemplate toks =
                Just (areas, [x]:rows)
        splitTemplate' (LeftSquareBracket:t)
            | (names, RightSquareBracket:t') <- break (== RightSquareBracket) t,
              all isIdent names, Just (areas, rows) <- splitTemplate t' =
                Just (areas,
                    (LeftSquareBracket:names ++ [RightSquareBracket]):rows)
        splitTemplate' toks
            | Just (areas, rows) <- splitTemplate toks = Just (areas, []:rows)
            | otherwise = Nothing
        isIdent (Ident _) = True
        isIdent _ = False
    shorthand self "grid" toks
        | ret@(_:_) <- shorthand self "grid-template" toks =
            ("grid-auto-flow", [Ident "row"]):ret
    shorthand self "grid" toks = case break (== Delim '/') toks of
        (rows, Delim '/':Ident "auto-flow":Ident "dense":cols) |
          Just _ <- longhand self self "grid-template-rows" rows,
          Just _ <- longhand self self "grid-auto-columns" cols ->
            [("grid-template-rows", rows),
             ("grid-template-columns", [Ident "none"]),
             ("grid-auto-columns", cols), ("grid-auto-rows", [Ident "none"]),
             ("grid-auto-flow", [Ident "column", Ident "dense"])]
        (rows, Delim '/':Ident "dense":Ident "auto-flow":cols) |
          Just _ <- longhand self self "grid-template-rows" rows,
          Just _ <- longhand self self "grid-auto-columns" cols ->
            [("grid-template-rows", rows),
             ("grid-template-columns", [Ident "none"]),
             ("grid-auto-columns", cols), ("grid-auto-rows", [Ident "none"]),
             ("grid-auto-flow", [Ident "column", Ident "dense"])]
        (rows, Delim '/':Ident "auto-flow":cols) |
          Just _ <- longhand self self "grid-template-rows" rows,
          Just _ <- longhand self self "grid-auto-columns" cols ->
            [("grid-template-rows", rows),
             ("grid-template-columns", [Ident "none"]),
             ("grid-auto-columns", cols), ("grid-auto-rows", [Ident "none"]),
             ("grid-auto-flow", [Ident "column"])]
        (Ident "auto-flow":Ident "dense":rows, Delim '/':cols) |
          Just _ <- longhand self self "grid-auto-rows" rows,
          Just _ <- longhand self self "grid-template-columns" cols ->
            [("grid-auto-rows", rows), ("grid-auto-columns", [Ident "none"]),
             ("grid-template-columns", cols),
             ("grid-template-rows", [Ident "none"]),
             ("grid-auto-flow", [Ident "row", Ident "dense"])]
        (Ident "dense":Ident "auto-flow":rows, Delim '/':cols) |
          Just _ <- longhand self self "grid-auto-rows" rows,
          Just _ <- longhand self self "grid-template-columns" cols ->
            [("grid-auto-rows", rows), ("grid-auto-columns", [Ident "none"]),
             ("grid-template-columns", cols),
             ("grid-template-rows", [Ident "none"]),
             ("grid-auto-flow", [Ident "row", Ident "dense"])]
        (Ident "auto-flow":rows, Delim '/':cols) |
          Just _ <- longhand self self "grid-auto-rows" rows,
          Just _ <- longhand self self "grid-template-columns" cols ->
            [("grid-auto-rows", rows), ("grid-auto-columns", [Ident "none"]),
             ("grid-template-columns", cols),
             ("grid-template-rows", [Ident "none"]),
             ("grid-auto-flow", [Ident "row"])]
        _ -> []
    shorthand self k v | Just _ <- longhand self self k v = [(k, v)]
        | otherwise = []

-- | Parse a length or FR unit.
parseFR [Dimension _ x "fr"] = Just (n2f x,"fr")
parseFR toks = parseLength toks
-- | Parse a length or FR unit, including extended keywords.
parseFR' [Dimension _ x "fr"] = Just (n2f x,"fr")
parseFR' toks = parseLength' toks

-- | Parse an identifier for a grid cell.
placement [Ident "auto"] = Just $ Autoplace
placement [Ident x] = Just $ Named x
placement [Number _ (NVInteger x)] = Just $ Numbered (fromEnum x) Nothing
placement [Number _ (NVInteger x), Ident y] = Just $ Numbered (fromEnum x) (Just y)
placement [Ident "span", Number _ (NVInteger x)]
    | x > 0 = Just $ Span (fromEnum x) Nothing
placement [Ident "span", Ident x] = Just $ Span 1 $ Just x
placement [Ident "span", Number _ (NVInteger x), Ident y]
    | x > 0 = Just $ Span (fromEnum x) (Just y)
placement [Ident "span", Ident y, Number _ (NVInteger x)]
    | x > 0 = Just $ Span (fromEnum x) (Just y)
placement _ = Nothing

-- | Parse grid-template-*
parseTemplate [Ident "none"] = Just []
parseTemplate [Ident "initial"] = Just []
parseTemplate toks | (tracks@(_:_), []) <- parseTrack toks = Just tracks
parseTemplate _ = Nothing
-- | Parse an individual track specified by grid-template-*
parseTrack (LeftSquareBracket:toks)
    | Just (names', toks') <- parseNames toks,
        ((names,size):cells,toks) <- parseTrack toks' = ((names' ++ names,size):cells,toks)
    | Just (names', toks') <- parseNames toks = ([(names',(0,"end"))],toks')
parseTrack (tok:toks) | Just x <- parseFR' [tok] =
    (([], x):fst (parseTrack toks), snd $ parseTrack toks)
parseTrack (Function "repeat":Number _ (NVInteger x):Comma:toks)
    | x > 0, (tracks@(_:_), RightParen:toks') <- parseTrack toks =
        (concat $ replicate (fromEnum x) tracks, toks')
parseTrack toks = ([], toks)
-- | (UNUSED) Parse a subgrid specified by grid-template-*
parseSubgrid (LeftSquareBracket:toks)
    | Just (names', toks') <- parseNames toks, (names,toks'') <- parseSubgrid toks' =
        (names' : names, toks')
parseSubgrid (Function "repeat":Number _ (NVInteger x):Comma:toks)
    | x > 0, (names@(_:_), RightParen:toks') <- parseSubgrid toks =
        (concat $ replicate (fromEnum x) names, toks')
parseSubgrid toks = ([], toks)
-- | Parse a track's names.
parseNames (Ident x:toks)
    | Just (names,toks') <- parseNames toks = Just (x:names,toks')
parseNames (RightSquareBracket:toks) = Just ([], toks)
parseNames _ = Nothing

-- | Desugar grid properties to a grid layout.
finalizeGrid :: PropertyParser x => CSSGrid -> Font' ->
    [CSSCell] -> [LayoutItem Length Length x] -> LayoutItem Length Length x
finalizeGrid self@CSSGrid {
        templateColumns = cols', templateRows = rows'
    } font cells childs = LayoutGrid temp self' cells' childs
  where
    self' = Size Track {
        cells = map finalizeFR $ map snd rows0,
        trackMins = [], trackNats = [],
        gap = finalizeLength (inline $ cssGap self) font
      } Track {
        cells = map finalizeFR $ map snd cols0,
        trackMins = [], trackNats = [],
        gap = finalizeLength (block $ cssGap self) font
      }

    (cells', rows0, cols0) = finalizeCells cells rows' cols'
    finalizeCells :: [CSSCell] -> [([Text], Unitted)] -> [([Text], Unitted)] ->
            ([GridItem], [([Text], Unitted)], [([Text], Unitted)])
    finalizeCells (cell:cells) rows cols = (cell':cells', rows_, cols_)
      where
        (cell', rows0, cols0) = finalizeCell cell rows cols
        (cells', rows_, cols_) = finalizeCells cells rows0 cols0
    finalizeCells [] rows cols = ([], rows, cols)
    finalizeCell :: CSSCell -> [([Text], Unitted)] -> [([Text], Unitted)] ->
            (GridItem, [([Text], Unitted)], [([Text], Unitted)])
    finalizeCell cell@CSSCell {
            rowStart = Autoplace, columnStart = Autoplace
        } rows cols | autoFlow self == Row =
            finalizeCell cell { columnStart = Numbered 1 Nothing } rows cols
        | autoFlow self == Col =
            finalizeCell cell { rowStart = Numbered 1 Nothing } rows cols
    finalizeCell cell rows cols = (Size GridItem {
            cellStart = startCol', cellEnd = endCol',
            minSize = 0, natSize = 0,
            alignment = fromMaybe (inline $ alignItems self) (inline $ alignSelf cell)
        } GridItem {
            cellStart = startRow', cellEnd = endRow',
            minSize = 0, natSize = 0,
            alignment = fromMaybe (inline $ alignItems self) (inline $ alignSelf cell)
        }, rows', cols')
      where
        (startRow', endRow', rows') = lowerTrack2 rows ([], autoRows self)
                (rowStart cell) (rowEnd cell)
        (startCol', endCol', cols') = lowerTrack2 cols ([], autoColumns self) 
                (columnStart cell) (columnEnd cell)

    lowerTrack2 tracks auto start Autoplace = lowerTrack2 tracks auto start $ Span 1 Nothing
    lowerTrack2 tracks auto start@(Span _ _) end@(Span _ _) =
        lowerTrack2 tracks auto start $ Numbered (pred $ length tracks) Nothing
    lowerTrack2 tracks auto start@(Span _ _) end = (start', end', tracks')
      where
        (end', tracks0) = lowerTrack tracks auto 0 end -- Already checked for spans.
        (start', tracks') = lowerTrack tracks auto (negate end') start
    lowerTrack2 tracks auto start end = (start', end', tracks')
      where
        (start', tracks0) = lowerTrack tracks auto 0 start -- already checked for spans.
        (end', tracks') = lowerTrack tracks auto start' end
    lowerTrack tracks auto _ (Named name)
        | ix:_ <- [ix | (ix, (names, _)) <- enumerate tracks, name `elem` names] = (ix, tracks)
        | otherwise = (length tracks, tracks ++ [auto])

    -- TODO Take into account placement strategy.
    lowerTrack tracks auto _ Autoplace = (length tracks, tracks ++ [auto])
    lowerTrack tracks _ _ (Numbered ix Nothing) = (ix, tracks)
    lowerTrack tracks auto _ (Numbered ix (Just name))
        | ix < length tracks' = (tracks' !! ix, tracks)
        | otherwise = (length tracks, tracks ++ [auto])
      where tracks' = [ix | (ix, (names, _)) <- enumerate tracks, name `elem` names]
    lowerTrack tracks _ start (Span x Nothing)
        | start > 0 = (start + x,tracks)
        | otherwise = (-start - x,tracks)
    lowerTrack tracks (_, auto) start (Span x (Just name)) = (tracks' !! x,tracks)
      where
        tracks0 | start < 0 = reverse tracks
            | otherwise = tracks
        tracks' = [ix | (ix, (names, _)) <-
            drop (abs start) $ enumerate (tracks0 ++ repeat ([name],auto)),
            name `elem` names]

    finalizeFR (x,"fr") = Right x
    finalizeFR x = Left $ finalizeLength x font
