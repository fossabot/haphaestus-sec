{-# LANGUAGE OverloadedStrings #-}
module Graphics.Layout.CSS.Parse (
    CSSBox(..), direction, txtOpts, BoxSizing(..), Display(..)) where
import Data.CSS.Syntax.Tokens (Token(..), NumericValue(..))
import Stylist (PropertyParser(..), TrivialPropertyParser, parseOperands,
                parseUnorderedShorthand', parseUnorderedShorthand)
import Data.Text.ParagraphLayout (PageOptions(..))
import Data.Text.ParagraphLayout.Rich (textDirection, ParagraphOptions,
            defaultParagraphOptions, paragraphAlignment, ParagraphAlignment(..))
import Data.Text.Glyphize (Direction(..))

import Graphics.Layout.Box as B
import Graphics.Text.Font.Choose (Pattern, unset)
import Graphics.Layout.CSS.Length (Unitted, parseLength', parseLength, auto, units)
import Graphics.Layout.CSS.Font (CSSFont)
import Graphics.Layout.Grid.CSS (CSSGrid(..), CSSCell(..), Placement(..))
import Graphics.Layout.Grid.Table (TableOptions)
import Graphics.Layout.Inline.CSS (CSSInline(..))
import Graphics.Layout.Flex.CSS (CSSFlex(..))

import Data.Maybe (isJust, fromMaybe)
import Text.Read (readMaybe)
import qualified Data.HashMap.Lazy as HM
import Data.Text (Text, unpack)
import Debug.Trace (trace) -- For debug warnings.

-- | Parsed CSS properties relevant to layout.
data CSSBox a = CSSBox {
    -- | Which layout formula to use, a.k.a. parsed CSS display property.
    display :: Display,
    -- | (Unused) Parsed CSS box-sizing
    boxSizing :: BoxSizing,
    -- | sizing, margins, border-width, & padding CSS properties.
    -- Stores units in case they're needed for font-related units.
    cssBox :: PaddedBox Unitted Unitted, -- calc()?
    -- | Query parameters describing desired font.
    font :: Pattern,
    -- | Additional font-related CSS properties.
    font' :: CSSFont,
    -- | Caller-specified data, to parse additional CSS properties.
    inner :: a,
    -- | Properties to lower size units before passing onto to `inner`
    innerProperties :: [(Text, [Token])],
    -- | Parent to use when parsing length-expanded inner properties.
    innerParent :: a,
    -- | Grid-related CSS properties.
    gridStyles :: CSSGrid,
    -- | Grid item related CSS properties.
    cellStyles :: CSSCell,
    -- | inline-related CSS properties.
    inlineStyles :: CSSInline,
    -- | Parsed widows & orphans controlling pagination.
    pageOptions :: PageOptions,
    -- | Parsed text-alignment & other options which applies per-paragraph.
    paragraphOptions :: ParagraphOptions,
    -- | (Semi-)parsed CSS properties & HTML attributes relating to laying out
    -- HTML table elements.
    tableOptions :: TableOptions,
    -- | Semi-parsed CSS properties relating to FlexBox layouts.
    flexOptions :: CSSFlex
}
-- | FlexOptions getter with `textLTR` set
flexOpts' self@CSSBox { flexOptions = ret } = ret { textRTL = direction self == DirRTL }
-- | Accessor for inlineStyle's `textDirection` attribute.
direction CSSBox { inlineStyles = CSSInline _ opts _ } = textDirection opts
-- | Accessor for inlineStyle's options.
txtOpts CSSBox { inlineStyles = CSSInline _ opts _ } = opts
-- | Possible values for CSS box-sizing.
data BoxSizing = BorderBox | ContentBox
-- | Empty border, to use as default value.
noborder = Border (0,"px") (0,"px") (0,"px") (0,"px")

-- | Possibly values for CSS display property.
data Display = Block | Grid | Inline | Table | None |
    TableRow | TableHeaderGroup | TableRowGroup | TableFooterGroup | TableCell |
    TableColumn | TableColumnGroup | TableCaption | Flex deriving Eq
-- | Can the display value contain table-rows?
rowContainer CSSBox { display = d } =
    d `elem` [Table, TableHeaderGroup, TableRowGroup, TableFooterGroup]

instance PropertyParser a => PropertyParser (CSSBox a) where
    temp = CSSBox {
        boxSizing = ContentBox,
        display = Inline,
        cssBox = PaddedBox {
            B.min = Size auto auto,
            size = Size auto auto,
            nat = Size 0 0,
            B.max = Size auto auto,
            padding = noborder,
            border = noborder,
            margin = noborder
        },
        font = temp,
        font' = temp,
        inner = temp,
        innerProperties = [],
        innerParent = trace ("Parent not overriden upon " ++
            "buffering inner properties for length resolution!") temp,
        gridStyles = temp,
        cellStyles = temp,
        inlineStyles = temp,
        pageOptions = PageOptions 0 0 2 2,
        paragraphOptions = defaultParagraphOptions {
            paragraphAlignment = AlignStart
        },
        tableOptions = temp,
        flexOptions = temp
      }
    inherit parent = CSSBox {
        boxSizing = boxSizing parent,
        display = Inline,
        cssBox = cssBox (temp :: CSSBox TrivialPropertyParser),
        font = inherit $ font parent,
        font' = inherit $ font' parent,
        inner = inherit $ inner parent,
        innerProperties = [],
        innerParent = inner parent,
        gridStyles = inherit $ gridStyles parent,
        cellStyles = inherit $ cellStyles parent,
        inlineStyles = inherit $ inlineStyles parent,
        pageOptions = pageOptions parent,
        paragraphOptions = paragraphOptions parent,
        tableOptions = inherit $ tableOptions parent,
        flexOptions = inherit $ flexOptions parent
      }
    priority self = concat [x inlineStyles, x font, x font', x gridStyles,
        x cellStyles, x flexOptions, x inner]
      where x getter = priority $ getter self

    -- Wasn't sure how to implement in FontConfig-Pure
    longhand _ self "font-family" [Ident "initial"] =
        Just self { font = unset "family" $ font self}

    longhand _ self "box-sizing" [Ident "content-box"] = Just self {boxSizing = ContentBox}
    longhand _ self "box-sizing" [Ident "border-box"] = Just self {boxSizing = BorderBox}
    longhand _ self "box-sizing" [Ident "initial"] = Just self {boxSizing = ContentBox}

    longhand _ self@CSSBox {cssBox = box} "padding-top" toks | Just x <- parseLength toks =
        Just self { cssBox = box { padding = (padding box) { top = x } } }
    longhand _ self@CSSBox {cssBox = box} "padding-bottom" toks | Just x <- parseLength toks =
        Just self { cssBox = box { padding = (padding box) { bottom = x } } }
    longhand _ self@CSSBox {cssBox = box} "padding-left" toks | Just x <- parseLength toks =
        Just self { cssBox = box { padding = (padding box) { left = x } } }
    longhand _ self@CSSBox {cssBox = box} "padding-right" toks | Just x <- parseLength toks =
        Just self { cssBox = box { padding = (padding box) { right = x } } }
    longhand _ self@CSSBox {cssBox = box} "padding-inline-start" toks
        | Just x <- parseLength toks, DirLTR <- direction self =
            Just self { cssBox = box { padding = (padding box) { left = x } } }
        | Just x <- parseLength toks, DirRTL <- direction self =
            Just self { cssBox = box { padding = (padding box) { right = x } } }
    longhand _ self@CSSBox {cssBox = box} "padding-inline-end" toks
        | Just x <- parseLength toks, DirLTR <- direction self =
            Just self { cssBox = box { padding = (padding box) { right = x } } }
        | Just x <- parseLength toks, DirRTL <- direction self =
            Just self { cssBox = box { padding = (padding box) { left = x } } }

    longhand _ self@CSSBox {cssBox = box} "border-top-width" toks | Just x <- parseLength toks =
        Just self { cssBox = box { border = (border box) { top = x } } }
    longhand _ self@CSSBox {cssBox = box} "border-bottom-width" toks | Just x <- parseLength toks =
        Just self { cssBox = box { border = (border box) { bottom = x } } }
    longhand _ self@CSSBox {cssBox = box} "border-left-width" toks | Just x <- parseLength toks =
        Just self { cssBox = box { border = (border box) { left = x } } }
    longhand _ self@CSSBox {cssBox = box} "border-right-width" toks | Just x <- parseLength toks =
        Just self { cssBox = box { border = (border box) { right = x } } }
    longhand p self "border-inline-start-color" toks
        | DirLTR <- direction self = longhand p self "border-left-color" toks
        | DirRTL <- direction self = longhand p self "border-right-color" toks
    longhand p self "border-inline-start-width" toks
        | DirLTR <- direction self = longhand p self "border-left-width" toks
        | DirRTL <- direction self = longhand p self "border-right-width" toks
    longhand p self "border-inline-start-style" toks
        | DirLTR <- direction self = longhand p self "border-left-style" toks
        | DirRTL <- direction self = longhand p self "border-right-style" toks
    longhand p self "border-inline-end-color" toks
        | DirLTR <- direction self = longhand p self "border-right-color" toks
        | DirRTL <- direction self = longhand p self "border-left-color" toks
    longhand p self "border-inline-end-width" toks
        | DirLTR <- direction self = longhand p self "border-right-width" toks
        | DirRTL <- direction self = longhand p self "border-left-width" toks
    longhand p self "border-inline-end-style" toks
        | DirLTR <- direction self = longhand p self "border-right-style" toks
        | DirRTL <- direction self = longhand p self "border-left-style" toks
    longhand p self "border-start-start-radius" t
        | DirLTR <- direction self = longhand p self "border-top-left-radius" t
        | DirRTL <- direction self = longhand p self "border-top-right-radius" t
    longhand p self "border-start-end-radius" t
        | DirLTR <- direction self = longhand p self "border-top-right-radius" t
        | DirRTL <- direction self = longhand p self "border-top-left-radius" t
    longhand p s "border-end-start-radius" t
        | DirLTR <- direction s = longhand p s "border-bottom-left-radius" t
        | DirRTL <- direction s = longhand p s "border-bottom-right-radius" t
    longhand p s "border-end-end-radius" t
        | DirLTR <- direction s = longhand p s "border-bottom-right-radius" t
        | DirRTL <- direction s = longhand p s "border-bottom-left-radius" t

    longhand _ self@CSSBox {cssBox = box} "margin-top" toks | Just x <- parseLength toks =
        Just self { cssBox = box { margin = (margin box) { top = x } } }
    longhand _ self@CSSBox {cssBox = box} "margin-bottom" toks | Just x <- parseLength toks =
        Just self { cssBox = box { margin = (margin box) { bottom = x } } }
    longhand _ self@CSSBox {cssBox = box} "margin-left" toks | Just x <- parseLength toks =
        Just self { cssBox = box { margin = (margin box) { left = x } } }
    longhand _ self@CSSBox {cssBox = box} "margin-right" toks | Just x <- parseLength toks =
        Just self { cssBox = box { margin = (margin box) { right = x } } }
    longhand _ self@CSSBox {cssBox = box} "margin-inline-start" toks
        | Just x <- parseLength toks, DirLTR <- direction self =
            Just self { cssBox = box { margin = (margin box) { left = x } } }
        | Just x <- parseLength toks, DirRTL <- direction self =
            Just self { cssBox = box { margin = (margin box) { right = x } } }
    longhand _ self@CSSBox {cssBox = box} "margin-inline-end" toks
        | Just x <- parseLength toks, DirLTR <- direction self =
            Just self { cssBox = box { margin = (margin box) { right = x } } }
        | Just x <- parseLength toks, DirRTL <- direction self =
            Just self { cssBox = box { margin = (margin box) { left = x } } }

    -- Placeholder implementations until vertical text is implemented.
    longhand p self "padding-block-start" t = longhand p self "padding-top" t
    longhand p self "padding-block-end" t = longhand p self "padding-bottom" t
    longhand p self "margin-block-start" t = longhand p self "margin-top" t
    longhand p self "margin-block-end" t = longhand p self "margin-bottom" t
    longhand p self "border-block-start-color" toks =
        longhand p self "border-top-color" toks
    longhand p self "border-block-start-style" toks =
        longhand p self "border-top-style" toks
    longhand p self "border-block-start-width" toks =
        longhand p self "border-top-width" toks
    longhand p s "border-block-end-color" t = longhand p s "border-bottom-color" t
    longhand p s "border-block-end-style" t = longhand p s "border-bottom-style" t
    longhand p s "border-block-end-width" t = longhand p s "border-bottom-width" t

    longhand _ self@CSSBox {cssBox = box} "width" toks | Just x <- parseLength' toks =
        Just self { cssBox = box { size = (size box) { inline = x } } }
    longhand _ self@CSSBox {cssBox = box} "height" toks | Just x <- parseLength' toks =
        Just self { cssBox = box { size = (size box) { block = x } } }
    longhand _ self@CSSBox {cssBox = box} "max-width" toks | Just x <- parseLength' toks =
        Just self { cssBox = box { B.max = (B.max box) { inline = x } } }
    longhand _ self@CSSBox {cssBox = box} "min-width" toks | Just x <- parseLength' toks =
        Just self { cssBox = box { B.min = (B.min box) { inline = x } } }
    longhand _ self@CSSBox {cssBox = box} "max-height" toks | Just x <- parseLength' toks =
        Just self { cssBox = box { B.max = (B.max box) { block = x } } }
    longhand _ self@CSSBox {cssBox = box} "min-height" toks | Just x <- parseLength' toks =
        Just self { cssBox = box { B.min = (B.min box) { block = x } } }

    longhand _ self "display" [Ident "block"] = Just self { display = Block }
    longhand _ self "display" [Ident "none"] = Just self { display = None }
    longhand _ self "display" [Ident "grid"] = Just self { display = Grid }
    longhand _ self "display" [Ident "table"] = Just self { display = Table }
    longhand CSSBox { display = Table } self "display" [Ident "table-row-group"] =
        Just self { display=TableRowGroup }
    longhand CSSBox { display = Table } self "display" [Ident "table-header-group"] =
        Just self { display = TableHeaderGroup }
    longhand CSSBox { display = Table } self "display" [Ident "table-footer-group"] =
        Just self { display = TableFooterGroup }
    longhand parent self "display" [Ident "table-row"] | rowContainer parent =
        Just self { display = TableRow }
    longhand CSSBox { display = TableRow } self "display" [Ident "table-cell"] =
        Just self { display = TableCell }
    longhand CSSBox { display = Table } self "display" [Ident "table-column-group"] =
        Just self { display = TableColumnGroup }
    longhand CSSBox { display = TableColumnGroup } self "display" [Ident "table-column"] =
        Just self { display = TableColumn }
    longhand CSSBox { display = Table } self "display" [Ident "table-caption"] =
        Just self { display=TableCaption }
    longhand _ self "display" [Ident "inline"] = Just self { display = Inline }
    longhand _ self "display" [Ident "flex"] = Just self { display = Flex }
    longhand _ self "display" [Ident "initial"] = Just self { display = Inline }

    longhand _ self "orphans" [Number _ (NVInteger x)] =
        Just self { pageOptions = (pageOptions self) { pageOrphans = fromInteger x } }
    longhand _ self "widows" [Number _ (NVInteger x)] =
        Just self { pageOptions = (pageOptions self) { pageWidows = fromInteger x } }

    longhand _ self@CSSBox {paragraphOptions=o} "text-align" [Ident "initial"] =
        Just self { paragraphOptions = o { paragraphAlignment = AlignStart } }
    longhand _ self@CSSBox {paragraphOptions=o} "text-align" [Ident "start"] =
        Just self { paragraphOptions = o { paragraphAlignment = AlignStart } }
    longhand _ self@CSSBox {paragraphOptions=o} "text-align" [Ident "end"] =
        Just self { paragraphOptions = o { paragraphAlignment = AlignEnd } }
    longhand _ self@CSSBox {paragraphOptions=o} "text-align" [Ident "left"] =
        Just self { paragraphOptions = o { paragraphAlignment = AlignLeft } }
    longhand _ self@CSSBox {paragraphOptions=o} "text-align" [Ident "right"] =
        Just self { paragraphOptions = o { paragraphAlignment = AlignRight } }
    longhand _ self@CSSBox {paragraphOptions=o} "text-align" [Ident "center"] =
        Just self { paragraphOptions = o { paragraphAlignment = AlignCentreH } }
    -- text-align: justify is unimplemented.
    longhand p self@CSSBox { paragraphOptions = o } "text-align"
            [Ident "match-parent"] = case paragraphAlignment$paragraphOptions p of
        AlignStart | DirLTR <- direction p -> ret AlignLeft
        AlignStart | DirRTL <- direction p -> ret AlignRight
        AlignEnd | DirLTR <- direction p -> ret AlignRight
        AlignEnd | DirRTL <- direction p -> ret AlignLeft
        x -> ret x
      where ret x = Just self { paragraphOptions = o { paragraphAlignment = x } }

    longhand a b c d | Just x <- longhand (font a) (font b) c d,
        Just y <- longhand (font' a) (font' b) c d =
            Just b { font = x, font' = y } -- Those properties can overlap!
    longhand a b c d | Just font' <- longhand (font a) (font b) c d = Just b {
        font = font'
      }
    longhand a b c d | Just font <- longhand (font' a) (font' b) c d = Just b {
        font' = font
      }
    longhand a b c d | Just inline' <- longhand (inlineStyles a) (inlineStyles b) c d =
        Just b { inlineStyles = inline' }
    longhand a b c d | Just grid' <- longhand (gridStyles a) (gridStyles b) c d,
            Just flex' <- longhand (flexOpts' a) (flexOpts' b) c d =
        Just b { gridStyles = grid', flexOptions = flex' }
    longhand a b c d | Just cell' <- longhand (cellStyles a) (cellStyles b) c d,
            Just flex' <- longhand (flexOpts' a) (flexOpts' b) c d =
        Just b { cellStyles = cell', flexOptions = flex' }
    longhand a b c d | Just grid' <- longhand (gridStyles a) (gridStyles b) c d =
        Just b { gridStyles = grid' }
    longhand a b c d | Just cell' <- longhand (cellStyles a) (cellStyles b) c d =
        Just b { cellStyles = cell' }
    longhand a b c d | Just table'<-longhand (tableOptions a) (tableOptions b) c d
        = Just b { tableOptions = table' }
    longhand a b c d | Just flex' <- longhand (flexOpts' a) (flexOpts' b) c d =
        Just b { flexOptions = flex' }
    longhand a b c d
        | (d', _:_)<-testLengthProp d, Just _<-longhand (inner a) (inner b) c d' =
            Just b {
                innerProperties = (c, d):innerProperties b,
                innerParent = inner a
            }
    longhand a b c d | Just inner' <- longhand (inner a) (inner b) c d = Just b {
        inner = inner'
      }

    -- Technically a grid shorthand, but we need parent data to parse it!
    longhand CSSBox { gridStyles = parent } self "grid-area" [Ident x]
        | Just ((colS, colE), (rowS, rowE)) <- x `HM.lookup` templateAreas parent
            = Just self { cellStyles = (cellStyles self) {
                columnStart = p colS,
                columnEnd = p colE,
                rowStart = p rowS,
                rowEnd = p $ fromMaybe (length $ templateAreas parent) rowE
            }}
      where p x = Numbered x Nothing

    longhand _ _ _ _ = Nothing

    shorthand self "font" toks = case parseOperands toks of
        (a:b:c:d:toks') | ret@(_:_) <- unordered [a,b,c,d] -> inner ret toks'
        (a:b:c:toks') | ret@(_:_) <- unordered [a,b,c] -> inner ret toks'
        (a:b:toks') | ret@(_:_) <- unordered [a,b] -> inner ret toks'
        (a:toks') | ret@(_:_) <- unordered [a] -> inner ret toks'
        toks' -> inner [] toks'
      where
        unordered operands = parseUnorderedShorthand' self [
            "font-style", "font-variant", "font-weight", "font-stretch"] operands
        inner ret (size:[Delim '/']:height:family)
            | Just _ <- longhand self self "font-size" size,
              Just _ <- longhand self self "line-height" height,
              Just _ <- longhand self self "font-family" $ concat family =
                ("font-size", size):("line-height", height):
                    ("font-family", concat family):ret
            | otherwise = []
        inner ret (size:family)
            | Just _ <- longhand self self "font-size" size,
              Just _ <- longhand self self "font-family" $ concat family =
                ("font-size", size):("line-height", [Ident "initial"]):
                    ("font-family", concat family):ret
            | otherwise = []
        inner _ _ = []
    shorthand self "margin" toks
        | length x > 0 && length x <= 4, all (validProp self "margin-top") x,
            (top:right:bottom:left:_) <- cycle x =
                [("margin-top", top), ("margin-right", right),
                 ("margin-bottom", bottom), ("margin-left", left)]
      where x = parseOperands toks
    shorthand self "padding" toks
        | length x > 0 && length x <= 4, all (validProp self "padding-top") x,
            (top:right:bottom:left:_) <- cycle x =
                [("padding-top", top), ("padding-right", right),
                 ("padding-bottom", bottom), ("padding-left", left)]
      where x = parseOperands toks
    shorthand self "border-width" toks
        | length x > 0 && length x <= 4, (top:right:bottom:left:_) <- cycle x,
            all (validProp self "border-top-width") x =
                [("border-top-width", top), ("border-right-width", right),
                 ("border-bottom-width", bottom), ("border-left-width", left)]
      where x = parseOperands toks

    shorthand self k v | ret@(_:_) <- shorthand (font self) k v = ret
    shorthand self k v | ret@(_:_) <- shorthand (font' self) k v = ret
    shorthand self k v | ret@(_:_) <- shorthand (inlineStyles self) k v = ret
    shorthand self k v | ret@(_:_) <- shorthand (gridStyles self) k v = ret
    shorthand self k v | ret@(_:_) <- shorthand (cellStyles self) k v = ret
    shorthand self k v | ret@(_:_) <- shorthand (tableOptions self) k v = ret
    shorthand self k v | ret@(_:_) <- shorthand (inner self) k v = ret
    shorthand self k v
        | (v', ls)<-testLengthProp v, ret@(_:_)<-shorthand (inner self) k v' =
            [(key, map (restore ls) value) | (key, value) <- ret]
      where
        restore ls (Dimension _ (NVInteger x) "px") | x' < length ls = ls !! x'
          where x' = fromInteger x
        restore _ ret = ret
    shorthand self k v | Just _ <- longhand self self k v = [(k, v)]
        | otherwise = []

validProp self key value = isJust $ longhand self self key value

testLengthProp (tok@(Dimension _ _ unit):toks) | unit `elem` units =
    let (toks', lengths) = testLengthProp toks
    in (Dimension "" (NVInteger $ toInteger $ succ $ length lengths) "px":toks',
        tok:lengths)
testLengthProp (tok:toks) = let (toks',ls) = testLengthProp toks in (tok:toks',ls)
testLengthProp [] = ([], [])
