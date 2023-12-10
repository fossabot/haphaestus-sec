{-# LANGUAGE OverloadedStrings #-}
-- These following language extensions are to aid a dependency injection into
-- inline styling.
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
-- | Parses & desugars CSS properties to general CatTrap datastructures.
module Graphics.Layout.CSS(CSSBox(..), BoxSizing(..), Display(..),
        finalizeCSS, finalizeCSS') where

import qualified Data.Text as Txt
import Stylist (PropertyParser(..))
import Stylist.Tree (StyleTree(..))
import Data.Text.ParagraphLayout.Rich (constructParagraph, defaultBoxOptions,
        LineHeight(..), InnerNode(..), Box(..), RootNode(..))

import Graphics.Layout.Box as B
import Graphics.Layout
import Graphics.Layout.CSS.Length
import Graphics.Layout.CSS.Font
import Graphics.Layout.Grid.CSS
import Graphics.Layout.Grid
import Graphics.Layout.Grid.Table
import Graphics.Layout.Inline.CSS
import Graphics.Layout.Flex.CSS

import Data.Char (isSpace)
import Graphics.Layout.CSS.Parse
import Data.Maybe (fromMaybe)

instance (PropertyParser x, Zero m, Zero n) => Default (UserData m n x) where
    def = ((placeholderFont, 0), zero, temp)

-- | Resolves length units in properties handled by downstream components.
inner' :: PropertyParser x => Font' -> CSSBox x -> x
inner' f self = foldr apply (inner self) $ innerProperties self
  where apply (k, v) ret = fromMaybe ret $
            longhand (innerParent self) ret k $ finalizeLengths f v

-- | Desugar parsed CSS into more generic layout parameters.
finalizeCSS :: PropertyParser x => Font' -> Font' -> StyleTree (CSSBox x) ->
        LayoutItem Length Length x
finalizeCSS root parent StyleTree { style = self'@CSSBox { display = None } } =
    LayoutFlow (inner' parent self') lengthBox []
finalizeCSS root parent self@StyleTree {
    style = self'@CSSBox { display = Grid }, children = childs
  } = LayoutFlow (inner' font_ self') (finalizeBox self' font_) [
        finalizeGrid (gridStyles self') font_ (map cellStyles $ map style childs)
            (finalizeChilds root font_ self' childs)]
  where
    font_ = pattern2font (font self') (font' self') parent root
finalizeCSS root parent self@StyleTree {
        style = self'@CSSBox {
            display = Table, tableOptions = opts@TableOptions {captionBelow=False}
        },
        children = childs
    } = LayoutFlow (inner' font_ self')
        (finalizeBox (collapseTBorders' self') font_)
        ([finalizeCSS root font_ child { style = child' { display = Block } }
            | child@StyleTree { style = child'@CSSBox { display = TableCaption } } <- childs] ++
        [finalizeTable root font_ temp opts childs])
  where
    font_ = pattern2font (font self') (font' self') parent root
finalizeCSS root parent self@StyleTree {
        style = self'@CSSBox {
            display = Table, tableOptions = opts@TableOptions {captionBelow=True}
        }, children = childs
    } = LayoutFlow (inner' font_ self')
        (finalizeBox (collapseTBorders' self') font_)
        (finalizeTable root font_ temp opts childs:
        [finalizeCSS root font_ child { style = child' { display = Block } }
            | child@StyleTree { style = child'@CSSBox { display = TableCaption } } <- childs])
  where
    font_ = pattern2font (font self') (font' self') parent root
finalizeCSS root parent self@StyleTree {
        style = self'@CSSBox { display = Flex, flexOptions = flex },
        children = childs
    } = LayoutFlow (inner' font_ self' ) (finalizeBox self' font_) [
        LayoutFlex temp $ lowerFlex flex font_ (map flexOptions childs')
            (flip map childs $ finalizeCSS root font_) (map style2font childs')]
  where
    font_ = style2font self'
    style2font style = pattern2font (font style) (font' style) parent root
    childs' = map style childs
finalizeCSS root parent self@StyleTree {
    style = self', children = childs
  } = LayoutFlow (inner' font_ self') (finalizeBox self' font_)
        (finalizeChilds root font_ self' childs)
  where
    font_ = pattern2font (font self') (font' self') parent root
-- | Desugars parsed CSS with a provided system font into more generic layout parameters.
finalizeCSS' sysfont self@StyleTree { style = self' } =
    finalizeCSS (pattern2font (font self') (font' self') sysfont sysfont) sysfont self

-- | Desugar a sequence of child nodes, taking care to capture runs of inlines.
finalizeChilds :: PropertyParser x => Font' -> Font' -> CSSBox x ->
        [StyleTree (CSSBox x)] -> [LayoutItem Length Length x]
finalizeChilds root parent style' (StyleTree { style = CSSBox { display = None } }:childs) =
    finalizeChilds root parent style' childs
finalizeChilds root parent style' childs@(child:_)
    | isInlineTree childs, Just self <- finalizeParagraph (flattenTree0 childs) =
        [LayoutInline (inherit $ inner' parent style') self paging]
    | (inlines@(_:_), blocks) <- spanInlines childs,
        Just self <- finalizeParagraph (flattenTree0 inlines) =
            LayoutInline (inherit $ inner' parent style') self paging :
                finalizeChilds root parent style' blocks
    | (StyleTree { style = CSSBox { display = Inline } }:childs') <- childs =
        finalizeChilds root parent style' childs' -- Inline's all whitespace...
  where
    paging = pageOptions $ style child
    isInlineTree = all isInlineTree0
    isInlineTree0 StyleTree { style = CSSBox { display = Inline }, children = childs } =
        isInlineTree childs
    isInlineTree0 _ = False
    spanInlines childs = case span isInlineTree0 childs of
        (inlines, (StyleTree {
            style = CSSBox { display = Inline }, children = tail
          }:blocks)) -> let (inlines', blocks') = spanInlines tail
            in (inlines ++ inlines', blocks' ++ blocks)
        ret -> ret
    flattenTree0 childs
        | iStyle@(CSSInline _ _ bidi) <- inlineStyles style',
            bidi `elem` [BdOverride, BdIsolateOverride] = RootBox $ Box
                (applyBidi iStyle $ map (flattenTree parent) $ enumerate childs)
                $ flip applyFontInline parent $ txtOpts style'
        | otherwise = RootBox $ Box (map (flattenTree parent) $ enumerate childs)
            $ flip applyFontInline parent $ txtOpts style'
    flattenTree p (i, StyleTree self child@(_:_)) =
        buildInline f i self $ map (flattenTree f) $ enumerate child
      where f = pattern2font (font self) (font' self) p root
    flattenTree f (i,StyleTree {style=self@CSSBox {inlineStyles=CSSInline txt _ _}})
        = buildInline f i self [
            TextSequence ((f, 0), zero, inherit $ inner' parent self) txt]
    buildInline f i self childs =
        InlineBox ((f, i), finalizeBox self f, inner' parent self)
                (Box childs' $ flip applyFontInline f $ txtOpts self)
                $ resolveBoxOpts f (tableOptions self)
      where childs' = applyBidi (inlineStyles self) childs
    finalizeParagraph (RootBox (Box [TextSequence _ txt] _))
        | Txt.all isSpace txt = Nothing -- Discard isolated whitespace.
    finalizeParagraph tree =
        Just $ constructParagraph "" tree "" $ paragraphOptions style'
    enumerate = zip $ enumFrom 0
finalizeChilds root parent style'@CSSBox { tableOptions = tOpts } childs
    | (_:_) <- table = finalizeTable root parent temp tOpts table:
        finalizeChilds root parent style' rest
    | (child:childs') <- childs = finalizeCSS root parent child:
        finalizeChilds root parent style' childs'
    | otherwise = []
  where
    (table, rest) = span isTable childs
    isTable (StyleTree CSSBox { display = TableRow } _) = True
    isTable (StyleTree CSSBox { display = TableHeaderGroup } _) = True
    isTable (StyleTree CSSBox { display = TableRowGroup } _) = True
    isTable (StyleTree CSSBox { display = TableFooterGroup } _) = True
    isTable (StyleTree CSSBox { display = TableCell } _) = True
    isTable (StyleTree CSSBox { display = TableColumn } _) = True
    isTable (StyleTree CSSBox { display = TableColumnGroup } _) = True
    -- Treat TableCaption as a block element!
    isTable _ = False

-- | Desugar most units, possibly in reference to given font.
finalizeBox self@CSSBox { cssBox = box } font_ =
    mapY' (flip finalizeLength font_) $ mapX' (flip finalizeLength font_) box

-- | Desugar a styletree of table elements to a grid layout.
finalizeTable root parent val opts childs = LayoutGrid val grid cells' childs'
  where
    grid = Track {
        cells = replicate width $ Left Auto,
        gap = hGap,
        trackMins = [], trackNats = []
      } `Size` Track {
        cells = replicate height $  Left Auto,
        gap = yGap,
        trackMins = [], trackNats = []
      }
    (cells', childs') = unzip (decor ++ cells)
    (hGap, yGap) = finalizeGap opts parent

    (cells, width, height) = lowerCells childs 0 emptyRow
    decor = decorateRow childs width 0 ++ decorateCol childs height 0
    lowerCells (StyleTree self@CSSBox { display = TableRow } cells:rest) h x =
        (row ++ rows, Prelude.max rowwidth width', height')
      where
        (row, rowwidth, x') = lowerRow cells 0 h x
        (rows, width', height') = lowerCells rest (succ h) $ commitRow x'
    lowerCells (StyleTree CSSBox { display = TableHeaderGroup } childs:rest) h x =
        -- Ignore table-header-group styles for now...
        -- Though it'd be nice for this to impact pagination...
        lowerCells (childs ++ rest) h x
    lowerCells (StyleTree CSSBox { display = TableFooterGroup } childs:rest) h x =
        lowerCells (childs ++ rest) h x -- As per TableHeaderGroup
    lowerCells (StyleTree CSSBox { display = TableRowGroup } childs:rest) h x =
        lowerCells (childs ++ rest) h x -- As per TableHeaderGroup
    lowerCells (StyleTree CSSBox { display = TableColumnGroup } _:rest) h x =
        lowerCells rest h x -- It'd be nice to allow styling based on this...
    lowerCells (StyleTree CSSBox { display = TableColumn } _:rest) h x =
        lowerCells rest h x -- As per TableColumnGroup, should be contained within.
    lowerCells (StyleTree CSSBox { display = TableCaption } _:rest) h x =
        lowerCells rest h x -- Handled by callers!
    lowerCells [] h _ = ([], 0, h)
    lowerCells items h x = (row ++ rows, Prelude.max rowwidth width', height')
      where
        (cells, rest) = break isRowGroup items
        (row, rowwidth, x') = lowerRow cells 0 h x
        (rows, width', height') = lowerCells rest (succ h) $ commitRow x'

    lowerRow (StyleTree self@CSSBox {
            display = TableCell, tableOptions = self' } childs:rest) ix row x =
        (cell:cells, width, x')
      where
        (cells, width, x') = lowerRow rest end row $
            insertCell start (colspan self') (rowspan self') x
        start = allocCol ix x
        end = start + colspan self'
        cell = (GridItem start end Start 0 0
                `Size` GridItem row (row + rowspan self') valign 0 0,
            finalizeCSS root parent $ StyleTree self { display = Block } childs)
        valign = finalizeVAlign self'
        halign = finalizeHAlign (paragraphOptions self) (direction self)
    lowerRow (self:rest) ix row x = (cell:cells, width, x')
      where
        ix' = allocCol ix x
        (cells, width, x') = lowerRow rest (succ ix') row $ insertCell ix' 1 1 x
        cell = (GridItem ix' (succ ix') Start 0 0
                `Size` GridItem row (succ row) Start 0 0,
            finalizeCSS root parent self {
                style = (style self) {
                    cssBox = collapseBorders opts $ cssBox $ style self
                }
            })
    lowerRow [] ix _ x = ([], ix, x)

    decorateRow (StyleTree self@CSSBox { display = TableRow } _:rest) w row =
        buildDecor self 0 w row 1:decorateRow rest w (succ row)
    decorateRow (StyleTree self@CSSBox { display = d } childs:rest) w row
        | d `elem` [TableHeaderGroup, TableFooterGroup, TableRowGroup] =
            buildDecor self 0 w row (countRows childs):
                decorateRow (childs ++ rest) w (row + countRows childs)
        | d `elem` [TableCaption, TableColumn, TableColumnGroup] =
            decorateRow rest w row
        | otherwise = decorateRow (dropWhile (not . isRowGroup) rest) w$succ row
    decorateRow [] _ _ = []
    decorateCol (StyleTree self@CSSBox { display = TableColumn } _:rest) h col =
        buildDecor self col 1 0 h:decorateCol rest h (succ col)
    decorateCol (StyleTree self@CSSBox { display = TableColumnGroup } childs:rest)
        h col = buildDecor self col (countCols' childs self) 0 h:
            decorateCol (childs ++ rest) h (col + countCols' childs self)
    decorateCol (_:rest) h col = decorateCol rest h col
    decorateCol [] _ _ = []

    countRows (StyleTree CSSBox { display = TableRow } _:rest) =
        succ $ countRows rest
    countRows (StyleTree CSSBox { display = d } childs:rest)
        | d `elem` [TableHeaderGroup, TableFooterGroup, TableRowGroup] =
            countRows childs + countRows rest
        | d `elem` [TableCaption, TableColumn, TableColumnGroup] = countRows rest
        | otherwise = succ $ countRows $ dropWhile (not . isRowGroup) rest
    countRows [] = 0
    countCols' cols@(_:_) _ = countCols cols
    countCols' _ CSSBox { tableOptions = TableOptions { colspan = x } } = x
    countCols (StyleTree CSSBox {
            display = TableColumn,
            tableOptions = TableOptions { colspan = x }
        } _:rest) = x + countCols rest
    countCols (StyleTree CSSBox {
            display = TableColumnGroup,
            tableOptions = TableOptions { colspan = x }
        } []:rest) = x + countCols rest
    countCols (StyleTree CSSBox { display = TableColumnGroup } childs:rest) =
        countCols childs + countCols rest
    countCols (_:rest) = countCols rest
    countCols [] = 0

    buildDecor self col colspan row rowspan =
        (GridItem col (col + colspan) Start 0 0 `Size`
            GridItem row (row + rowspan) Start 0 0,
            finalizeCSS root parent $ StyleTree self {
                display = Block, cssBox = collapseBorders opts $ cssBox self
            } [])

    isRowGroup (StyleTree CSSBox { display = TableRow } _) = True
    isRowGroup (StyleTree CSSBox { display = TableHeaderGroup } _) = True
    isRowGroup (StyleTree CSSBox { display = TableFooterGroup } _) = True
    isRowGroup (StyleTree CSSBox { display = TableRowGroup } _) = True
    isRowGroup (StyleTree CSSBox { display = TableCaption } _) = True
    isRowGroup (StyleTree CSSBox { display = TableColumn } _) = True
    isRowGroup (StyleTree CSSBox { display = TableColumnGroup } _) = True
    isRowGroup _ = False

-- | Applies border-collapse to a table element.
collapseTBorders' :: CSSBox x -> CSSBox x
collapseTBorders' self = self {
    cssBox = collapseTBorders (tableOptions self) (cssBox self)
  }
