{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
-- | Datastructures for parsing table styling properties,
-- & for positioning cells into Grid layout regions.
module Graphics.Layout.Grid.Table where

import Data.CSS.Syntax.Tokens (Token(..), NumericValue(..))
import Stylist (PropertyParser(..))
import Graphics.Layout.CSS.Length (Unitted, parseLength, Font', finalizeLength)
import Graphics.Layout.Box (Length(..), PaddedBox(..), zero, mapX, mapY)
import Graphics.Layout.Grid (Alignment(..))
import Data.Text.Glyphize (Direction(..))
import Data.Text.ParagraphLayout.Rich (
        ParagraphOptions(..), ParagraphAlignment(..))

import Text.Read (readMaybe)
import Data.Text (unpack)

-- | Tracks `rowspan` attributes so later rows can dodge it.
type Overflowed = [Int]

-- | A row with no cells overflowing into it.
emptyRow :: Overflowed
emptyRow = []

-- | Decrement all `rowspan`s being overflowed, removing 0'd ones.
commitRow :: Overflowed -> Overflowed
commitRow = map $ Prelude.max 0 . pred

-- | Find the next column which a previous multi-row cell hasn't called "dibs" on.
allocCol :: Int -> Overflowed -> Int
allocCol ix cols = ix + length (span (> 0) $ drop ix cols)

-- | Splice a newly-allocated cell covernig `colspan` (2nd arg) & `rowspan` (3rd arg)
-- from "ix" (from 1st arg) into the final arg.
insertCell :: Int -> Int -> Int -> Overflowed -> Overflowed
insertCell ix colspan rowspan cols = before ++ inner colspan after
  where
    (before, after) = splitAt ix cols
    inner x cols' | x <= 0 = cols'
    inner colspan (col:cols') = Prelude.max col rowspan:inner (pred colspan) cols'
    inner x [] = replicate x colspan

-- | Parsed CSS properties & HTML attributes for laying out "table" elements.
-- To parse HTML attributes, expects the following useragent stylesheet rules:
--
-- [rowspan] { -argo-rowspan: attr(rowspan) }
-- [colspan] { -argo-colspan: attr(colspan) }
data TableOptions = TableOptions {
    -- | HTML rowspan attribute
    rowspan :: Int,
    -- | HTML colspan attribute
    colspan :: Int,
    -- | Parsed CSS caption-side.
    captionBelow :: Bool,
    -- | Parsed CSS border-collapse
    borderCollapse :: Bool,
    -- | Semi-parsed border-spacing, horizontal axis
    borderHSpacing :: Unitted,
    -- | Semi-parsed border-spacing, vertical axis
    borderVSpacing :: Unitted,
    -- TODO: Implement `table-layout: fixed`, that needs its own layout formula...
    -- | Parsed CSS vertical-align
    verticalAlign :: Unitted
}

instance PropertyParser TableOptions where
    temp = TableOptions {
        rowspan = 1, colspan = 1,
        captionBelow = False, borderCollapse = False,
        borderHSpacing = (0,"px"), borderVSpacing = (0,"px"),
        verticalAlign = (0,"baseline")
    }
    inherit = id

    longhand _ self "-argo-rowspan" [Ident "initial"] = Just self { rowspan = 1 }
    longhand _ self "-argo-rowspan" [String x]
        | Just y <- readMaybe $ unpack x, y >= 1 = Just self { rowspan = y }
    longhand _ self "-argo-rowspan" [Number _ (NVInteger x)]
        | x >= 1 = Just self { rowspan = fromEnum x }
    longhand _ self "-argo-colspan" [Ident "initial"] = Just self { colspan = 1 }
    longhand _ self "-argo-colspan" [String x]
        | Just y <- readMaybe $ unpack x, y >= 1 = Just self { colspan = y }
    longhand _ self "-argo-colspan" [Number _ (NVInteger x)]
        | x >= 1 = Just self { colspan = fromEnum x }

    longhand _ self "caption-side" [Ident "top"] = Just self { captionBelow = False }
    longhand _ self "caption-side" [Ident "bottom"] = Just self { captionBelow = True }
    longhand _ self "caption-side" [Ident "initial"] = Just self {captionBelow = False}

    longhand _ self "border-collapse" [Ident "collapse"] =
        Just self { borderCollapse = True }
    longhand _ self "border-collapse" [Ident "separate"] =
        Just self { borderCollapse = False }
    longhand _ self "border-collapse" [Ident "initial"] =
        Just self { borderCollapse = False }

    longhand _ self "border-spacing" v@[Dimension _ _ _] | Just x <- parseLength v =
        Just self { borderHSpacing = x, borderVSpacing = x }
    longhand _ self "border-spacing" [x@(Dimension _ _ _), y@(Dimension _ _ _)]
            | Just x' <- parseLength [x], Just y' <- parseLength [y] =
        Just self { borderHSpacing = x', borderVSpacing = y' }
    longhand _ self "border-spacing" [Ident "initial"] =
        Just self { borderHSpacing = (0,"px"), borderVSpacing = (0,"px") }

    longhand _ self "vertical-align" [Ident x]
        | x `elem` ["baseline", "sub", "super", "text-top", "text-bottom",
            "middle", "top", "bottom"] = Just self { verticalAlign = (0,x) }
        | x == "initial" = Just self { verticalAlign = (0,"baseline") }
        | otherwise = Nothing
    longhand _ self "vertical-align" v | Just x <- parseLength v =
        Just self { verticalAlign = x }

    longhand _ _ _ _ = Nothing

-- | Resolve any units in the "border-spacing" property according to the given font.
-- If "border-collapse" is set, removes this spacing.
finalizeGap :: TableOptions -> Font' -> (Length, Length)
finalizeGap TableOptions { borderCollapse = True } _ = (Pixels 0, Pixels 0)
finalizeGap TableOptions { borderHSpacing = x, borderVSpacing = y } font =
    (finalizeLength x font, finalizeLength y font)

-- | Shorthand for a padded box without its CSS units resolved, simplifies type signatures.
type UPaddedBox = PaddedBox Unitted Unitted
-- | Removes margins & halves borders if "border-collapse" is set,
-- as per the CSS specification. Apply this on the table cells, rows, & columns.
collapseBorders :: TableOptions -> UPaddedBox -> UPaddedBox
collapseBorders TableOptions { borderCollapse = False } ret = ret
collapseBorders _ box = box {
    margin = zero,
    border = mapX half $ mapY half $ border box
  }
-- | Removes padding & halves borders if "border-collapse" is set,
-- as per the CSS specification. Apply this on the table itself.
collapseTBorders :: TableOptions -> UPaddedBox -> UPaddedBox
collapseTBorders TableOptions { borderCollapse = False } ret = ret
collapseTBorders _ box = box {
    padding = zero,
    border = mapX half $ mapY half $ border box
  }
-- | Helper for halving a unit.
half (x,u) = (x/2,u)

-- | Lower vertical alignment to grid alignment options.
finalizeVAlign :: TableOptions -> Alignment
finalizeVAlign TableOptions { verticalAlign = (_,"top") } = Start
finalizeVAlign TableOptions { verticalAlign = (_,"middle") } = Mid
finalizeVAlign TableOptions { verticalAlign = (_,"bottom") } = End
finalizeVAlign _ = Start -- FIXME: Support baseline alignment!
-- | Lower text alignment to grid alignment.
finalizeHAlign :: ParagraphOptions -> Direction -> Alignment
finalizeHAlign (paragraphAlignment -> AlignStart) _ = Start
finalizeHAlign (paragraphAlignment -> AlignEnd) _ = End
finalizeHAlign (paragraphAlignment -> AlignLeft) DirLTR = Start
finalizeHAlign (paragraphAlignment -> AlignLeft) _ = End
finalizeHAlign (paragraphAlignment -> AlignRight) DirLTR = End
finalizeHAlign (paragraphAlignment -> AlignRight) _ = Start
finalizeHAlign (paragraphAlignment -> AlignCentreH) _ = Mid
