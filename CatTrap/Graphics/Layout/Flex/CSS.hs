{-# LANGUAGE OverloadedStrings #-}
module Graphics.Layout.Flex.CSS(CSSFlex(..), lowerFlex) where

import Data.CSS.Syntax.Tokens (Token(..), NumericValue(..))
import Stylist (PropertyParser(..), TrivialPropertyParser, parseOperands,
                parseUnorderedShorthand', parseUnorderedShorthand)
import Graphics.Layout.Flex as F
import Graphics.Layout.CSS.Length (parseLength, finalizeLength, n2f, Unitted, Font')
import Graphics.Layout.Box (Length)
import Data.Maybe (isJust)

data CSSFlex = CSSFlex {
    directionCSS :: Direction,
    reverseRowsCSS :: Bool,
    wrapCSS :: FlexWrapping,
    justifyCSS :: Maybe Justification,
    alignItemsCSS :: Alignment,
    alignLinesCSS :: Maybe Justification, -- `Nothing` is "stretch"
    rowGapCSS :: Unitted,
    columnGapCSS :: Unitted,
    -- flex children
    orderCSS :: Integer,
    growCSS :: Double,
    shrinkCSS :: Double,
    basisCSS :: Unitted,
    alignSelfCSS :: Alignment,
    textRTL :: Bool -- Extra parameter from caller.
}

setDir self dir rev = Just self { directionCSS = dir, reverseRowsCSS = rev }

parseJustify self "flex-start" | reverseRowsCSS self = Just JEnd
    | otherwise = Just JStart
parseJustify self "flex-end" | reverseRowsCSS self = Just JStart
    | otherwise = Just JEnd
parseJustify self "start" | textRTL self = Just JEnd
    | otherwise = Just JStart
parseJustify self "end" | textRTL self = Just JStart
    | otherwise = Just JEnd
parseJustify _ "left" = Just JStart
parseJustify _ "right" = Just JEnd
parseJustify _ "center" = Just JCenter
parseJustify _ "space-between" = Just JSpaceBetween
parseJustify _ "space-around" = Just JSpaceAround
parseJustify _ "space-evenly" = Just JSpaceEvenly
parseJustify _ _ = Nothing

parseAlign _ "stretch" = Just AlStretch
parseAlign self "flex-start" | reverseRowsCSS self = Just AlEnd
    | otherwise = Just AlStart
parseAlign _ "start" = Just AlStart
parseAlign self "self-start" | textRTL self = Just AlEnd
    | otherwise = Just AlStart
parseAlign self "flex-end" | reverseRowsCSS self = Just AlStart
    | otherwise = Just AlEnd
parseAlign _ "end" = Just AlEnd
parseAlign self "self-end" | textRTL self = Just AlStart
    | otherwise = Just AlEnd
parseAlign _ "center" = Just AlCenter
parseAlign _ "baseline" = Just AlBaseline
parseAlign _ _ = Nothing

instance PropertyParser CSSFlex where
    temp = CSSFlex {
        directionCSS = Row,
        reverseRowsCSS = False,
        wrapCSS = NoWrap,
        justifyCSS = Nothing, -- flex-start, conditional on directionCSS
        alignItemsCSS = AlStretch,
        alignLinesCSS = Just JStart,
        rowGapCSS = (0,"px"),
        columnGapCSS = (0,"px"),
        orderCSS = 0,
        growCSS = 0,
        shrinkCSS = 1,
        basisCSS = (0,"auto"),
        alignSelfCSS = AlStretch, -- Should be auto, but we're implementing that in `inherit`.
        textRTL = False
      }
    inherit parent = temp { alignSelfCSS = alignItemsCSS parent }
    priority _ = ["flex-direction"]

    longhand _ self "flex-direction" [Ident "row"] = setDir self Row False
    longhand _ self "flex-direction" [Ident "row-reverse"] = setDir self Row True
    longhand _ self "flex-direction" [Ident "column"] = setDir self F.Column False
    longhand _ self "flex-direction" [Ident "column-reverse"] =
        setDir self F.Column True
    longhand _ self "flex-direction" [Ident "initial"] = setDir self Row False

    longhand _ self "flex-wrap" [Ident "no-wrap"] = Just self { wrapCSS = NoWrap }
    longhand _ self "flex-wrap" [Ident "wrap"] = Just self { wrapCSS = Wrap }
    longhand _ self "flex-wrap" [Ident "wrap-reverse"] =
        Just self { wrapCSS = WrapReverse }
    longhand _ self "flex-wrap" [Ident "initial"] = Just self { wrapCSS = NoWrap }

    longhand _ self "justify-content" [Ident x]
        | x == "initial" = Just self { justifyCSS = parseJustify self "flex-start" }
        | y@(Just _) <- parseJustify self x = Just self { justifyCSS = y }

    longhand _ self "align-items" [Ident x]
        | x == "initial" = Just self { alignItemsCSS = AlStretch }
        | Just y <- parseAlign self x = Just self { alignItemsCSS = y }

    longhand _ self "align-content" [Ident x] | x `elem` ["initial", "normal"] =
            Just self { alignLinesCSS = parseJustify self "start" }
        | x == "stretch" = Just self { alignLinesCSS = Nothing }
        | y@(Just _) <- parseJustify self x = Just self { alignLinesCSS = y }

    longhand _ self "row-gap" [Ident x]
        | x `elem` ["initial", "normal"] = Just self { rowGapCSS = (0,"px") }
        | otherwise = Nothing
    longhand _ self "row-gap" toks
        | Just x <- parseLength toks = Just self { rowGapCSS = x}
    longhand _ self "column-gap" [Ident x]
        | x `elem` ["initial", "normal"] = Just self { columnGapCSS = (0,"px") }
        | otherwise = Nothing
    longhand _ self "column-gap" toks
        | Just x <- parseLength toks = Just self { columnGapCSS = x }

    longhand _ self "order" [Number _ (NVInteger x)] = Just self { orderCSS = x }
    longhand _ self "order" [Ident "initial"] = Just self { orderCSS = 0 }

    longhand _ self "flex-grow" [Number _ x] | n2f x>0 = Just self {growCSS=n2f x}
    longhand _ self "flex-grow" [Ident "initial"] = Just self { growCSS = 0 }
    longhand _ self "flex-shrink" [Number _ x] | n2f x > 0 =
        Just self { shrinkCSS = n2f x }
    longhand _ self "flex-shrink" [Ident "initial"] = Just self { shrinkCSS = 1 }

    longhand _ self "flex-basis" toks | Just x <- parseLength toks =
        Just self { basisCSS = x }

    longhand parent self "align-self" [Ident x] | x `elem` ["initial", "auto"] =
            Just self { alignSelfCSS = alignItemsCSS parent }
        | Just y <- parseAlign self x = Just self { alignSelfCSS = y }

    longhand _ _ _ _ = Nothing

    shorthand self "flex-flow" toks =
        parseUnorderedShorthand self ["flex-direction", "flex-wrap"] toks
    shorthand self "gap" toks | [x] <- parseOperands toks =
            parseUnorderedShorthand' self ["row-gap", "column-gap"] [x, x]
        | otherwise = parseUnorderedShorthand self ["row-gap", "column-gap"] toks
    shorthand self "flex" toks
        | [Ident "initial"] <- toks =
            [("flex-grow", init), ("flex-shrink", init), ("flex-basis", px0)]
        | [Ident "auto"] <- toks =
            [("flex-grow", n1), ("flex-shrink", n1), ("flex-basis", init)]
        | [Ident "none"] <- toks =
            [("flex-grow", n0), ("flex-shrink", n0), ("flex-basis", init)]
        | [a] <- operands, test "flex-grow" a =
            [("flex-grow", a), ("flex-shrink", init), ("flex-basis", px0)]
        | [a] <- operands, test "flex-basis" a =
            [("flex-grow", n1), ("flex-shrink", init), ("flex-basis", a)]
        | [a, b] <- operands, test "flex-grow" a, test "flex-shrink" b =
            [("flex-grow", a), ("flex-shrink", b), ("flex-basis", px0)]
        | [a, b] <- operands, test "flex-grow" a, test "flex-basis" b =
            [("flex-grow", a), ("flex-shrink", init), ("flex-basis", b)]
        | [a, b, c] <- operands, test "flex-grow" a, test "flex-shrink" b, test "flex-basis" c =
            [("flex-grow", a), ("flex-shrink", b), ("flex-basis", c)]
      where
        operands = parseOperands toks
        test a = isJust . longhand self self a
        init = [Ident "initial"]
        px0 = [Dimension "0" (NVInteger 0) "px"]
        n1 = [Number "1" (NVInteger 1)]
        n0 = [Number "0" (NVInteger 0)]
    shorthand self k v | Just _ <- longhand self self k v = [(k, v)]
        | otherwise = []

lowerFlex :: CSSFlex -> Font' -> [CSSFlex] -> [a] -> [Font'] -> FlexParent a Length
lowerFlex self font kids kids' fonts' = FlexParent {
    direction = directionCSS self,
    reverseRows = reverseRowsCSS self,
    wrap = wrapCSS self,
    justify = case justifyCSS self of
        Just x -> x
        Nothing | reverseRowsCSS self -> JEnd
        Nothing -> JStart,
    alignLines = alignLinesCSS self,
    baseGap = case directionCSS self of
        Row -> flip finalizeLength font $ rowGapCSS self
        F.Column -> flip finalizeLength font $ columnGapCSS self,
    crossGap = case directionCSS self of
        Row -> flip finalizeLength font $ columnGapCSS self
        F.Column -> flip finalizeLength font $ rowGapCSS self,
    pageWidth = 0,
    children = [[FlexChild {
        grow = growCSS kid,
        shrink = shrinkCSS kid,
        basis = flip finalizeLength font' $ rowGapCSS kid,
        alignment = alignSelfCSS kid,
        flexInner = kid'
      } | (kid, kid', font') <- zip3 kids kids' fonts']]
  }
