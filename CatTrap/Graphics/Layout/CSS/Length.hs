{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
-- | Infrastructure for parsing & desugaring length units & keywords,
-- in reference to the selected font.
module Graphics.Layout.CSS.Length(Unitted, auto, parseLength, parseLength', units,
        n2f, finalizeLength, finalizeLengths, px2pt, Font'(..)) where

import Data.CSS.Syntax.Tokens (Token(..), NumericValue(..))
import qualified Data.Text as Txt
import Data.Scientific (toRealFloat, fromFloatDigits)
import Debug.Trace (trace) -- For warnings.
import Data.Text.Glyphize (Font)
import Graphics.Text.Font.Choose (Pattern(..))

import Graphics.Layout.Box

-- | A number+unit, prior to resolving side units.
-- The unit may alternately represent a keyword, in which case the number is
-- ignored & typically set to 0.
type Unitted = (Double, Txt.Text)
instance Zero Unitted where zero = (0,"px")
-- | The CSS `auto` keyword.
auto :: Unitted
auto = (0,"auto")

-- | Parse a pre-tokenized CSS length value.
parseLength :: [Token] -> Maybe Unitted
parseLength [Percentage _ x] = Just (n2f x,"%")
parseLength [Dimension _ x unit]
    | n2f x == 0 && unit == "" = Just (0,"px")
    | unit `elem` units = Just (n2f x,unit)
parseLength [Number _ x] | n2f x == 0 = Just (0,"px")
parseLength [Ident "auto"] = Just (0,"auto")
parseLength [Ident "initial"] = Just (0,"auto")
parseLength _ = Nothing
-- | Variant of `parseLength` which supports min-content & max-content keywords.
parseLength' [Ident "min-content"] = Just (0,"min-content")
parseLength' [Ident "max-content"] = Just (0,"max-content")
parseLength' toks = parseLength toks

-- | Supported length units.
units = Txt.words "cap ch em ex ic lh rem rlh vh vw vmax vmin px cm mm Q in pc pt %"

-- | Convert a lexed number to a Double.
n2f :: (Fractional x, RealFloat x) => NumericValue -> x
n2f (NVInteger x) = realToFrac x
n2f (NVNumber x) = toRealFloat x

-- | Resolve a parsed length according to the sizing parameters in a given `Font'`.
finalizeLength :: Unitted -> Font' -> Length
finalizeLength (x,"cap") f = Pixels $ x*fontHeight f 'A'
finalizeLength (x,"ch") f = Pixels $ x*fontAdvance f '0'
finalizeLength (x,"em") f = Pixels $ x*fontSize f
finalizeLength (x,"") f = Pixels $ x*fontSize f -- For line-height.
finalizeLength (x,"ex") f = Pixels $ x*fontHeight f 'x'
finalizeLength (x,"ic") f = Pixels $ x*fontHeight f '水' -- CJK water ideograph
finalizeLength (x,"lh") f = Pixels $ x*lineheight f
finalizeLength (x,"rem") f = Pixels $ x*rootEm f
finalizeLength (x,"rlh") f = Pixels $ x*rlh f
finalizeLength (x,"vh") f = Pixels $ x*vh f
finalizeLength (x,"vb") f = Pixels $ x*vh f -- TODO: Support vertical text
finalizeLength (x,"vw") f = Pixels $ x*vw f
finalizeLength (x,"vi") f = Pixels $ x*vw f -- TODO: Support vertical text
finalizeLength (x,"vmax") f = Pixels $ x*vmax f
finalizeLength (x,"vmin") f = Pixels $ x*vmin f
finalizeLength (x,"px") f = Pixels $ x*scale f
finalizeLength (x,"cm") f = Pixels $ x*scale f*96/2.54
finalizeLength (x,"in") f = Pixels $ x*96*scale f
finalizeLength (x,"mm") f | Pixels x' <- finalizeLength (x,"cm") f = Pixels $ x'/10
finalizeLength (x,"Q") f | Pixels x' <- finalizeLength (x,"cm") f = Pixels $ x'/40
finalizeLength (x,"pc") f | Pixels x' <- finalizeLength (x,"in") f = Pixels $ x'/6
finalizeLength (x,"pt") f | Pixels x' <- finalizeLength (x,"in") f = Pixels $ x'/72
finalizeLength (x,"%") _ = Percent $ x/100
finalizeLength (_,"auto") _ = Auto
finalizeLength (_,"min-content") _ = Min
finalizeLength (_,"max-content") _ = Preferred
finalizeLength (x, " ") _ = Pixels x -- Internal constant value...
finalizeLength (_,unit) _ = trace ("Invalid unit " ++ Txt.unpack unit) $ Pixels 0
-- | Convert from a computed length to the "pt" unit.
px2pt f x = x / scale f / 96 * 72

-- | Convert any length-units in the given CSS tokens to device pixels
finalizeLengths :: Font' -> [Token] -> [Token]
finalizeLengths f (Dimension _ x unit:toks)
    | unit `elem` units, Pixels y <- finalizeLength (n2f x,unit) f =
        Dimension "" (NVNumber $ fromFloatDigits y) "px":finalizeLengths f toks
finalizeLengths f (Number a b:ts)|n2f b==0=Dimension a b "px":finalizeLengths f ts
finalizeLengths f (tok:toks) = tok:finalizeLengths f toks
finalizeLengths _ [] = []

-- | A Harfbuzz font with sizing parameters.
data Font' = Font' {
    -- | The Harfbuzz font used to shape text & query character-size information.
    hbFont :: Font,
    -- | The FontConfig query result. Useful to incorporate into output rendering.
    pattern :: Pattern,
    -- | Query the height of a character.
    -- Used for cap, ex, or ic units.
    fontHeight :: Char -> Double,
    -- | Query the width of a character, used for ch unit.
    fontAdvance :: Char -> Double,
    -- | The desired font-size, used for em unit.
    fontSize :: Double,
    -- | The root font's size, used for rem unit.
    rootEm :: Double,
    -- | The desired line-height, used for lh unit.
    lineheight :: Double,
    -- | The root font's line-height, used for rlh unit.
    rlh :: Double,
    -- | Scale-factor for vh unit.
    vh :: Double,
    -- | Scale-factor for vw unit.
    vw :: Double,
    -- | Scale-factor for vmax unit.
    vmax :: Double,
    -- | Scale-factor for vmin unit.
    vmin :: Double,
    -- | How many device pixels in a CSS px?
    scale :: Double
}

instance Eq Font' where
    a == b = pattern a == pattern b
instance Show Font' where
    show a = show $ pattern a
