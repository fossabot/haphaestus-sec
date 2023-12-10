{-# LANGUAGE OverloadedStrings #-}
-- | Infrastructure for parsing & desugaring CSS properties related to fonts.
module Graphics.Layout.CSS.Font(Font'(..), placeholderFont, hbUnit,
        pattern2hbfont, pattern2font, CSSFont(..), variations') where

import Data.CSS.Syntax.Tokens (Token(..), NumericValue(..), serialize)
import Stylist (PropertyParser(..))
import qualified Data.Text as Txt
import Data.Maybe (fromMaybe)

import Graphics.Layout.Box
import Graphics.Layout.CSS.Length

import Data.Text.Glyphize as HB
import Graphics.Text.Font.Choose (Pattern(..), Value(..), normalizePattern,
                                  getValue', getValue0, setValue, Binding(..),
                                  configSubstitute', defaultSubstitute,
                                  fontSort', MatchKind(..), fontRenderPrepare')
import qualified Data.ByteString as B
import System.IO.Unsafe (unsafePerformIO)

-- | zero'd `Font'` to serve as the root's parent in a font heirarchy.
placeholderFont = Font' undefined [] (const 0) (const 0) 0 0 0 0  0 0 0 0  1
-- | Scale-factor for text-shaping APIs.
hbUnit = 64 :: Double

-- | Convert from FontConfig query result to a Harfbuzz font.
pattern2hbfont :: Pattern -> Int -> [Variation] -> Font
pattern2hbfont pat scale variations = createFontWithOptions options face
  where
    bytes = unsafePerformIO $ B.readFile $ getValue0 "file" pat
    face = createFace bytes $ toEnum $ fromMaybe 0 $ getValue' "index" pat
    options = foldl value2opt defaultFontOptions { optionScale = Just (scale, scale) } $
                normalizePattern pat

    value2opt opts ("slant", (_, ValueInt x):_) = opts {
        optionSynthSlant = Just $ realToFrac x
      }
    value2opt opts ("fontvariations", _:_) = opts {optionVariations = variations}
    value2opt opts _ = opts

-- | Convert Parsed CSS to a `Font'`.
-- Includes sizing parameters derived from a root & parent `Font'`.
pattern2font :: Pattern -> CSSFont -> Font' -> Font' -> Font'
pattern2font pat styles@CSSFont { cssFontSize = (x,"initial") } parent root =
    pattern2font pat styles { cssFontSize = (x*fontSize root," ") } parent root
pattern2font pat styles parent root = Font' {
        hbFont = font',
        pattern = font,
        fontHeight = height' . fontGlyphExtents font' . fontGlyph',
        fontAdvance = fromIntegral . fontGlyphHAdvance font' . fontGlyph',
        fontSize = fontSize',
        rootEm = fontSize root,
        lineheight = lineheight',
        rlh = lineheight root,

        vh = vh root,
        vw = vw root,
        vmax = vmax root,
        vmin = vmin root,
        scale = scale root
    } where
        height' (Just x) = fromIntegral $ HB.height x
        height' Nothing = fontSize'
        lineheight' | snd (cssLineheight styles) == "normal",
            Just extents <- fontHExtents font' = (fromIntegral $ lineGap extents)/hbUnit
            | otherwise = lowerLength' (cssLineheight styles) parent
        fontSize' = lowerLength' (cssFontSize styles) parent
        lowerLength' a = lowerLength (fontSize parent) . finalizeLength a
        fontGlyph' ch = fromMaybe 0 $ fontGlyph font' ch Nothing
        q | Nothing <- lookup "family" pat, Just val <- lookup "family" $ pattern root =
                ("family", val):setValue "size" Weak (px2pt root fontSize') pat
            | otherwise = setValue "size" Weak (px2pt root fontSize') pat
        font = case fontSort' (defaultSubstitute $ configSubstitute' q MatchPattern) False of
            Just (font:_, _) -> fontRenderPrepare' q font
            _ -> error "TODO: Set fallback font!"
        font' = pattern2hbfont font (round scale') $ variations' fontSize' styles
        scale' = fontSize' * hbUnit

-- | Parsed CSS font properties, excluding the FontConfig query.
data CSSFont = CSSFont {
    -- | Parsed CSS font-size.
    cssFontSize :: Unitted,
    -- | Parsed CSS line-height.
    cssLineheight :: Unitted,
    -- | Parsed CSS font-variation-settings.
    variations :: [Variation],
    -- | Parsed CSS font-weight.
    weightVariation :: Variation,
    -- | Parsed CSS font-stretch.
    widthVariation :: Variation,
    -- | Parsed CSS font-style.
    slantVariation :: Variation,
    -- | Parsed CSS font-optical-sizing.
    opticalSize :: Bool
}
-- | All font-variations from the parsed CSS properties.
-- | Requires the resolved font-size in case font-optical-sizing is set.
variations' :: Double -> CSSFont -> [Variation]
variations' fontsize self =
    (if opticalSize self then (Variation opsz (realToFrac fontsize):) else id)
    (slantVariation self:widthVariation self:weightVariation self:variations self)

-- | Represents a multiple of the initial font-size.
-- Resolved by `pattern2font`.
fracDefault :: CSSFont -> Double -> Maybe CSSFont
fracDefault self frac = Just self {
    cssFontSize = (frac,"initial")
}
instance PropertyParser CSSFont where
    temp = CSSFont {
        cssFontSize = (12,"pt"),
        cssLineheight = (1,""),
        variations = [],
        weightVariation = Variation wght 400,
        widthVariation = Variation wdth 100,
        slantVariation = Variation ital 0,
        opticalSize = True
    }
    inherit parent = parent
    priority _ = []

    longhand _ self "font-size" [Ident "xx-small"] = fracDefault self $ 3/5
    longhand _ self "font-size" [Ident "x-small"] = fracDefault self $ 3/4
    longhand _ self "font-size" [Ident "small"] = fracDefault self $ 8/9
    longhand _ self "font-size" [Ident "medium"] = fracDefault self 1
    longhand _ self "font-size" [Ident "initial"] = fracDefault self 1
    longhand _ self "font-size" [Ident "large"] = fracDefault self $ 6/5
    longhand _ self "font-size" [Ident "x-large"] = fracDefault self $ 3/2
    longhand _ self "font-size" [Ident "xx-large"] = fracDefault self 2
    longhand _ self "font-size" [Ident "xxx-large"] = fracDefault self 3
    longhand parent self "font-size" [Ident "larger"] =
        Just self { cssFontSize = (x*1.2,unit) }
      where (x,unit) = cssFontSize parent
    longhand parent self "font-size" [Ident "smaller"] =
        Just self { cssFontSize = (x/1.2,unit) }
      where (x, unit) = cssFontSize parent
    longhand _ self "font-size" toks
        | Just x <- parseLength toks = Just self { cssFontSize = x }

    longhand _ self "line-height" [Ident "normal"] = Just self { cssLineheight = (0,"normal") }
    longhand _ self "line-height" [Number _ x] = Just self { cssLineheight = (n2f x,"em") }
    longhand _ self "line-height" toks
        | Just x <- parseLength toks = Just self { cssLineheight = x }

    longhand _ self "font-variation-settings" [Ident "normal"] = Just self { variations = [] }
    longhand _ self "font-variation-settings" [Ident "initial"] = Just self {variations = []}
    longhand _ self "font-variation-settings" toks
        | Just x <- parseVariations toks = Just self { variations = x }

    longhand _ self "font-weight" [Ident "normal"] =
        Just self { weightVariation = Variation wght 400 }
    longhand _ self "font-weight" [Ident "initial"] =
        Just self { weightVariation = Variation wght 400 }
    longhand _ self "font-weight" [Ident "bold"] =
        Just self { weightVariation = Variation wght 700 }
    longhand _ self "font-weight" [Number _ (NVInteger x)] | x >= 100 && x < 1000 =
        Just self { weightVariation = Variation wght $ fromIntegral x }
    longhand parent self "font-weight" [Ident "bolder"]
        | varValue (weightVariation parent) < 400 =
            Just self { weightVariation = Variation wght 400 }
        | varValue (weightVariation parent) < 600 =
            Just self { weightVariation = Variation wght 700 }
        | otherwise = Just self { weightVariation = Variation wght 900 }
    longhand parent self "font-weight" [Ident "lighter"]
        | varValue (weightVariation parent) < 600 =
            Just self { weightVariation = Variation wght 100 }
        | varValue (weightVariation parent) < 800 =
            Just self { weightVariation = Variation wght 400 }
        | otherwise = Just self { weightVariation = Variation wght 700 }

    longhand _ self "font-stretch" [Ident "ultra-condensed"] =
        Just self { widthVariation = Variation wdth 50 }
    longhand _ self "font-stretch" [Ident "extra-condensed"] =
        Just self { widthVariation = Variation wdth 62.5 }
    longhand _ self "font-stretch" [Ident "condensed"] =
        Just self { widthVariation = Variation wdth 75 }
    longhand _ self "font-stretch" [Ident "semi-condensed"] =
        Just self { widthVariation = Variation wdth 87.5 }
    longhand _ self "font-stretch" [Ident k] | k `elem` ["initial", "normal"] =
        Just self { widthVariation = Variation wdth 100 }
    longhand _ self "font-stretch" [Ident "semi-expanded"] =
        Just self { widthVariation = Variation wdth 112.5 }
    longhand _ self "font-stretch" [Ident "expanded"] =
        Just self { widthVariation = Variation wdth 125 }
    longhand _ self "font-stretch" [Ident "extra-expanded"] =
        Just self { widthVariation = Variation wdth 150 }
    longhand _ self "font-stretch" [Ident "ultra-expanded"] =
        Just self { widthVariation = Variation wdth 200 }
    longhand _ self "font-stretch" [Percentage _ x] =
        Just self { widthVariation = Variation wdth $ n2f x }

    longhand _ self "font-style" [Ident "oblique", Dimension _ x "deg"] =
        Just self { slantVariation = Variation slnt $ n2f x }
    longhand _ self "font-style" [Ident "oblique", Dimension _ x "grad"] =
        Just self { slantVariation = Variation slnt (n2f x/400*360) }
    longhand _ self "font-style" [Ident "oblique", Dimension _ x "rad"] =
        Just self { slantVariation = Variation slnt (n2f x*180/pi) }
    longhand _ self "font-style" [Ident "oblique", Dimension _ x "turn"] =
        Just self { slantVariation = Variation slnt (n2f x*360) }
    longhand _ self "font-style" [Ident "italic"] =
        Just self { slantVariation = Variation ital 1 }
    longhand _ self "font-style" [Ident "normal"] =
        Just self { slantVariation = Variation ital 0 }
    longhand _ self "font-style" [Ident "initial"] =
        Just self { slantVariation = Variation ital 0 }

    longhand _ s "font-optical-sizing" [Ident "auto"] = Just s {opticalSize = True}
    longhand _ s "font-optical-sizing" [Ident "initial"] = Just s {opticalSize = True}
    longhand _ s "font-optical-sizing" [Ident "none"] = Just s {opticalSize = False}

    longhand _ _ _ _ = Nothing

-- | Utility for parsing multiple font variations (via Harfbuzz).
parseVariations (x@(String _):y@(Number _ _):Comma:toks)
    | Just var <- parseVariation $ Txt.unpack $ serialize [x, y],
        Just vars <- parseVariations toks = Just $ var:vars
parseVariations toks@[String _, Number _ _]
    | Just var <- parseVariation $ Txt.unpack $ serialize toks = Just [var]
parseVariations _ = Nothing

wght = tag_from_string "wght"
wdth = tag_from_string "wdth"
slnt = tag_from_string "slnt"
ital = tag_from_string "ital"
opsz = tag_from_string "opsz"
