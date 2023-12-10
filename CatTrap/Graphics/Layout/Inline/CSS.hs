{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
-- | Infrastructure for parsing & desugaring text related CSS properties.
module Graphics.Layout.Inline.CSS(
    CSSInline(..), Default(..), UnicodeBidi(..), applyFontInline, applyBidi,
    resolveVAlign, resolveBoxOpts, plaintext) where

import Data.CSS.Syntax.Tokens (Token(..))
import Stylist (PropertyParser(..))
import qualified Data.Text as Txt
import Data.Text (Text)
import Data.Text.ParagraphLayout.Rich
import Data.Text.Glyphize (Direction(..))

import Graphics.Layout.CSS.Font (Font'(..), hbUnit)
import Graphics.Layout.CSS.Length (finalizeLength, Unitted)
import Graphics.Layout.Box (Length(..))
import Graphics.Layout.Grid.Table (TableOptions(..)) -- for VAlign
import Data.Char (isSpace)
import Data.Int (Int32)
import Debug.Trace (trace) -- To report unexpected cases.

-- | Document text with Balkón styling options, CSS stylable.
data CSSInline = CSSInline Txt.Text TextOptions UnicodeBidi
-- | To what degree is the text direction isolated?
data UnicodeBidi = BdNormal | BdEmbed | BdOverride | BdIsolate
        | BdIsolateOverride | BdPlainText deriving (Eq, Ord, Enum, Read, Show)

-- | Construct plain text
plaintext :: Txt.Text -> CSSInline
plaintext txt = CSSInline txt (defaultTextOptions DirLTR) BdNormal

instance PropertyParser CSSInline where
    temp = CSSInline "" (defaultTextOptions DirLTR) BdNormal
    inherit (CSSInline _ opts _) = CSSInline "" opts BdNormal
    priority _ = ["direction"] -- To inform logical spacing in caller!

    longhand _ (CSSInline _ opts bidi) "content" [Ident "initial"] =
        Just $ CSSInline "" opts bidi
    longhand _ (CSSInline _ opts bidi) "content" toks
        | all isString toks =
            Just $ CSSInline (Txt.concat [x | String x <- toks]) opts bidi
      where
        isString (String _) = True
        isString _ = False

    longhand _ (CSSInline t o b) "-argo-lang" [Ident kw]
        | kw `elem` ["initial", "auto"] = Just $ CSSInline t o {textLanguage=""} b
    longhand _ (CSSInline txt opts bidi) "-argo-lang" [String x] =
        Just $ CSSInline txt opts { textLanguage = Txt.unpack x } bidi

    longhand _ (CSSInline txt opts bidi) "direction" [Ident "ltr"] =
        Just $ CSSInline txt opts { textDirection = DirLTR } bidi
    longhand _ (CSSInline txt opts bidi) "direction" [Ident "rtl"] =
        Just $ CSSInline txt opts { textDirection = DirRTL } bidi
    longhand _ (CSSInline txt opts bidi) "direction" [Ident "initial"] =
        Just $ CSSInline txt opts { textDirection = DirLTR } bidi

    longhand _ (CSSInline txt opts _) "unicode-bidi" [Ident "initial"] =
        Just $ CSSInline txt opts BdNormal
    longhand _ (CSSInline txt opts _) "unicode-bidi" [Ident "normal"] =
        Just $ CSSInline txt opts BdNormal
    longhand _ (CSSInline txt opts _) "unicode-bidi" [Ident "embed"] =
        Just $ CSSInline txt opts BdEmbed
    longhand _ (CSSInline txt opts _) "unicode-bidi" [Ident "isolate"] =
        Just $ CSSInline txt opts BdIsolate
    longhand _ (CSSInline txt opts _) "unicode-bidi" [Ident "bidi-override"] =
        Just $ CSSInline txt opts BdOverride
    longhand _ (CSSInline txt opts _) "unicode-bidi" [Ident "isolate-override"] =
        Just $ CSSInline txt opts BdIsolateOverride
    longhand _ (CSSInline txt opts _) "unicode-bidi" [Ident "plaintext"] =
        Just $ CSSInline txt opts BdPlainText
    longhand _ _ _ _ = Nothing

-- | Fills in properties from looked-up fonts.
applyFontInline :: TextOptions -> Font' -> TextOptions
applyFontInline opts font = opts {
    textFont = hbFont font,
    textLineHeight = Absolute $ toHB $ lineheight font
  }
-- | Apply Bidi chars around the inline text. FIXME: Handle the tree!
applyBidi :: Default d => CSSInline -> [InnerNode Text d] -> [InnerNode Text d]
applyBidi (CSSInline _ _ BdNormal) txt = txt
applyBidi (CSSInline _ (textDirection -> DirLTR) BdEmbed) txt =
    chLREmbed:txt+:chPopDir
applyBidi (CSSInline _ (textDirection -> DirRTL) BdEmbed) txt =
    chRLEmbed:txt+:chPopDir
applyBidi (CSSInline _ (textDirection -> DirLTR) BdIsolate) txt =
    chLRIsolate:txt+:chPopDirIsolate
applyBidi (CSSInline _ (textDirection -> DirRTL) BdIsolate) txt =
    chRLIsolate:txt+:chPopDirIsolate
applyBidi (CSSInline _ (textDirection -> DirLTR) BdOverride) txt =
    chLROverride:txt+:chPopDir
applyBidi (CSSInline _ (textDirection -> DirRTL) BdOverride) txt =
    chRLOverride:txt+:chPopDir
applyBidi (CSSInline _ (textDirection -> DirLTR) BdIsolateOverride) txt =
    ch1stStrongIsolate:chLROverride:txt+:chPopDir+:chPopDirIsolate
applyBidi (CSSInline _ (textDirection -> DirRTL) BdIsolateOverride) txt =
    ch1stStrongIsolate:chRLOverride:txt+:chPopDir+:chPopDirIsolate
applyBidi (CSSInline _ _ BdPlainText) txt =
    ch1stStrongIsolate:txt+:chPopDirIsolate
applyBidi (CSSInline _ (textDirection -> dir) _) txt =
    trace ("Unexpected direction! " ++ show dir) txt

-- | Append a single character to the end of a string.
a +: b = a ++ [b]

chLREmbed, chRLEmbed, chLROverride, chRLOverride, chPopDir,
    chLRIsolate, chRLIsolate, ch1stStrongIsolate, chPopDirIsolate :: Default a =>
        InnerNode Text a
chLREmbed = leaf '\x202A'
chRLEmbed = leaf '\x202B'
chLROverride = leaf '\x202D'
chRLOverride = leaf '\x202E'
chPopDir = leaf '\x202C'
chLRIsolate = leaf '\x2066'
chRLIsolate = leaf '\x2067'
ch1stStrongIsolate = leaf '\x2068'
chPopDirIsolate = leaf '\x2069'

-- | A Balkón fragment holding a magic character.
leaf ch = TextSequence def $ Txt.singleton ch

-- | Types with default values.
-- Used to fill in values into generated fragments from caller.
class Default a where
    def :: a

-- | Converts parsed valign keywords or length units to Balkón alignment.
resolveVAlign :: Font' -> Unitted -> VerticalAlignment
resolveVAlign _ (_,"top") = AlignLineTop
resolveVAlign _ (_,"super") = AlignLineTop -- FIXME: Is there a better translation?
resolveVAlign _ (_,"text-top") = AlignLineTop -- FIXME: Better translation?
resolveVAlign _ (_,"bottom") = AlignLineBottom
resolveVAlign _ (_,"sub") = AlignLineBottom -- FIXME: Better translation?
resolveVAlign _ (_,"text-bottom") = AlignLineBottom
resolveVAlign _ (_,"baseline") = AlignBaseline 0
resolveVAlign f (_,"middle") = AlignBaseline $ toHB $ fontHeight f 'x' / 2
resolveVAlign f x | Pixels y <- finalizeLength x f = AlignBaseline $ toHB y
    | Percent y <- finalizeLength x f = AlignBaseline $ toHB $ y * lineheight f
    | otherwise = trace ("Invalid length! " ++ show x) $ AlignBaseline 0
-- | Converts grid options to box options.
resolveBoxOpts f grid = defaultBoxOptions {
    boxVerticalAlignment = resolveVAlign f $ verticalAlign grid
  }

-- | Convert from CatTrap units to Balkón|Harfbuzz units.
toHB :: Double -> Int32
toHB = toEnum . fromEnum . (*) hbUnit
