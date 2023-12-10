{-# LANGUAGE OverloadedStrings #-}
-- | HarfBuzz is a text shaping library.
-- Using the HarfBuzz library allows programs to convert a sequence of
-- Unicode input into properly formatted and positioned glyph output
-- for practically any writing system and language.
-- See `shape` for the central function all other datatypes serves to support.
module Data.Text.Glyphize (shape, version, versionAtLeast, versionString, HarfbuzzError(..),

    Buffer(..), ContentType(..), ClusterLevel(..), Direction(..), defaultBuffer,
    dirFromStr, dirToStr, dirReverse, dirBackward, dirForward, dirHorizontal, dirVertical,
    scriptHorizontalDir, languageDefault, tag_from_string, tag_to_string, guessSegmentProperties,

    GlyphInfo(..), GlyphPos(..), Feature(..), featTag, Variation(..), varTag,
    parseFeature, unparseFeature, parseVariation, unparseVariation, globalStart, globalEnd,

    countFace, Face, createFace, ftCreateFace, emptyFace, faceTableTags, faceGlyphCount,
    faceCollectUnicodes, faceCollectVarSels, faceCollectVarUnicodes, faceIndex, faceUpem,
    faceBlob, faceTable,

    Font, createFont, ftCreateFont, emptyFont, fontFace, fontGlyph, fontGlyphAdvance,
    fontGlyphContourPoint, fontGlyphContourPointForOrigin, fontGlyphFromName,
    fontGlyphHAdvance, fontGlyphVAdvance, fontGlyphHKerning, fontGlyphHOrigin, fontGlyphVOrigin,
    fontGlyphKerningForDir, fontGlyphName, fontGlyphName_, fontGlyphOriginForDir,
    fontNominalGlyph, fontPPEm, fontPtEm, fontScale, fontVarGlyph, fontSyntheticSlant,
    fontVarCoordsNormalized, fontTxt2Glyph, fontGlyph2Str, fontVarCoordsDesign,

    GlyphExtents(..), fontGlyphExtents, fontGlyphExtentsForOrigin,
    FontExtents(..), fontExtentsForDir, fontHExtents, fontVExtents,
    FontOptions(..), defaultFontOptions, createFontWithOptions, ftCreateFontWithOptions, 
    ) where

import Data.Text.Glyphize.Font
import Data.Text.Glyphize.Buffer
import Data.Text.Glyphize.Oom
import Data.Text.Glyphize.Array (noCache)

import System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)
import Foreign.Ptr (Ptr(..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)

import Foreign.C.String (CString(..), peekCString)
import Foreign.Marshal.Array (withArrayLen)

-- | Shapes the text in the given `Buffer` according to the given `Font`
-- yielding glyphs and their positions.
-- If any `Feature`s are given they will be applied during shaping.
-- If two `Feature`s have the same tag but overlapping ranges
-- the value of the `Feature` with the higher index takes precedance.
shape :: Font -> Buffer -> [Feature] -> [(GlyphInfo, GlyphPos)]
shape _ Buffer {text = ""} _ = []
shape font buffer features = unsafePerformIO $ withBuffer buffer $ \buffer' -> do
    withForeignPtr font $ \font' -> withArrayLen features $ \len features' ->
        hb_shape font' buffer' features' $ toEnum len
    infos <- glyphInfos buffer'
    pos <- glyphsPos buffer'
    return $ noCache zip infos pos
foreign import ccall "hb_shape" hb_shape :: Font_ -> Buffer' -> Ptr Feature -> Word -> IO ()

-- | Fills in unset segment properties based on buffer unicode contents.
-- If buffer is not empty it must have `ContentType` `ContentTypeUnicode`.
-- If buffer script is not set it will be set to the Unicode script of the first
-- character in the buffer that has a script other than "common", "inherited",
-- or "unknown".
-- Next if the buffer direction is not set it will be set to the natural
-- horizontal direction of the buffer script as returned by `scriptHorizontalDir`.
-- If `scriptHorizontalDir` returns `Nothing`, then `DirLTR` is used.
-- Finally if buffer language is not set, it will be set to the process's default
-- language as returned by `languageDefault`. This may change in the future by
-- taking buffer script into consideration when choosting a language.
-- Note that `languageDefault` is not thread-safe the first time it is called.
-- See documentation for that function for details.
guessSegmentProperties :: Buffer -> Buffer
guessSegmentProperties = unsafePerformIO . flip withBuffer thawBuffer

foreign import ccall "hb_version" hb_version :: Ptr Int -> Ptr Int -> Ptr Int -> IO ()
-- | Returns the library version as 3 integer components.
version :: (Int, Int, Int)
version = unsafeDupablePerformIO $
    alloca $ \a' -> alloca $ \b' -> alloca $ \c' -> do
        hb_version a' b' c'
        a <- peek a'
        b <- peek b'
        c <- peek c'
        return (a, b, c)
-- | Tests the library version against a minimum value, as 3 integer components.
foreign import ccall "hb_version_atleast" versionAtLeast :: Int -> Int -> Int -> Bool
foreign import ccall "hb_version_string" hb_version_string :: CString
-- | Returns library version as a string with 3 integer components.
versionString :: String
versionString = unsafeDupablePerformIO $ peekCString hb_version_string
