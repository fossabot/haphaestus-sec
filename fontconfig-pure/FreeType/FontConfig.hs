-- NOTE: Not tested
module FreeType.FontConfig (ftCharIndex, ftCharSet, ftCharSetAndSpacing,
    ftQuery, ftQueryAll, ftQueryFace,
    FTFC_Instance(..), FTFC_Metrics(..), FTFC_Subpixel(..), instantiatePattern,
    FTFC_Glyph(..), glyphForIndex, bmpAndMetricsForIndex) where

import Graphics.Text.Font.Choose.CharSet (CharSet, CharSet_, thawCharSet, thawCharSet_)
import Graphics.Text.Font.Choose.Pattern (Pattern, Pattern_, thawPattern, thawPattern_)
import Graphics.Text.Font.Choose.FontSet (FontSet, FontSet_, withFontSet, thawFontSet)
import FreeType.Core.Base (FT_Face(..))
import Data.Word (Word32, Word)

import Foreign.Ptr (nullPtr, Ptr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.C.String (CString, withCString)
import System.IO.Unsafe (unsafePerformIO)

import Control.Exception (throw, catch)
import Graphics.Text.Font.Choose.Result (Error(ErrTypeMismatch))

-- For FcFt transliteration
import Graphics.Text.Font.Choose.Value (Value(..))
import Graphics.Text.Font.Choose.Pattern (getValue', getValue0, getValue, getValues')

import Data.Maybe (fromMaybe)
import Linear.V2 (V2(..))
import Linear.Matrix(M22)
import Data.Bits ((.|.))

import FreeType.Core.Base
import FreeType.Support.Outline (ft_Outline_Embolden)
import FreeType.Control.Subpixel (FT_LcdFilter, ft_Library_SetLcdFilter)
import FreeType.Core.Types
import FreeType.Exception (FtError(..))

c2w :: Char -> Word32
c2w = fromIntegral . fromEnum

-- | Maps a Unicode char to a glyph index.
-- This function uses information from several possible underlying encoding
-- tables to work around broken fonts. As a result, this function isn't designed
-- to be used in performance sensitive areas; results from this function are
-- intended to be cached by higher level functions.
ftCharIndex :: FT_Face -> Char -> Word
ftCharIndex face = fcFreeTypeCharIndex face . c2w
foreign import ccall "FcFreeTypeCharIndex" fcFreeTypeCharIndex :: FT_Face -> Word32 -> Word

-- | Scans a FreeType face and returns the set of encoded Unicode chars.
ftCharSet :: FT_Face -> CharSet
ftCharSet face = unsafePerformIO $ thawCharSet_ $ fcFreeTypeCharSet face nullPtr
foreign import ccall "FcFreeTypeCharSet" fcFreeTypeCharSet
    :: FT_Face -> Ptr () -> IO CharSet_ -- 2nd arg's deprecated!

-- | How consistant are the widths of the chars in a font.
data Spacing = Proportional -- ^ Where the font has glyphs of many widths.
    | Dual -- ^ Where the font has glyphs in precisely two widths.
    | Mono -- ^ Where all glyphs have the same width.
-- | Scans a FreeType face and returns the set of encoded Unicode chars.
-- `snd` receives the computed spacing type of the font.
ftCharSetAndSpacing :: FT_Face -> (CharSet, Spacing)
ftCharSetAndSpacing face = unsafePerformIO $ alloca $ \spacing' -> do
    chars <- thawCharSet_ $ fcFreeTypeCharSetAndSpacing face nullPtr spacing'
    spacing_ <- peek spacing'
    let spacing = case spacing_ of{
        0 -> Proportional;
        90 -> Dual;
        100 -> Mono;
        _ -> throw ErrTypeMismatch}
    return (chars, spacing)
foreign import ccall "FcFreeTypeCharSetAndSpacing" fcFreeTypeCharSetAndSpacing ::
    FT_Face -> Ptr () -> Ptr Int -> IO CharSet_ -- 2nd arg's deprecated!

-- | Constructs a pattern representing the 'id'th face in 'fst'.
-- The number of faces in 'file' is returned in 'snd'.
ftQuery :: FilePath -> Int -> IO (Pattern, Int)
ftQuery filename id = withCString filename $ \filename' -> alloca $ \count' -> do
    pattern <- thawPattern_ $ fcFreeTypeQuery filename' id nullPtr count'
    count <- peek count'
    return (pattern, count)
foreign import ccall "FcFreeTypeQuery" fcFreeTypeQuery ::
    CString -> Int -> Ptr () -> Ptr Int -> IO Pattern_ -- 3rd arg's deprecated!

-- | Constructs patterns found in 'filename'.
-- If id is -1, then all patterns found in 'filename' are added to 'fst'.
-- Otherwise, this function works exactly like `ftQuery`.
-- The number of faces in 'filename' is returned in 'snd'.
ftQueryAll :: FilePath -> Int -> IO (FontSet, Int)
ftQueryAll filename id = withCString filename $ \filename' -> alloca $ \count' ->
    withFontSet [] $ \fonts' -> do
        fcFreeTypeQueryAll filename' id nullPtr count' fonts'
        fonts <- thawFontSet fonts'
        count <- peek count'
        return (fonts, count)
foreign import ccall "FcFreeTypeQueryAll" fcFreeTypeQueryAll ::
    CString -> Int -> Ptr () -> Ptr Int -> FontSet_ -> IO Word -- 2nd arg's deprecated!

-- | Constructs a pattern representing 'face'.
-- 'filename' and 'id' are used solely as data for pattern elements.
ftQueryFace :: FT_Face -> FilePath -> Int -> IO Pattern
ftQueryFace face filename id = withCString filename $ \filename' ->
    thawPattern_ $ fcFreeTypeQueryFace face filename' id nullPtr
foreign import ccall "FcFreeTypeQueryFace" fcFreeTypeQueryFace ::
    FT_Face -> CString -> Int -> Ptr () -> IO Pattern_ -- Final arg's deprecated!

------
--- Transliterated from FcFt
--- https://codeberg.org/dnkl/fcft/
--- Untested
------

-- | A `FT_Face` queried from FontConfig with glyph-loading parameters.
data FTFC_Instance = Instance {
    fontName :: Maybe String,
    fontPath :: Maybe String,
    fontFace :: FT_Face,
    fontLoadFlags :: Int,
    fontAntialias :: Bool,
    fontEmbolden :: Bool,
    fontIsColor :: Bool,
    fontRenderFlags :: Int,
    fontRenderFlagsSubpixel :: Int,
    fontPixelSizeFixup :: Double,
    fontPixelFixupEstimated :: Bool,
    fontBGR :: Bool,
    fontLCDFilter :: FT_LcdFilter,
    fontFeats :: [String], -- Callers probably want to validate via harfbuzz
    fontMetrics :: FTFC_Metrics
}
-- | Results queried from FontConfig with caller-relevant properties,
-- notably relating to layout.
data FTFC_Metrics = Metrics {
    height :: Int,
    descent :: Int,
    ascent :: Int,
    maxAdvance :: (Int, Int), -- Width/height of font's widest glyph.
    metricsAntialias :: Bool,
    metricsSubpixel :: FTFC_Subpixel,
    metricsName :: Maybe String
}
-- | Defines subpixel order to use.
-- Note that this is *ignored* if antialiasing has been disabled.
data FTFC_Subpixel = SubpixelNone -- ^ From FontConfig.
    | SubpixelHorizontalRGB | SubpixelHorizontalBGR |
    SubpixelVerticalRGB | SubpixelVerticalBGR
    | SubpixelDefault -- ^ Disable subpixel antialiasing.

-- | Converts the results of a FontConfig query requesting a specific size
-- into a `FT_Face` & related properties.
-- Throw exceptions.
instantiatePattern :: FT_Library -> Pattern -> (Double, Double) -> IO FTFC_Instance
instantiatePattern ftlib pattern (req_pt_size, req_px_size) = do
    let dpi = fromMaybe 75 $ getValue' "dpi" pattern :: Double

    ft_face <- case getValue "ftface" pattern of
        ValueFTFace x -> return x
        _ -> ft_New_Face ftlib (getValue0 "file" pattern) -- is a mutex needed?
            (toEnum $ fromMaybe 0 $ getValue' "index" pattern)

    ft_Set_Pixel_Sizes ft_face 0 $ toEnum $ fromEnum $
        fromMaybe req_px_size $ getValue' "pixelsize" pattern
    let scalable = fromMaybe True $ getValue' "scalable" pattern
    let outline = fromMaybe True $ getValue' "outline" pattern
    (pixel_fixup, fixup_estimated) <- case getValue "pixelsizefixupfactor" pattern of
        ValueDouble x -> return (x, False)
        _ | scalable && not outline -> do
            let px_size = if req_px_size < 0 then req_pt_size * dpi / 72 else req_px_size
            ft_face' <- peek ft_face
            size' <- peek $ frSize ft_face'
            return (px_size / (fromIntegral $ smY_ppem $ srMetrics size'), True)
        _ -> return (1, False)

    let hinting = fromMaybe True $ getValue' "hinting" pattern
    let antialias = fromMaybe True $ getValue' "antialias" pattern
    let hintstyle = fromMaybe 1 $ getValue' "hintstyle" pattern :: Int
    let rgba = fromMaybe 0 $ getValue' "rgba" pattern :: Int
    let load_flags | not antialias && (not hinting || hintstyle == 0) =
                        ft_LOAD_NO_HINTING .|. ft_LOAD_MONOCHROME
                   | not antialias = ft_LOAD_MONOCHROME
                   | not hinting || hintstyle == 0 = ft_LOAD_NO_HINTING
                   | otherwise = ft_LOAD_DEFAULT
    let load_target | not antialias && hinting && hintstyle /= 0 = ft_LOAD_TARGET_MONO
                    | not antialias = ft_LOAD_TARGET_NORMAL
                    | not hinting || hintstyle == 0 = ft_LOAD_TARGET_NORMAL
                    | hintstyle == 1 = ft_LOAD_TARGET_LIGHT
                    | hintstyle == 2 = ft_LOAD_TARGET_NORMAL
                    | rgba `elem` [1, 2] = ft_LOAD_TARGET_LCD
                    | rgba `elem` [3, 4] = ft_LOAD_TARGET_LCD_V
                    | otherwise = ft_LOAD_TARGET_NORMAL

    let embedded_bitmap = fromMaybe True $ getValue' "embeddedbitmap" pattern
    let load_flags1 | embedded_bitmap = load_flags .|. ft_LOAD_NO_BITMAP
                    | otherwise = load_flags
    let autohint = fromMaybe False $ getValue' "autohint" pattern
    let load_flags2 | autohint = load_flags .|. ft_LOAD_FORCE_AUTOHINT
                    | otherwise = load_flags
    let render_flags_normal | not antialias = ft_RENDER_MODE_MONO
                            | otherwise = ft_RENDER_MODE_NORMAL
    let render_flags_subpixel | not antialias = ft_RENDER_MODE_MONO
                              | rgba `elem` [1, 2] = ft_RENDER_MODE_LCD
                              | rgba `elem` [3, 4] = ft_RENDER_MODE_LCD_V
                              | otherwise = ft_RENDER_MODE_NORMAL

    let lcdfilter = case fromMaybe 1 $ getValue' "lcdfilter" pattern :: Int of {
        3 -> 16; x -> x}
    case getValue "matrix" pattern of
        ValueMatrix m -> ft_Set_Transform ft_face (Just $ m22toFt m) Nothing
        _ -> return ()

    ft_face' <- peek ft_face
    size' <- peek $ frSize ft_face'
    let metrics' = srMetrics size'
    let c x = fromIntegral x / 64 * pixel_fixup
    return Instance {
        fontName = getValue' "fullname" pattern,
        fontPath = getValue' "file" pattern,
        fontFace = ft_face,
        fontLoadFlags = load_target .|. load_flags .|. ft_LOAD_COLOR,
        fontAntialias = antialias,
        fontEmbolden = fromMaybe False $ getValue' "embolden" pattern,
        fontIsColor = fromMaybe False $ getValue' "color" pattern,
        fontRenderFlags = render_flags_normal,
        fontRenderFlagsSubpixel = render_flags_subpixel,
        fontPixelSizeFixup = pixel_fixup,
        fontPixelFixupEstimated = fixup_estimated,
        fontBGR = rgba `elem` [2, 4],
        fontLCDFilter = toEnum lcdfilter,
        fontFeats = getValues' "fontfeatures" pattern,
        fontMetrics = Metrics {
            height = fromEnum $ c $ smHeight metrics',
            descent = fromEnum $ c $ smDescender metrics',
            ascent = fromEnum $ c $ smAscender metrics',
            maxAdvance = (fromEnum $ c $ smMax_advance metrics',
                fromEnum $ c $ smHeight metrics'),
            metricsAntialias = antialias,
            metricsSubpixel = case rgba of
                _ | not antialias -> SubpixelNone
                1 -> SubpixelHorizontalRGB
                2 -> SubpixelHorizontalBGR
                3 -> SubpixelVerticalRGB
                4 -> SubpixelVerticalBGR
                _ -> SubpixelNone,
            metricsName = getValue' "fullname" pattern
        }
      }

-- | Results from `glyphForIndex`.
data FTFC_Glyph a = Glyph {
    glyphFontName :: Maybe String,
    glyphImage :: a,
    glyphAdvance :: (Double, Double),
    glyphSubpixel :: FTFC_Subpixel,
    glyphMetrics :: FT_Glyph_Metrics
}

-- | Looks up a given glyph in a `FTFC_Instance` & its underlying `FT_Face`
-- Taking into account additional properties from FontConfig.
-- Runs a provided callback to render the glyph into a reusable datastructure.
-- The `FT_Bitmap` given to this callback must not be used outside it.
-- Throws exceptions.
glyphForIndex :: FTFC_Instance -> Word32 -> FTFC_Subpixel -> 
    (FT_Bitmap -> IO a) -> IO (FTFC_Glyph a)
glyphForIndex font index subpixel cb = do
    ft_Load_Glyph (fontFace font) index (toEnum $ fontLoadFlags font)
    face' <- peek $ fontFace font
    size' <- peek $ frSize face'
    -- Formula from old FreeType function `FT_GlyphSlotEmbolden`.
    -- Approximate as fallback for fonts not using fontsets or variables axis.
    let strength = fromIntegral (frUnits_per_EM face')*smY_scale (srMetrics size')`div`24
    glyph' <- peek $ frGlyph face'

    glyph1' <- case gsrFormat glyph' of
        FT_GLYPH_FORMAT_OUTLINE | fontEmbolden font -> do
            outline <- withPtr (gsrOutline glyph') $ flip ft_Outline_Embolden strength
            return glyph' { gsrOutline = outline }
        _ -> return glyph'

    let render_flags = case subpixel of {
-- FT_GLYPH_FORMAT_SVG is not exposed by our language bindings,
-- Should be largely irrelevant now... Certain FreeType versions required this flag.
--        _ | FT_GLYPH_FORMAT_SVG <- gsrFormat glyph1' -> ft_RENDER_MODE_NORMAL;
        _ | not $ fontAntialias font -> fontRenderFlags font;
        SubpixelNone -> fontRenderFlags font;
        SubpixelHorizontalRGB -> ft_RENDER_MODE_LCD;
        SubpixelHorizontalBGR -> ft_RENDER_MODE_LCD;
        SubpixelVerticalRGB -> ft_RENDER_MODE_LCD_V;
        SubpixelVerticalBGR -> ft_RENDER_MODE_LCD_V;
        SubpixelDefault -> fontRenderFlagsSubpixel font}
    let bgr = case subpixel of {
        _ | not $ fontAntialias font -> False;
        SubpixelNone -> False;
        SubpixelHorizontalRGB -> False;
        SubpixelHorizontalBGR -> True;
        SubpixelVerticalRGB -> False;
        SubpixelVerticalBGR -> True;
        SubpixelDefault -> fontBGR font}

    can_set_lcd_filter <- isSuccess $ ft_Library_SetLcdFilter (gsrLibrary glyph1') 0
    -- FIXME: Do we need a mutex?
    let set_lcd_filter = ft_Library_SetLcdFilter (gsrLibrary glyph1') $ fontLCDFilter font
    case render_flags of {
        FT_RENDER_MODE_LCD | can_set_lcd_filter -> set_lcd_filter;
        FT_RENDER_MODE_LCD_V | can_set_lcd_filter -> set_lcd_filter;
        _ -> return ()}

    glyph2' <- case gsrFormat glyph1' of {
        FT_GLYPH_FORMAT_BITMAP -> return glyph1';
        _ -> withPtr glyph1' $ flip ft_Render_Glyph $ toEnum render_flags}
    -- If set_lcd_filter requires mutex, release it here.
    case gsrFormat glyph2' of {
        FT_GLYPH_FORMAT_BITMAP -> return ();
        _ -> throw $ FtError "glyphForIndex" 2
    }

    img <- cb $ gsrBitmap glyph2'
    return Glyph {
        glyphFontName = fontName font, glyphImage = img,
        glyphAdvance = (fromIntegral (vX $ gsrAdvance glyph2') / 64 *
            if fontPixelFixupEstimated font then fontPixelSizeFixup font else 1,
            fromIntegral (vY $ gsrAdvance glyph2') / 64 *
            if fontPixelFixupEstimated font then fontPixelSizeFixup font else 1),
        glyphSubpixel = subpixel,
        glyphMetrics = gsrMetrics glyph2'
    }

bmpAndMetricsForIndex ::
    FTFC_Instance -> FTFC_Subpixel -> Word32 -> IO (FT_Bitmap, FT_Glyph_Metrics)
bmpAndMetricsForIndex inst subpixel index = do
    glyph <- glyphForIndex inst index subpixel pure
    return (glyphImage glyph, glyphMetrics glyph)

withPtr :: Storable a => a -> (Ptr a -> IO b) -> IO a
withPtr a cb = alloca $ \a' -> do
    poke a' a
    cb a'
    peek a'

isSuccess :: IO a -> IO Bool
isSuccess cb = do
    cb
    return True
  `catch` \(FtError _ _) -> return False

m22toFt :: M22 Double -> FT_Matrix
m22toFt (V2 (V2 xx xy) (V2 yx yy)) = FT_Matrix {
    mXx = c xx * 0x10000, mXy = c xy * 0x10000,
    mYx = c yx * 0x10000, mYy = c yy * 0x10000
  } where c = toEnum . fromEnum

-- Taken from FreeType language bindings,
-- but converted to constants rather than pattern synonyms.
ft_LOAD_DEFAULT, ft_LOAD_NO_SCALE, ft_LOAD_NO_HINTING, ft_LOAD_RENDER,
    ft_LOAD_NO_BITMAP, ft_LOAD_VERTICAL_LAYOUT, ft_LOAD_FORCE_AUTOHINT,
    ft_LOAD_CROP_BITMAP, ft_LOAD_PEDANTIC, ft_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH,
    ft_LOAD_NO_RECURSE, ft_LOAD_IGNORE_TRANSFORM, ft_LOAD_MONOCHROME,
    ft_LOAD_LINEAR_DESIGN, ft_LOAD_NO_AUTOHINT, ft_LOAD_COLOR,
    ft_LOAD_COMPUTE_METRICS, ft_LOAD_BITMAP_METRICS_ONLY :: Int
ft_LOAD_DEFAULT                     = 0
ft_LOAD_NO_SCALE                    = 1
ft_LOAD_NO_HINTING                  = 2
ft_LOAD_RENDER                      = 4
ft_LOAD_NO_BITMAP                   = 8
ft_LOAD_VERTICAL_LAYOUT             = 16
ft_LOAD_FORCE_AUTOHINT              = 32
ft_LOAD_CROP_BITMAP                 = 64
ft_LOAD_PEDANTIC                    = 128
ft_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH = 512
ft_LOAD_NO_RECURSE                  = 1024
ft_LOAD_IGNORE_TRANSFORM            = 2048
ft_LOAD_MONOCHROME                  = 4096
ft_LOAD_LINEAR_DESIGN               = 8192
ft_LOAD_NO_AUTOHINT                 = 32768
ft_LOAD_COLOR                       = 1048576
ft_LOAD_COMPUTE_METRICS             = 2097152
ft_LOAD_BITMAP_METRICS_ONLY         = 4194304

ft_LOAD_TARGET_NORMAL, ft_LOAD_TARGET_LIGHT, ft_LOAD_TARGET_MONO,
    ft_LOAD_TARGET_LCD, ft_LOAD_TARGET_LCD_V :: Int
ft_LOAD_TARGET_NORMAL = 0
ft_LOAD_TARGET_LIGHT  = 65536
ft_LOAD_TARGET_MONO   = 131072
ft_LOAD_TARGET_LCD    = 196608
ft_LOAD_TARGET_LCD_V  = 262144

ft_RENDER_MODE_NORMAL, ft_RENDER_MODE_LIGHT, ft_RENDER_MODE_MONO,
    ft_RENDER_MODE_LCD, ft_RENDER_MODE_LCD_V :: Int
ft_RENDER_MODE_NORMAL = 0
ft_RENDER_MODE_LIGHT  = 1
ft_RENDER_MODE_MONO   = 2
ft_RENDER_MODE_LCD    = 3
ft_RENDER_MODE_LCD_V  = 4
