{-# LANGUAGE DeriveGeneric #-}
module Data.Text.Glyphize.Font where

import Data.ByteString.Internal (ByteString(..))
import Data.ByteString (packCStringLen)
import Data.Word (Word8, Word32)
import Data.Int (Int32)
import FreeType.Core.Base (FT_Face)
import Data.Text.Glyphize.Buffer (tag_to_string, tag_from_string, Direction, dir2int,
                                c2w, w2c)
import Data.Text.Glyphize.Oom (throwNull, throwFalse)

import Control.Monad (forM, unless)
import Control.Exception (bracket)
import Data.Maybe (fromMaybe)

import System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)
import Foreign.ForeignPtr (ForeignPtr(..), withForeignPtr, newForeignPtr, newForeignPtr_)
import Foreign.Ptr (Ptr(..), FunPtr(..), nullPtr, nullFunPtr, castPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (withArray, withArrayLen, peekArray)
import Foreign.Storable (Storable(..))
import Foreign.Storable.Generic (GStorable(..))
import GHC.Generics (Generic(..))
import Foreign.C.String (CString, withCString, withCStringLen, peekCString, peekCStringLen)

------
--- Features & Variants
------

-- | The structure that holds information about requested feature application.
-- The feature will be applied with the given value to all glyphs which are
-- in clusters between start (inclusive) and end (exclusive).
-- Setting start to HB_FEATURE_GLOBAL_START and end to HB_FEATURE_GLOBAL_END specifies
-- that the feature always applies to the entire buffer.
data Feature = Feature {
    featTag' :: Word32,
    -- ^ Tag of the feature. Use `featTag` to decode as an ASCII string.
    featValue :: Word32,
    -- ^ The value of the feature.
    -- 0 disables the feature, non-zero (usually 1) enables the feature.
    -- For features implemented as lookup type 3 (like "salt") the value
    -- is a one based index into the alternates.
    featStart :: Word,
    -- ^ The cluster to start applying this feature setting (inclusive).
    featEnd :: Word
    -- ^ The cluster to end applying this feature setting (exclusive).
} deriving (Read, Show, Generic, Ord, Eq)
instance GStorable Feature
-- | Parses a string into a hb_feature_t.
-- The format for specifying feature strings follows. All valid CSS
-- font-feature-settings values other than "normal" and the global values
-- are also accepted. CSS string escapes are not supported.
-- See https://harfbuzz.github.io/harfbuzz-hb-common.html#hb-feature-from-string
-- for additional details.
-- The range indices refer to the positions between Unicode characters.
-- The position before the first character is always 0.
parseFeature :: String -> Maybe Feature
parseFeature str = unsafePerformIO $ withCStringLen str $ \(str', len) -> alloca $ \ret' -> do
    success <- hb_feature_from_string str' len ret'
    if success then Just <$> peek ret' else return Nothing
parseFeature' str = unsafePerformIO $ withCStringLen str $ \(str', len) -> alloca $ \ret' -> do
    throwFalse $ hb_feature_from_string str' len ret'
    peek ret'
foreign import ccall "hb_feature_from_string" hb_feature_from_string
    :: CString -> Int -> Ptr Feature -> IO Bool
-- | Converts a `Feature` into a `String` in the format understood by `parseFeature`.
unparseFeature :: Feature -> String
unparseFeature feature = unsafePerformIO $ alloca $ \feature' -> allocaBytes 128 $ \ret' -> do
    feature' `poke` feature
    hb_feature_to_string feature' ret' 128
    peekCString ret'
foreign import ccall "hb_feature_to_string" hb_feature_to_string
    :: Ptr Feature -> CString -> Word -> IO ()

-- | Data type for holding variation data.
-- Registered OpenType variation-axis tags are listed in
-- [OpenType Axis Tag Registry](https://docs.microsoft.com/en-us/typography/opentype/spec/dvaraxisreg).
data Variation = Variation {
    varTag' :: Word32,
    -- ^ Tag of the variation-axis name. Use `varTag` to decode as an ASCII string.
    varValue :: Float
    -- ^ Value of the variation axis.
} deriving (Read, Show, Generic, Ord, Eq)
instance GStorable Variation
-- | Parses a string into a hb_variation_t.
-- The format for specifying variation settings follows.
-- All valid CSS font-variation-settings values other than "normal" and "inherited"
-- are also accepted, though, not documented below.
-- The format is a tag, optionally followed by an equals sign, followed by a number.
-- For example wght=500, or slnt=-7.5.
parseVariation :: String -> Maybe Variation
parseVariation str = unsafePerformIO $ withCStringLen str $ \(str', len) -> alloca $ \ret' -> do
    success <- hb_variation_from_string str' len ret'
    if success then Just <$> peek ret' else return Nothing
parseVariation' str = unsafePerformIO $ withCStringLen str $ \(str', len) -> alloca $ \ret' -> do
    throwFalse $ hb_variation_from_string str' len ret'
    peek ret'
foreign import ccall "hb_variation_from_string" hb_variation_from_string
    :: CString -> Int -> Ptr Variation -> IO Bool
-- | Converts a `Variation` into a `String` in the format understood by `parseVariation`.
unparseVariation var = unsafePerformIO $ alloca $ \var' -> allocaBytes 128 $ \ret' -> do
    var' `poke` var
    hb_variation_to_string var' ret' 128
    peekCString ret'
foreign import ccall "hb_variation_to_string" hb_variation_to_string
    :: Ptr Variation -> CString -> Word -> IO ()

-- | Tag of the feature.
featTag = tag_to_string . featTag'
-- | Tag of the variation-axis.
varTag = tag_to_string . varTag'
globalStart, globalEnd :: Word
-- | Special setting for `featStart` to apply the feature from the start of the buffer.
globalStart = 0
-- | Special setting for `featEnd` to apply the feature to the end of the buffer.
globalEnd = maxBound

------
--- Faces
------

-- | Fetches the number of `Face`s in a `ByteString`.
countFace :: ByteString -> Word
countFace bytes = unsafePerformIO $ withBlob bytes hb_face_count
foreign import ccall "hb_face_count" hb_face_count :: Blob_ -> IO Word

-- | A Font face.
type Face = ForeignPtr Face'
type Face_ = Ptr Face'
data Face'
-- | Constructs a new face object from the specified blob and a face index into that blob.
-- The face index is used for blobs of file formats such as TTC and and DFont that
-- can contain more than one face. Face indices within such collections are zero-based.
-- Note: If the blob font format is not a collection, index is ignored. Otherwise,
-- only the lower 16-bits of index are used. The unmodified index can be accessed
-- via `faceIndex`.
-- Note: The high 16-bits of index, if non-zero, are used by `createFont` to
-- load named-instances in variable fonts. See `createFont` for details.
createFace :: ByteString -> Word -> Face
createFace bytes index = unsafePerformIO $ do
    face <- withBlob bytes $ throwNull . flip hb_face_create index
    hb_face_make_immutable face
    newForeignPtr hb_face_destroy face
foreign import ccall "hb_face_create" hb_face_create :: Blob_ -> Word -> IO Face_
foreign import ccall "hb_face_make_immutable" hb_face_make_immutable :: Face_ -> IO ()
foreign import ccall "&hb_face_destroy" hb_face_destroy :: FunPtr (Face_ -> IO ())

-- | Creates a`Face` object from the specified `FT_Face`.
-- Not thread-safe due to FreeType dependency.
ftCreateFace :: FT_Face -> IO Face
ftCreateFace ft = do
    ret <- throwNull $ hb_ft_face_create_referenced ft
    newForeignPtr hb_face_destroy ret
foreign import ccall "hb_ft_face_create_referenced" hb_ft_face_create_referenced
    :: FT_Face -> IO Face_

-- | Fetches the singleton empty `Face` object.
emptyFace :: Face
emptyFace = unsafePerformIO $ newForeignPtr hb_face_destroy hb_face_get_empty
foreign import ccall "hb_face_get_empty" hb_face_get_empty :: Face_

-- | Fetches a list of all table tags for a face, if possible.
-- The list returned will begin at the offset provided
faceTableTags :: Face -> Word -> Word -> (Word, [String])
faceTableTags fce offs cnt = unsafePerformIO $ withForeignPtr fce $ \fce' -> do
    alloca $ \cnt' -> allocaBytes (fromEnum cnt * 4) $ \arr' -> do
        poke cnt' cnt
        length <- hb_face_get_table_tags fce' offs cnt' arr'
        cnt_ <- peek cnt'
        arr <- forM [0..fromEnum cnt_-1] $ peekElemOff arr'
        return (length, Prelude.map tag_to_string arr)
foreign import ccall "hb_face_get_table_tags" hb_face_get_table_tags
    :: Face_ -> Word -> Ptr Word -> Ptr Word32 -> IO Word

-- | Fetches the glyph-count value of the specified face object.
faceGlyphCount :: Face -> Word
faceGlyphCount = faceFunc hb_face_get_glyph_count
foreign import ccall "hb_face_get_glyph_count" hb_face_get_glyph_count :: Face_ -> Word

-- | Collects all of the Unicode characters covered by `Face` into a list of unique values.
faceCollectUnicodes :: Face -> [Word32]
faceCollectUnicodes = faceCollectFunc hb_face_collect_unicodes
foreign import ccall "hb_face_collect_unicodes" hb_face_collect_unicodes
    :: Face_ -> Set_ -> IO ()

-- | Collects all Unicode "Variation Selector" characters covered by `Face`
-- into a list of unique values.
faceCollectVarSels :: Face -> [Word32]
faceCollectVarSels = faceCollectFunc hb_face_collect_variation_selectors
foreign import ccall "hb_face_collect_variation_selectors"
    hb_face_collect_variation_selectors :: Face_ -> Set_ -> IO ()

-- | Collects all Unicode characters for variation_selector covered by `Face`
-- into a list of unique values.
faceCollectVarUnicodes :: Face -> Word32 -> [Word32]
faceCollectVarUnicodes fce varSel = (faceCollectFunc inner) fce
  where inner a b = hb_face_collect_variation_unicodes a varSel b
foreign import ccall "hb_face_collect_variation_unicodes"
    hb_face_collect_variation_unicodes :: Face_ -> Word32 -> Set_ -> IO ()

-- | Fetches the face-index corresponding to the given `Face`.
faceIndex :: Face -> Word
faceIndex = faceFunc hb_face_get_index
foreign import ccall "hb_face_get_index" hb_face_get_index :: Face_ -> Word

-- | Fetches the units-per-em (upem) value of the specified `Face` object.
faceUpem :: Face -> Word
faceUpem = faceFunc hb_face_get_upem
foreign import ccall "hb_face_get_upem" hb_face_get_upem :: Face_ -> Word

-- | Fetches the binary blob that contains the specified `Face`.
-- Returns an empty `ByteString` if referencing face data is not possible.
faceBlob :: Face -> ByteString
faceBlob = blob2bs . faceFunc hb_face_reference_blob
foreign import ccall "hb_face_reference_blob" hb_face_reference_blob :: Face_ -> Blob_

-- | Fetches the specified table within the specified face.
faceTable :: Face -> String -> ByteString
faceTable face tag = blob2bs $ unsafePerformIO $ withForeignPtr face $ \fce' -> do
    hb_face_reference_table fce' $ tag_from_string tag
foreign import ccall "hb_face_reference_table" hb_face_reference_table :: Face_ -> Word32 -> IO Blob_

------
--- Configure faces
------

-- | Allows configuring properties on a `Face` when creating it.
data FaceOptions = FaceOptions {
    faceOptGlyphCount :: Maybe Int,
    -- ^ Sets the glyph count for a newly-created `Face` to the specified value.
    faceOptIndex :: Maybe Word,
    -- ^ Assigns the specified face-index to the newly-created `Face`.
    -- Note: changing the index has no effect on the face itself,
    -- only value returned by `faceIndex`.
    faceOptUPEm :: Maybe Word
    -- ^ Sets the units-per-em (upem) for a newly-created `Face` object
    -- to the specified value.
} deriving (Read, Show, Ord, Eq)
-- | `FaceOptions` which has no effect on the newly-created `Face` object.
defaultFaceOptions = FaceOptions Nothing Nothing Nothing
-- | Internal utility to apply the given `FaceOptions` to a `Face`.
_setFaceOptions face opts = do
    case faceOptGlyphCount opts of
        Just x -> hb_face_set_glyph_count face x
        Nothing -> return ()
    case faceOptIndex opts of
        Just x -> hb_face_set_index face x
        Nothing -> return ()
    case faceOptUPEm opts of
        Just x -> hb_face_set_upem face x
        Nothing -> return ()
foreign import ccall "hb_face_set_glyph_count" hb_face_set_glyph_count
    :: Face_ -> Int -> IO ()
foreign import ccall "hb_face_set_index" hb_face_set_index :: Face_ -> Word -> IO ()
foreign import ccall "hb_face_set_upem" hb_face_set_upem :: Face_ -> Word -> IO ()

-- | Variant of `createFace` which applies given options.
createFaceWithOpts  :: FaceOptions -> ByteString -> Word -> Face
createFaceWithOpts opts bytes index = unsafePerformIO $ do
    face <- withBlob bytes $ throwNull . flip hb_face_create index
    _setFaceOptions face opts
    hb_face_make_immutable face
    newForeignPtr hb_face_destroy face
-- | Variant of `ftCreateFace` which applies given options.
ftCreateFaceWithOpts :: FaceOptions -> FT_Face -> IO Face
ftCreateFaceWithOpts opts ftFace = do
    face <- throwNull $ hb_ft_face_create_referenced ftFace
    _setFaceOptions face opts
    hb_face_make_immutable face
    newForeignPtr hb_face_destroy face

-- | Creates a `Face` containing the specified tables+tags, with the specified options.
-- Can be compiled to a binary font file by calling `faceBlob`,
-- with tables sorted by size then tag.
buildFace :: [(String, ByteString)] -> FaceOptions -> Face
buildFace tables opts = unsafePerformIO $ do
    builder <- throwNull hb_face_builder_create
    forM tables $ \(tag, bytes) ->
        throwFalse $ withBlob bytes $
            hb_face_builder_add_table builder $ tag_from_string tag
    _setFaceOptions builder opts
    hb_face_make_immutable builder
    newForeignPtr hb_face_destroy builder
foreign import ccall "hb_face_builder_create" hb_face_builder_create :: IO Face_
foreign import ccall "hb_face_builder_add_table" hb_face_builder_add_table
    :: Face_ -> Word32 -> Blob_ -> IO Bool
{-
-- | Creates a `Face` containing the specified tables+tags, with the specified options.
-- Can be compiled to a binary font file by calling `faceBlob`,
-- with tables in the given order.
buildOrderedFace :: [(String, ByteString)] -> FaceOptions -> Face
buildOrderedFace tables opts = unsafePerformIO $ do
    builder <- hb_face_builder_create
    forM tables $ \(tag, bytes) -> do
        blob <- bs2blob bytes
        withForeignPtr blob $ hb_face_builder_add_table builder $ tag_from_string tag
    withArray (map tag_from_string $ map fst tables) $ hb_face_builder_sort_tables builder
    _setFaceOptions builder opts
    hb_face_make_immutable builder
    newForeignPtr hb_face_destroy builder
foreign import ccall "hb_face_builder_sort_tables" hb_face_builder_sort_tables
    :: Face_ -> Ptr Word32 -> IO ()-}

------
--- Fonts
------

-- | Data type for holding fonts
type Font = ForeignPtr Font'
type Font_ = Ptr Font'
data Font'

-- | Constructs a new `Font` object from the specified `Face`.
-- Note: If face's index value (as passed to `createFace` has non-zero top 16-bits,
-- those bits minus one are passed to hb_font_set_var_named_instance(),
-- effectively loading a named-instance of a variable font,
-- instead of the default-instance.
-- This allows specifying which named-instance to load by default when creating the face.
createFont :: Face -> Font
createFont fce = unsafePerformIO $ do
    font <- throwNull $ withForeignPtr fce $ hb_font_create
    hb_font_make_immutable font
    newForeignPtr hb_font_destroy font
foreign import ccall "hb_font_create" hb_font_create :: Face_ -> IO Font_
foreign import ccall "hb_font_make_immutable" hb_font_make_immutable :: Font_ -> IO ()
foreign import ccall "&hb_font_destroy" hb_font_destroy :: FunPtr (Font_ -> IO ())

-- | Creates an `Font` object from the specified FT_Face.
-- Note: You must set the face size on ft_face before calling `ftCreateFont` on it.
-- HarfBuzz assumes size is always set
-- and will access `frSize`` member of `FT_Face` unconditionally.
ftCreateFont :: FT_Face -> IO Font
ftCreateFont fce = do
    font <- throwNull $ hb_ft_font_create_referenced fce
    hb_font_make_immutable font
    newForeignPtr hb_font_destroy font
foreign import ccall "hb_ft_font_create_referenced" hb_ft_font_create_referenced
    :: FT_Face -> IO Font_

-- | Constructs a sub-font font object from the specified parent font,
-- replicating the parent's properties.
createSubFont :: Font -> Font
createSubFont parent = unsafePerformIO $ do
    font <- throwNull $ withForeignPtr parent $ hb_font_create_sub_font
    hb_font_make_immutable font
    newForeignPtr hb_font_destroy font
foreign import ccall "hb_font_create_sub_font" hb_font_create_sub_font :: Font_ -> IO Font_

-- | Fetches the empty `Font` object.
emptyFont :: Font
emptyFont = unsafePerformIO $ newForeignPtr hb_font_destroy hb_font_get_empty
foreign import ccall "hb_font_get_empty" hb_font_get_empty :: Font_

-- | Fetches the `Face` associated with the specified `Font` object.
fontFace :: Font -> Face
fontFace font = unsafePerformIO $ withForeignPtr font $ \font' -> do
    face' <- throwNull $ hb_font_get_face font'
    newForeignPtr_ face' -- FIXME: Keep the font alive...
foreign import ccall "hb_font_get_face" hb_font_get_face :: Font_ -> IO Face_
 
-- | Fetches the glyph ID for a Unicode codepoint in the specified `Font`,
-- with an optional variation selector.
fontGlyph :: Font -> Char -> Maybe Char -> Maybe Word32
fontGlyph font char var =
    unsafeDupablePerformIO $ withForeignPtr font $ \font' -> alloca $ \ret -> do
        success <- hb_font_get_glyph font' (c2w char) (c2w $ fromMaybe '\0' var) ret
        if success then return . Just =<< peek ret else return Nothing
fontGlyph' font char var =
    unsafeDupablePerformIO $ withForeignPtr font $ \font' -> alloca $ \ret -> do
        throwFalse $ hb_font_get_glyph font' (c2w char) (c2w $ fromMaybe '\0' var) ret
        peek ret
foreign import ccall "hb_font_get_glyph" hb_font_get_glyph
    :: Font_ -> Word32 -> Word32 -> Ptr Word32 -> IO Bool

-- | Fetches the advance for a glyph ID from the specified font,
-- in a text segment of the specified direction.
-- Calls the appropriate direction-specific variant (horizontal or vertical)
-- depending on the value of direction .
fontGlyphAdvance :: Font -> Word32 -> Maybe Direction -> (Int32, Int32)
fontGlyphAdvance font glyph dir = unsafeDupablePerformIO $
    withForeignPtr font $ \font' -> alloca $ \x' -> alloca $ \y' -> do
        hb_font_get_glyph_advance_for_direction font' glyph (dir2int dir) x' y'
        x <- peek x'
        y <- peek y'
        return (x, y)
foreign import ccall "hb_font_get_glyph_advance_for_direction"
    hb_font_get_glyph_advance_for_direction
        :: Font_ -> Word32 -> Int -> Ptr Int32 -> Ptr Int32 -> IO ()

-- | Fetches the (x,y) coordinates of a specified contour-point index
-- in the specified glyph, within the specified font.
fontGlyphContourPoint :: Font -> Word32 -> Int -> Maybe (Int32, Int32)
fontGlyphContourPoint font glyph index = unsafeDupablePerformIO $
    withForeignPtr font $ \font' -> alloca $ \x' -> alloca $ \y' -> do
        success <- hb_font_get_glyph_contour_point font' glyph index x' y'
        if success
        then do
            x <- peek x'
            y <- peek y'
            return $ Just (x, y)
        else return Nothing
fontGlyphContourPoint' font glyph index = unsafeDupablePerformIO $
    withForeignPtr font $ \font' -> alloca $ \x' -> alloca $ \y' -> do
        throwFalse $ hb_font_get_glyph_contour_point font' glyph index x' y'
        x <- peek x'
        y <- peek y'
        return (x, y)
foreign import ccall "hb_font_get_glyph_contour_point" hb_font_get_glyph_contour_point
    :: Font_ -> Word32 -> Int -> Ptr Int32 -> Ptr Int32 -> IO Bool

-- | Fetches the (X,Y) coordinates of a specified contour-point index
-- in the specified glyph ID in the specified font,
-- with respect to the origin in a text segment in the specified direction.
-- Calls the appropriate direction-specific variant (horizontal or vertical)
-- depending on the value of direction .
fontGlyphContourPointForOrigin :: Font -> Word32 -> Int -> Maybe Direction -> Maybe (Int32, Int32)
fontGlyphContourPointForOrigin font glyph index dir = unsafeDupablePerformIO $
    withForeignPtr font $ \font' -> alloca $ \x' -> alloca $ \y' -> do
        success <- hb_font_get_glyph_contour_point_for_origin font' glyph index
                (dir2int dir) x' y'
        if success
        then do
            x <- peek x'
            y <- peek y'
            return $ Just (x, y)
        else return Nothing
fontGlyphContourPointForOrigin' font glyph index dir = unsafeDupablePerformIO $
    withForeignPtr font $ \font' -> alloca $ \x' -> alloca $ \y' -> do
        throwFalse $ hb_font_get_glyph_contour_point_for_origin font' glyph index
                (dir2int dir) x' y'
        x <- peek x'
        y <- peek y'
        return (x, y)
foreign import ccall "hb_font_get_glyph_contour_point_for_origin"
    hb_font_get_glyph_contour_point_for_origin
        :: Font_ -> Word32 -> Int -> Int -> Ptr Int32 -> Ptr Int32 -> IO Bool

-- | Glyph extent values, measured in font units.
-- Note that height is negative, in coordinate systems that grow up.
data GlyphExtents = GlyphExtents {
    xBearing :: Word32,
    -- ^ Distance from the x-origin to the left extremum of the glyph.
    yBearing :: Word32,
    -- ^ Distance from the top extremum of the glyph to the y-origin.
    width :: Word32,
    -- ^ Distance from the left extremum of the glyph to the right extremum.
    height :: Word32
    -- ^ Distance from the top extremum of the glyph to the right extremum.
} deriving (Generic, Read, Show, Ord, Eq)
instance GStorable GlyphExtents
-- | Fetches the `GlyphExtents` data for a glyph ID in the specified `Font`.
fontGlyphExtents :: Font -> Word32 -> Maybe GlyphExtents
fontGlyphExtents font glyph = unsafeDupablePerformIO $
    withForeignPtr font $ \font' -> alloca $ \ret -> do
        success <- hb_font_get_glyph_extents font' glyph ret
        if success
        then return . Just =<< peek ret
        else return Nothing
fontGlyphExtents' font glyph = unsafeDupablePerformIO $
    withForeignPtr font $ \font' -> alloca $ \ret -> do
        throwFalse $ hb_font_get_glyph_extents font' glyph ret
        peek ret
foreign import ccall "hb_font_get_glyph_extents" hb_font_get_glyph_extents
    :: Font_ -> Word32 -> Ptr GlyphExtents -> IO Bool

-- | Fetches the `GlyphExtents` data for a glyph ID in the specified `Font`,
-- with respect to the origin in a text segment in the specified direction.
-- Calls the appropriate direction-specific variant (horizontal or vertical)
-- depending on the value of given `Direction`.
fontGlyphExtentsForOrigin :: Font -> Word32 -> Maybe Direction -> Maybe GlyphExtents
fontGlyphExtentsForOrigin font glyph dir = unsafeDupablePerformIO $
    withForeignPtr font $ \font' -> alloca $ \ret -> do
        ok <- hb_font_get_glyph_extents_for_origin font' glyph (dir2int dir) ret
        if ok
        then return . Just =<< peek ret
        else return Nothing
fontGlyphExtentsForOrigin' font glyph dir = unsafeDupablePerformIO $
    withForeignPtr font $ \font' -> alloca $ \ret -> do
        throwFalse $ hb_font_get_glyph_extents_for_origin font' glyph (dir2int dir) ret
        peek ret
foreign import ccall "hb_font_get_glyph_extents_for_origin"
    hb_font_get_glyph_extents_for_origin
        :: Font_ -> Word32 -> Int -> Ptr GlyphExtents -> IO Bool

-- | Fetches the glyph ID that corresponds to a name string in the specified `Font`.
fontGlyphFromName :: Font -> String -> Maybe Word32
fontGlyphFromName font name = unsafeDupablePerformIO $
    withForeignPtr font $ \font' -> alloca $ \ret -> do
        success <- withCStringLen name $ \(name', len) ->
            hb_font_get_glyph_from_name font' name' len ret
        if success
        then return . Just =<< peek ret
        else return Nothing
fontGlyphFromName' font name = unsafeDupablePerformIO $
    withForeignPtr font $ \font' -> alloca $ \ret -> do
        throwFalse $ withCStringLen name $ \(name', len) ->
            hb_font_get_glyph_from_name font' name' len ret
        peek ret
foreign import ccall "hb_font_get_glyph_from_name" hb_font_get_glyph_from_name
    :: Font_ -> CString -> Int -> Ptr Word32 -> IO Bool

-- | Fetches the advance for a glyph ID in the specified `Font`,
-- for horizontal text segments.
fontGlyphHAdvance :: Font -> Word32 -> Int32
fontGlyphHAdvance = fontFunc hb_font_get_glyph_h_advance
foreign import ccall "hb_font_get_glyph_h_advance" hb_font_get_glyph_h_advance
    :: Font_ -> Word32 -> Int32

-- | Fetches the advance for a glyph ID in the specified `Font`,
-- for vertical text segments.
fontGlyphVAdvance :: Font -> Word32 -> Int32
fontGlyphVAdvance = fontFunc hb_font_get_glyph_v_advance
foreign import ccall "hb_font_get_glyph_v_advance" hb_font_get_glyph_v_advance
    :: Font_ -> Word32 -> Int32

-- | Fetches the kerning-adjustment value for a glyph-pair in the specified `Font`,
-- for horizontal text segments.
fontGlyphHKerning :: Font -> Word32 -> Word32 -> Int32
fontGlyphHKerning = fontFunc hb_font_get_glyph_h_kerning
foreign import ccall "hb_font_get_glyph_h_kerning" hb_font_get_glyph_h_kerning
    :: Font_ -> Word32 -> Word32 -> Int32

-- | Fetches the (X,Y) coordinate of the origin for a glyph ID in the specified `Font`,
-- for horizontal text segments.
fontGlyphHOrigin :: Font -> Word32 -> Maybe (Int32, Int32)
fontGlyphHOrigin font glyph = unsafeDupablePerformIO $ withForeignPtr font $ \font' ->
    alloca $ \x' -> alloca $ \y' -> do
        success <- hb_font_get_glyph_h_origin font' glyph x' y'
        if success
        then do
            x <- peek x'
            y <- peek y'
            return $ Just (x, y)
        else return Nothing
fontGlyphHOrigin' font glyph = unsafeDupablePerformIO $ withForeignPtr font $ \font' ->
    alloca $ \x' -> alloca $ \y' -> do
        throwFalse $ hb_font_get_glyph_h_origin font' glyph x' y'
        x <- peek x'
        y <- peek y'
        return (x, y)
foreign import ccall "hb_font_get_glyph_h_origin" hb_font_get_glyph_h_origin ::
    Font_ -> Word32 -> Ptr Int32 -> Ptr Int32 -> IO Bool

-- | Fetches the (X,Y) coordinates of the origin for a glyph ID in the specified `Font`,
-- for vertical text segments.
fontGlyphVOrigin :: Font -> Word32 -> Maybe (Int32, Int32)
fontGlyphVOrigin font glyph = unsafeDupablePerformIO $ withForeignPtr font $ \font' ->
    alloca $ \x' -> alloca $ \y' -> do
        success <- hb_font_get_glyph_v_origin font' glyph x' y'
        if success
        then do
            x <- peek x'
            y <- peek y'
            return $ Just (x, y)
        else return Nothing
fontGlyphVOrigin' font glyph = unsafeDupablePerformIO $ withForeignPtr font $ \font' ->
    alloca $ \x' -> alloca $ \y' -> do
        throwFalse $ hb_font_get_glyph_v_origin font' glyph x' y'
        x <- peek x'
        y <- peek y'
        return (x, y)
foreign import ccall "hb_font_get_glyph_v_origin" hb_font_get_glyph_v_origin ::
    Font_ -> Word32 -> Ptr Int32 -> Ptr Int32 -> IO Bool

-- | Fetches the kerning-adjustment value for a glyph-pair in the specified `Font`.
-- Calls the appropriate direction-specific variant (horizontal or vertical)
-- depending on the value of given `Direction`.
fontGlyphKerningForDir :: Font -> Word32 -> Word32 -> Maybe Direction -> (Int32, Int32)
fontGlyphKerningForDir font a b dir = unsafeDupablePerformIO $ withForeignPtr font $ \font' ->
    alloca $ \x' -> alloca $ \y' -> do
        hb_font_get_glyph_kerning_for_direction font' a b (dir2int dir) x' y'
        x <- peek x'
        y <- peek y'
        return (x, y)
foreign import ccall "hb_font_get_glyph_kerning_for_direction"
    hb_font_get_glyph_kerning_for_direction ::
        Font_ -> Word32 -> Word32 -> Int -> Ptr Int32 -> Ptr Int32 -> IO ()

-- | Fetches the glyph-name string for a glyph ID in the specified `Font`.
fontGlyphName :: Font -> Word32 -> Maybe String
fontGlyphName a b = fontGlyphName_ a b 32
fontGlyphName' a b = fontGlyphName_' a b 32
-- | Variant of `fontGlyphName` which lets you specify the maximum of the return value.
-- Defaults to 32.
fontGlyphName_ :: Font -> Word32 -> Int -> Maybe String
fontGlyphName_ font glyph size = unsafeDupablePerformIO $ withForeignPtr font $ \font' ->
    allocaBytes size $ \name' -> do
        success <- hb_font_get_glyph_name font' glyph name' (toEnum size)
        if success
        then Just <$> peekCStringLen (name', size)
        else return Nothing
fontGlyphName_' font glyph size = unsafeDupablePerformIO $ withForeignPtr font $ \font' ->
    allocaBytes size $ \name' -> do
        throwFalse $ hb_font_get_glyph_name font' glyph name' (toEnum size)
        peekCStringLen (name', size)
foreign import ccall "hb_font_get_glyph_name" hb_font_get_glyph_name ::
    Font_ -> Word32 -> CString -> Word32 -> IO Bool

-- | Fetches the (X,Y) coordinates of the origin for a glyph in the specified `Font`.
-- Calls the appropriate direction-specific variant (horizontal or vertical)
-- depending on the value of given `Direction`.
fontGlyphOriginForDir :: Font -> Word32 -> Maybe Direction -> (Int32, Int32)
fontGlyphOriginForDir font glyph dir = unsafeDupablePerformIO $ withForeignPtr font $ \font' ->
    alloca $ \x' -> alloca $ \y' -> do
        hb_font_get_glyph_origin_for_direction font' glyph (dir2int dir) x' y'
        x <- peek x'
        y <- peek y'
        return (x, y)
foreign import ccall "hb_font_get_glyph_origin_for_direction"
    hb_font_get_glyph_origin_for_direction ::
        Font_ -> Word32 -> Int -> Ptr Int32 -> Ptr Int32 -> IO ()

-- Skipping Draw methodtables, easier to use FreeType for that.

-- | Fetches the nominal glyph ID for a Unicode codepoint in the specified font.
-- This version of the function should not be used to fetch glyph IDs for codepoints
-- modified by variation selectors. For variation-selector support use
-- `fontVarGlyph` or use `fontGlyph`.
fontNominalGlyph :: Font -> Char -> Maybe Word32
fontNominalGlyph font c =
    unsafeDupablePerformIO $ withForeignPtr font $ \font' -> alloca $ \glyph' -> do
        success <- hb_font_get_nominal_glyph font' (c2w c) glyph'
        if success then Just <$> peek glyph' else return Nothing
fontNominalGlyph' font c =
    unsafeDupablePerformIO $ withForeignPtr font $ \font' -> alloca $ \glyph' -> do
        throwFalse $ hb_font_get_nominal_glyph font' (c2w c) glyph'
        peek glyph'
foreign import ccall "hb_font_get_nominal_glyph" hb_font_get_nominal_glyph ::
    Font_ -> Word32 -> Ptr Word32 -> IO Bool

-- | Fetches the parent of the given `Font`.
fontParent :: Font -> Font
fontParent child =
    unsafeDupablePerformIO (newForeignPtr_ =<< withForeignPtr child hb_font_get_parent)
foreign import ccall "hb_font_get_parent" hb_font_get_parent :: Font_ -> IO Font_

-- | Fetches the horizontal & vertical points-per-em (ppem) of a `Font`.
fontPPEm :: Font -> (Word32, Word32)
fontPPEm font =
    unsafeDupablePerformIO $ withForeignPtr font $ \font' -> alloca $ \x' -> alloca $ \y' -> do
        hb_font_get_ppem font' x' y'
        x <- peek x'
        y <- peek y'
        return (x, y)
foreign import ccall "hb_font_get_ppem" hb_font_get_ppem ::
    Font_ -> Ptr Word32 -> Ptr Word32 -> IO ()

-- | Fetches the "point size" of a `Font`. Used in CoreText to implement optical sizing.
fontPtEm :: Font -> Float
fontPtEm = fontFunc hb_font_get_ptem
foreign import ccall "hb_font_get_ptem" hb_font_get_ptem :: Font_ -> Float

-- | Fetches the horizontal and vertical scale of a `Font`.
fontScale :: Font -> (Int, Int)
fontScale font = unsafeDupablePerformIO $
    withForeignPtr font $ \font' -> alloca $ \x' -> alloca $ \y' -> do
        hb_font_get_scale font' x' y'
        x <- peek x' :: IO Int32
        y <- peek y' :: IO Int32
        return (fromEnum x, fromEnum y)
foreign import ccall "hb_font_get_scale" hb_font_get_scale
    :: Font_ -> Ptr Int32 -> Ptr Int32 -> IO ()

-- | Fetches the "synthetic slant" of a font.
fontSyntheticSlant :: Font -> Float
fontSyntheticSlant = fontFunc hb_font_get_synthetic_slant
foreign import ccall "hb_font_get_synthetic_slant" hb_font_get_synthetic_slant ::
    Font_ -> Float

-- | Fetches the glyph ID for a Unicode codepoint when followed by
-- the specified variation-selector codepoint, in the specified `Font`.
fontVarGlyph :: Font -> Word32 -> Word32 -> Maybe Word32
fontVarGlyph font unicode varSel = unsafeDupablePerformIO $
    withForeignPtr font $ \font' -> alloca $ \glyph' -> do
        success <- hb_font_get_variation_glyph font' unicode varSel glyph'
        if success
        then return . Just =<< peek glyph'
        else return Nothing
fontVarGlyph' font unicode varSel = unsafeDupablePerformIO $
    withForeignPtr font $ \font' -> alloca $ \glyph' -> do
        throwFalse $ hb_font_get_variation_glyph font' unicode varSel glyph'
        peek glyph'
foreign import ccall "hb_font_get_variation_glyph" hb_font_get_variation_glyph
    :: Font_ -> Word32 -> Word32 -> Ptr Word32 -> IO Bool

-- | Fetches the list of variation coordinates (in design-space units)
-- currently set on a `Font`.
-- Note that this returned list may only contain values for some (or none) of the axes;
-- ommitted axes effectively have their default values.
fontVarCoordsDesign :: Font -> [Float]
fontVarCoordsDesign font = unsafeDupablePerformIO $
    withForeignPtr font $ \font' -> alloca $ \length' -> do
        arr <- hb_font_get_var_coords_design font' length'
        length <- peek length'
        peekArray (fromEnum length) arr
foreign import ccall "hb_font_get_var_coords_design"
    hb_font_get_var_coords_design :: Font_ -> Ptr Word -> IO (Ptr Float)

-- | Fetches the list of normalized variation coordinates currently set on a font.
-- Note that this returned list may only contain values for some (or none) of the axes;
-- ommitted axes effectively have default values.
fontVarCoordsNormalized :: Font -> [Int]
fontVarCoordsNormalized font = unsafeDupablePerformIO $
    withForeignPtr font $ \font' -> alloca $ \length' -> do
        arr <- throwNull $ hb_font_get_var_coords_normalized font' length'
        length <- peek length'
        forM [0..fromEnum length-1] $ peekElemOff arr
foreign import ccall "hb_font_get_var_coords_normalized"
    hb_font_get_var_coords_normalized :: Font_ -> Ptr Word -> IO (Ptr Int)

-- | Fetches the glyph ID from given `Font` that matches the specified string.
-- Strings of the format gidDDD or uniUUUU are parsed automatically.
fontTxt2Glyph :: Font -> String -> Maybe Word32
fontTxt2Glyph font str = unsafeDupablePerformIO $
    withForeignPtr font $ \font' -> alloca $ \ret -> do
        ok <- withCStringLen str $ \(str', len) ->
            hb_font_glyph_from_string font' str' len ret
        if ok
        then return . Just =<< peek ret
        else return Nothing
fontTxt2Glyph' font str = unsafeDupablePerformIO $
    withForeignPtr font $ \font' -> alloca $ \ret -> do
        throwFalse $ withCStringLen str $ \(str', len) ->
            hb_font_glyph_from_string font' str' len ret
        peek ret
foreign import ccall "hb_font_glyph_from_string" hb_font_glyph_from_string
    :: Font_ -> CString -> Int -> Ptr Word32 -> IO Bool

-- | Fetches the name of the specified glyph ID in given `Font` as a string.
-- If the glyph ID has no name in the `Font`, a string of the form gidDDD is generated
-- with DDD being the glyph ID.
fontGlyph2Str :: Font -> Word32 -> Int -> String
fontGlyph2Str font glyph length = unsafeDupablePerformIO $
    withForeignPtr font $ \font' -> allocaBytes length $ \ret -> do
        hb_font_glyph_to_string font' glyph ret length
        peekCString ret
foreign import ccall "hb_font_glyph_to_string" hb_font_glyph_to_string
    :: Font_ -> Word32 -> CString -> Int -> IO ()

-- | Font-wide extent values, measured in font units.
-- Note that typically ascender is positive and descender is negative,
-- in coordinate systems that grow up.
-- Note: Due to presence of 9 additional private fields,
-- arrays of font extents will not decode correctly. So far this doesn't matter.
data FontExtents = FontExtents {
    ascender :: Int32,
    -- ^ The height of typographic ascenders.
    descender :: Int32,
    -- ^ The depth of typographic descenders.
    lineGap :: Int32
    -- ^ The suggested line-spacing gap.
} deriving (Generic, Read, Show, Ord, Eq)
instance GStorable FontExtents
-- | Fetches the extents for a font in a text segment of the specified direction.
-- Calls the appropriate direction-specific variant (horizontal or vertical)
-- depending on the value of direction .
fontExtentsForDir :: Font -> Maybe Direction -> FontExtents
fontExtentsForDir font dir = unsafeDupablePerformIO $ alloca $ \ret -> do
    withForeignPtr font $ \font' ->
        hb_font_get_extents_for_direction font' (dir2int dir) ret
    peek ret
foreign import ccall "hb_font_get_extents_for_direction"
    hb_font_get_extents_for_direction :: Font_ -> Int -> Ptr FontExtents -> IO ()

-- | Fetches the extents for a specified font, for horizontal text segments.
fontHExtents font = unsafeDupablePerformIO $ alloca $ \ret -> do
    ok <- withForeignPtr font $ \font' -> hb_font_get_h_extents font' ret
    if ok
    then return . Just =<< peek ret
    else return Nothing
fontHExtents' font = unsafeDupablePerformIO $ alloca $ \ret -> do
    throwFalse $ withForeignPtr font $ \font' -> hb_font_get_h_extents font' ret
    peek ret
foreign import ccall "hb_font_get_h_extents" hb_font_get_h_extents
    :: Font_ -> Ptr FontExtents -> IO Bool

-- | Fetches the extents for a specified font, for vertical text segments.
fontVExtents font = unsafeDupablePerformIO $ alloca $ \ret -> do
    ok <- withForeignPtr font $ \font' -> hb_font_get_v_extents font' ret
    if ok
    then return . Just =<< peek ret
    else return Nothing
fontVExtents' font = unsafeDupablePerformIO $ alloca $ \ret -> do
    throwFalse $ withForeignPtr font $ \font' -> hb_font_get_v_extents font' ret
    peek ret
foreign import ccall "hb_font_get_v_extents" hb_font_get_v_extents
    :: Font_ -> Ptr FontExtents -> IO Bool

-- Not exposing the Font Funcs API as being extremely imperative with little value to callers.

------
--- Configurable fonts
------

-- | Allows configuring properties on a `Font` when creating it.
data FontOptions = FontOptions {
    optionPPEm :: Maybe (Word, Word),
    -- ^ Sets the horizontal and vertical pixels-per-em (ppem) of the newly-created `Font`.
    optionPtEm :: Maybe Float,
    -- ^ Sets the "point size" of a newly-created `Font`.
    -- Used in CoreText to implement optical sizing.
    -- Note: There are 72 points in an inch.
    optionScale :: Maybe (Int, Int),
    -- ^ Sets the horizontal and vertical scale of a newly-created `Font`.
    optionFace :: Maybe Face,
    -- ^ Sets the font-face value of the newly-created `Font`.
    optionParent :: Maybe Font,
    -- ^ Sets the parent `Font` of the newly-created `Font`.
    optionSynthSlant :: Maybe Float,
    -- ^ Sets the "synthetic slant" of a newly-created `Font`. By default is zero.
    -- Synthetic slant is the graphical skew applied to the font at rendering time.
    -- Harfbuzz needs to know this value to adjust shaping results, metrics,
    -- and style valuesto match the slanted rendering.
    -- Note: The slant value is a ratio. For example, a 20% slant would be
    -- represented as a 0.2 value.
    optionVariations :: [Variation],
    -- ^ Applies a list of font-variation settings to a font.
    -- Axes not included will be effectively set to their default values.
    optionVarCoordsDesign :: [Float],
    -- ^ Applies a list of variation coordinates (in design-space units)
    -- to a newly-created `Font`.
    -- Axes not included in coords will be effectively set to their default values.
    optionVarCoordsNormalized :: [Int],
    -- ^ Applies a list of variation coordinates (in normalized units)
    -- to a newly-created `Font`.
    -- Axes not included in coords will be effectively set to their default values.
    optionVarNamedInstance :: Maybe Word
    -- ^ Sets design coords of a font from a named instance index.
} deriving (Show, Ord, Eq)
-- | `FontOptions` which has no effect on the newly-created `Font`.
defaultFontOptions = FontOptions {
    optionPPEm = Nothing, optionPtEm = Nothing, optionScale = Nothing,
    optionFace = Nothing, optionParent = Nothing, optionSynthSlant = Nothing,
    optionVariations = [], optionVarCoordsDesign = [], optionVarCoordsNormalized = [],
    optionVarNamedInstance = Nothing
}
-- | Internal utility to apply the given `FontOptions` to the given `Font`.
_setFontOptions font opts = do
    case optionPPEm opts of
        Just (x, y) -> hb_font_set_ppem font x y
        Nothing -> return ()
    case optionPtEm opts of
        Just ptem -> hb_font_set_ptem font ptem
        Nothing -> return ()
    case optionScale opts of
        Just (x, y) -> hb_font_set_scale font x y
        Nothing -> return ()
    case optionFace opts of
        Just face -> withForeignPtr face $ hb_font_set_face font
        Nothing -> return ()
    case optionParent opts of
        Just parent -> withForeignPtr parent $ hb_font_set_parent font
        Nothing -> return ()
    case optionSynthSlant opts of
        Just slant -> hb_font_set_synthetic_slant font slant
        Nothing -> return ()

    unless (null $ optionVariations opts) $
        withArrayLen (optionVariations opts) $ \len vars ->
            hb_font_set_variations font vars $ toEnum len
    unless (null $ optionVarCoordsDesign opts) $
        withArrayLen (optionVarCoordsDesign opts) $ \len coords ->
            hb_font_set_var_coords_design font coords $ toEnum len
    unless (null $ optionVarCoordsNormalized opts) $
        withArrayLen (optionVarCoordsNormalized opts) $ \len coords ->
            hb_font_set_var_coords_normalized font coords $ toEnum len
    case optionVarNamedInstance opts of
        Just inst -> hb_font_set_var_named_instance font inst
        Nothing -> return ()
foreign import ccall "hb_font_set_ppem" hb_font_set_ppem :: Font_ -> Word -> Word -> IO ()
foreign import ccall "hb_font_set_ptem" hb_font_set_ptem :: Font_ -> Float -> IO ()
foreign import ccall "hb_font_set_scale" hb_font_set_scale :: Font_ -> Int -> Int -> IO ()
foreign import ccall "hb_font_set_face" hb_font_set_face :: Font_ -> Face_ -> IO ()
foreign import ccall "hb_font_set_parent" hb_font_set_parent :: Font_ -> Font_ -> IO ()
foreign import ccall "hb_font_set_synthetic_slant" hb_font_set_synthetic_slant ::
    Font_ -> Float -> IO ()
foreign import ccall "hb_font_set_variations" hb_font_set_variations ::
    Font_ -> Ptr Variation -> Word -> IO ()
foreign import ccall "hb_font_set_var_coords_design" hb_font_set_var_coords_design ::
    Font_ -> Ptr Float -> Word -> IO ()
foreign import ccall "hb_font_set_var_coords_normalized"
    hb_font_set_var_coords_normalized :: Font_ -> Ptr Int -> Word -> IO ()
foreign import ccall "hb_font_set_var_named_instance" hb_font_set_var_named_instance ::
    Font_ -> Word -> IO ()

-- | Variant of `createFont` which applies the given `FontOptions`.
createFontWithOptions :: FontOptions -> Face -> Font
createFontWithOptions opts fce = unsafePerformIO $ do
    font <- throwNull $ withForeignPtr fce $ hb_font_create
    _setFontOptions font opts
    hb_font_make_immutable font
    newForeignPtr hb_font_destroy font

-- | Variant of `ftCreateFont` which applies the given `FontOptions`.
ftCreateFontWithOptions :: FontOptions -> FT_Face -> Font
ftCreateFontWithOptions opts fce = unsafePerformIO $ do
    font <- throwNull $ hb_ft_font_create_referenced fce
    _setFontOptions font opts
    hb_font_make_immutable font
    newForeignPtr hb_font_destroy font

-- | Variant of createSubFont which applies the given `FontOptions`.
createSubFontWithOptions :: FontOptions -> Font -> Font
createSubFontWithOptions opts font = unsafePerformIO $ do
    font <- throwNull $ withForeignPtr font $ hb_font_create_sub_font
    _setFontOptions font opts
    hb_font_make_immutable font
    newForeignPtr hb_font_destroy font

------
--- Internal
------

-- | Harfbuzz's equivalent to the ByteString type.
type Blob = ForeignPtr Blob'
data Blob'
type Blob_ = Ptr Blob'
-- | Convert from a ByteString to Harfbuzz's equivalent.
bs2blob :: ByteString -> IO Blob
bs2blob (BS bytes len) = do
    blob <- throwNull $ withForeignPtr bytes $ \bytes' ->
        hb_blob_create bytes' len hb_MEMORY_MODE_DUPLICATE nullPtr nullFunPtr
    newForeignPtr hb_blob_destroy blob
-- | Convert from a ByteString to a temporary copy of Harfbuzz's equivalent.
-- Do not use this Blob outside the passed callback.
withBlob :: ByteString -> (Blob_ -> IO a) -> IO a
withBlob (BS bytes len) cb = withForeignPtr bytes $ \bytes' -> do
    throwNull $ pure bytes'
    bracket
        (hb_blob_create bytes' len hb_MEMORY_MODE_READONLY nullPtr nullFunPtr)
        hb_blob_destroy' cb
foreign import ccall "hb_blob_create" hb_blob_create ::
    Ptr Word8 -> Int -> Int -> Ptr () -> FunPtr (Ptr () -> IO ()) -> IO Blob_
hb_MEMORY_MODE_DUPLICATE = 0
hb_MEMORY_MODE_READONLY = 1
foreign import ccall "&hb_blob_destroy" hb_blob_destroy :: FunPtr (Blob_ -> IO ())

-- | Convert to a ByteString from Harfbuzz's equivalent.
blob2bs :: Blob_ -> ByteString
blob2bs blob = unsafePerformIO $ alloca $ \length' -> do
    dat <- hb_blob_get_data blob length'
    length <- peek length'
    ret <- packCStringLen (dat, fromIntegral length)
    hb_blob_destroy' blob
    return ret
foreign import ccall "hb_blob_get_data" hb_blob_get_data :: Blob_ -> Ptr Word -> IO CString
foreign import ccall "hb_blob_destroy" hb_blob_destroy' :: Blob_ -> IO ()

-- | Internal utility for defining trivial language bindings unwrapping `Face` foreign pointers.
faceFunc :: (Face_ -> a) -> (Face -> a)
faceFunc cb fce = unsafeDupablePerformIO $ withForeignPtr fce $ return . cb

-- | Internal utility for defining trivial language bindings unwrapping `Font` foreign pointers.
fontFunc :: (Font_ -> a) -> (Font -> a)
fontFunc cb fnt = unsafeDupablePerformIO $ withForeignPtr fnt $ return . cb

-- | Internal utility for exposing Harfbuzz functions that populate a bitset.
-- Converts the populated bitset to a Haskell lazy linked-list.
faceCollectFunc :: (Face_ -> Set_ -> IO ()) -> (Face -> [Word32])
faceCollectFunc cb fce = unsafePerformIO $ withForeignPtr fce $ \fce' -> do
    set <- createSet
    withForeignPtr set $ cb fce'
    set2list set

-- | A Harfbuzz bitset.
data Set'
type Set = ForeignPtr Set'
type Set_ = Ptr Set'
-- | Creates a Harfbuzz bitset wrapping it in a foreignpointer.
createSet :: IO Set
createSet = do
    ret <- throwNull hb_set_create
    newForeignPtr hb_set_destroy ret
foreign import ccall "hb_set_create" hb_set_create :: IO Set_
foreign import ccall "&hb_set_destroy" hb_set_destroy :: FunPtr (Set_ -> IO ())

-- | Lazily retrieves the next codepoint in a bitset.
setNext :: Set -> Word32 -> Maybe Word32
setNext set iter = unsafePerformIO $ withForeignPtr set $ \set' -> alloca $ \iter' -> do
    poke iter' iter
    success <- hb_set_next set' iter'
    if success
    then return . Just =<< peek iter'
    else return Nothing
foreign import ccall "hb_set_next" hb_set_next :: Set_ -> Ptr Word32 -> IO Bool

-- | Converts a Harfbuzz bitset into a lazy linkedlist.
set2list :: Set -> IO [Word32]
set2list set = return $ inner maxBound
  where
    inner iter | Just x <- setNext set iter = x : inner x
        | otherwise = []
