{-# LANGUAGE MagicHash, UnliftedFFITypes, DeriveGeneric #-}
module Data.Text.Glyphize.Buffer where

import qualified Data.Text.Internal.Lazy as Lazy
import qualified Data.Text.Lazy as Lazy (Text, pack)
import qualified Data.Text.Internal as Txt
import Data.Char (toUpper, toLower)
import Control.Monad (forM)
import Control.Exception (bracket)
import Control.DeepSeq (NFData)

import Data.Text.Glyphize.Oom (throwFalse, throwNull)
import Data.Text.Glyphize.Array (iterateLazy, noCache)

import qualified Data.Text.Array as A
import GHC.Exts (ByteArray#, sizeofByteArray#, Int#)
import Data.Word (Word32)
import Data.Int (Int32)
import Data.Bits ((.|.), (.&.), shiftR, shiftL, testBit)

import System.IO.Unsafe (unsafePerformIO)

import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.Storable (Storable(..))
import GHC.Generics (Generic(..))
import Foreign.Storable.Generic (GStorable(..))

------
--- Public Datastructures
------

-- | Text to be shaped or the resulting glyphs, for which language/script/direction/etc.
data Buffer = Buffer {
    text :: Lazy.Text,
    -- ^ The Unicode text, in visual order, for HarfBuzz to convert into glyphs.
    -- See https://hackage.haskell.org/package/text-2.0.1/docs/Data-Text-Internal-Lazy.html#t:Text for details.
    contentType :: Maybe ContentType,
    -- ^ What the bytes of the ByteString contents represents,
    -- namely unicode characters (before shaping) or glyphs (result of shaping).
    -- Typically callers should leave this as `Just ContentTypeUnicode`.
    direction :: Maybe Direction,
    -- ^ The text flow direction of the buffer.
    -- No shaping can happen without setting buffer direction, and it controls
    -- the visual direction for the output glyphs; for RTL direction the glyphs
    -- will be reversed. Many layout features depend on the proper setting of
    -- the direction, for example, reversing RTL text before shaping,
    -- then shaping with LTR direction is not the same as keeping the text in
    -- logical order and shaping with RTL direction.
    script :: Maybe String,
    -- ^ Script is crucial for choosing the proper shaping behaviour for scripts
    -- that require it (e.g. Arabic) and the which OpenType features defined in
    -- the font to be applied.
    language :: Maybe String,
    -- ^ Languages are crucial for selecting which OpenType feature to apply to
    -- the buffer which can result in applying language-specific behaviour.
    -- Languages are orthogonal to the scripts, and though they are related,
    -- they are different concepts and should not be confused with each other.
    beginsText :: Bool,
    -- ^ special handling of the beginning of text paragraph can be applied to
    -- this buffer. Should usually be set, unless you are passing to the buffer
    -- only part of the text without the full context.
    endsText :: Bool,
    -- ^ special handling of the end of text paragraph can be applied to this buffer.
    preserveDefaultIgnorables :: Bool,
    -- ^ character with Default_Ignorable Unicode property should use the
    -- corresponding glyph from the font, instead of hiding them (done by
    -- replacing them with the space glyph and zeroing the advance width.)
    -- Takes precedance over `removeDefaultIgnorables`.
    removeDefaultIgnorables :: Bool,
    -- ^ character with Default_Ignorable Unicode property should be removed
    -- from glyph string instead of hiding them (done by replacing them with
    -- the space glyph and zeroing the advance width.)
    don'tInsertDottedCircle :: Bool,
    -- ^ a dotted circle should not be inserted in the rendering of incorrect
    -- character sequences (such as <0905 093E>).
    clusterLevel :: ClusterLevel,
    -- ^ dictates one aspect of how HarfBuzz will treat non-base characters
    -- during shaping.
    invisibleGlyph :: Char,
    -- ^ The glyph number that replaces invisible characters in the
    -- shaping result. If set to zero (default), the glyph for the U+0020
    -- SPACE character is used. Otherwise, this value is used verbatim.
    replacementCodepoint :: Char,
    -- ^ the glyph number that replaces invalid entries for a given encoding
    -- when adding text to buffer.
    notFoundGlyph :: Char
    -- ^ the glyph number that replaces replaces characters not found in the font.
  } deriving (Eq, Show, Read, Ord)

-- | Whether the given text is Unicode or font-specific "glyphs".
data ContentType = ContentTypeUnicode | ContentTypeGlyphs deriving (Eq, Show, Read, Ord)
-- | Defines how fine the groupings represented by `GlyphInfo`'s `cluster` property are.`
data ClusterLevel = ClusterMonotoneGraphemes | ClusterMonotoneChars | ClusterChars
    deriving (Eq, Show, Read, Ord)

-- | An empty buffer with sensible default properties.
defaultBuffer = Buffer {
        text = Lazy.empty,
        contentType = Just ContentTypeUnicode,
        direction = Nothing,
        script = Nothing,
        language = Nothing,
        beginsText = True,
        endsText = True,
        preserveDefaultIgnorables = False,
        removeDefaultIgnorables = False,
        don'tInsertDottedCircle = False,
        clusterLevel = ClusterMonotoneGraphemes,
        invisibleGlyph = ' ',
        replacementCodepoint = '\xFFFD',
        notFoundGlyph = '\0'
    }

------
--- Directions
------

-- | The direction of a text segment or buffer.
data Direction = DirLTR | DirRTL | DirTTB | DirBTT deriving (Eq, Show, Read, Ord)
-- | Converts a string to an `Direction`.
-- Matching is loose and applies only to the first letter. For examples, 
-- "LTR" and "left-to-right" will both return HB_DIRECTION_LTR.
dirFromStr ('L':_) = Just DirLTR
dirFromStr ('l':_) = Just DirLTR
dirFromStr ('R':_) = Just DirRTL
dirFromStr ('r':_) = Just DirRTL
dirFromStr ('T':_) = Just DirTTB
dirFromStr ('t':_) = Just DirTTB
dirFromStr ('B':_) = Just DirBTT
dirFromStr ('b':_) = Just DirBTT
dirFromStr _ = Nothing
-- | Converts an `Direction` to a string.
dirToStr DirLTR = "ltr"
dirToStr DirRTL = "rtl"
dirToStr DirTTB = "ttb"
dirToStr DirBTT = "btt"
-- | Reverses a text direction.
dirReverse DirLTR = DirRTL
dirReverse DirRTL = DirLTR
dirReverse DirTTB = DirBTT
dirReverse DirBTT = DirTTB
-- | Tests whether a text direction moves backward
-- (from right to left, or from bottom to top).
dirBackward dir = dir `Prelude.elem` [DirRTL, DirBTT]
-- | Tests whether a text direction moves forward
-- (from left to right, or from top to bottom).
dirForward dir = dir `Prelude.elem` [DirLTR, DirTTB]
-- | Tests whether a text direction is horizontal.
dirHorizontal dir = dir `Prelude.elem` [DirLTR, DirRTL]
-- | Tests whether a text direction is vertical.
dirVertical dir = dir `Prelude.elem` [DirTTB, DirBTT]

-- | Converts a `Direction` to C encoding.
dir2int Nothing = 0
dir2int (Just DirLTR) = 4
dir2int (Just DirRTL) = 5
dir2int (Just DirTTB) = 6
dir2int (Just DirBTT) = 7
-- | Sets `direction` property on C `Buffer'` struct.
foreign import ccall "hb_buffer_set_direction" hb_buffer_set_direction
    :: Buffer' -> Int -> IO ()

-- | Converts a `Direction` from C encoding.
int2dir 4 = Just DirLTR
int2dir 5 = Just DirRTL
int2dir 6 = Just DirTTB
int2dir 7 = Just DirBTT
int2dir _ = Nothing

-- | Fetches the `Direction` of a script when it is set horizontally.
-- All right-to-left scripts will return `DirRTL`.
-- All left-to-right scripts will return `DirLTR`.
-- Scripts that can be written either horizontally or vertically will return `Nothing`.
-- Unknown scripts will return `DirLTR`.
scriptHorizontalDir :: String -> Maybe Direction
scriptHorizontalDir = int2dir . hb_script_get_horizontal_direction . script_from_string
foreign import ccall "hb_script_get_horizontal_direction" hb_script_get_horizontal_direction
    :: Word32 -> Int

------
--- Locales
------
data Language'
-- | Represents a natural written language.
-- Corresponds to a BCP47 language tag.
type Language = Ptr Language'
-- | Fetch the default language from current locale.
-- NOTE that the first time this function is called, it calls (C code)
-- "setlocale (LC_CTYPE, nullptr)" to fetch current locale.
-- The underlying setlocale function is, in many implementations, NOT threadsafe.
-- To avoid problems, call this function once before multiple threads can call it.
-- This function may be used to fill in missing fields on a `Buffer`.
languageDefault :: IO String
languageDefault = hb_language_get_default >>= hb_language_to_string >>= peekCString
foreign import ccall "hb_language_to_string" hb_language_to_string :: Language -> IO CString
foreign import ccall "hb_language_get_default" hb_language_get_default :: IO Language

-- | Converts a `String` representing a BCP 47 language tag to the corresponding `Language`.
hb_language_from_string :: String -> IO Language
hb_language_from_string str =
    withCString str $ \str' -> hb_language_from_string' str' (-1)
foreign import ccall "hb_language_from_string" hb_language_from_string'
    :: CString -> Int -> IO Language

{-
-- | Check whether a second language tag is the same or a more specific version
-- of the provided language tag.
-- For example, "fa_IR.utf8" is a more specific tag for "fa" or for "fa_IR".
languageMatches :: String -> String -> Bool
languageMatches lang specific = unsafePerformIO $ do
    lang' <- hb_language_from_string lang
    specific' <- hb_language_from_string specific
    hb_language_matches lang' specific'
foreign import ccall "hb_language_matches" hb_language_matches :: Language -> Language -> IO Bool-}

------
--- FFI Support
------

-- | Directly corresponds to "hb_buffer_t".
data Buffer''
type Buffer' = Ptr Buffer''

-- | Temporarily allocates a `Buffer'`
withNewBuffer :: (Buffer' -> IO a) -> IO a
withNewBuffer cb = bracket hb_buffer_create hb_buffer_destroy cb
foreign import ccall "hb_buffer_create" hb_buffer_create :: IO Buffer'
foreign import ccall "hb_buffer_destroy" hb_buffer_destroy :: Buffer' -> IO ()

-- | Decodes given lazy `Text` into given `Buffer'`.
-- Should be valid Unicode data.
-- Captures a few trailing & preceding chars when possible to give additional
-- context to the shaping.
bufferWithText _ Lazy.Empty cb = cb
bufferWithText buffer txt@(Lazy.Chunk (Txt.Text (A.ByteArray arr) offset length) txts) cb = do
    hb_buffer_add_utf8 buffer arr (sizeofByteArray# arr) (toEnum offset) length
    bufferWithText buffer txts cb
foreign import ccall "hb_buffer_add_utf8" hb_buffer_add_utf8
    :: Buffer' -> ByteArray# -> Int# -> Word -> Int -> IO ()

-- | Converts initial char to uppercase & all others to lowercase.
-- Internal utility for reimplementation `script_from_string`.
titlecase :: String -> String
titlecase "" = ""
titlecase (c:cs) = toUpper c : Prelude.map toLower cs
-- | Converts a string str representing an ISO 15924 script tag to a corresponding "tag" `Word32`.
script_from_string :: String -> Word32
script_from_string str = tag_from_string $ case titlecase str of
    'Q':'a':'a':'i':_ -> "Zinh"
    'Q':'a':'a':'c':_ -> "Copt"

    'A':'r':'a':'n':_ -> "Arab"
    'C':'y':'r':'s':_ -> "Cyrl"
    'G':'e':'o':'k':_ -> "Geor"
    'H':'a':'n':'s':_ -> "Hani"
    'H':'a':'n':'t':_ -> "Hani"
    'J':'a':'m':'o':_ -> "Hang"
    'L':'a':'t':'f':_ -> "Latn"
    'L':'a':'t':'g':_ -> "Latn"
    'S':'y':'r':'e':_ -> "Syrc"
    'S':'y':'r':'j':_ -> "Syrc"
    'S':'y':'r':'n':_ -> "Syrc"
    x -> x
-- | Converts a `String` into a "tag" `Word32`. Valid tags are 4 `Char`s.
-- Shorter input `String`s will be padded with spaces.
-- Longer input strings will be truncated.
tag_from_string :: String -> Word32
tag_from_string str = case str ++ Prelude.repeat ' ' of
    c1:c2:c3:c4:_ -> Prelude.foldl (.|.) 0 [
        shiftL (c2w c1 .&. 0x7f) 24,
        shiftL (c2w c2 .&. 0x7f) 16,
        shiftL (c2w c3 .&. 0x7f) 8,
        shiftL (c2w c4 .&. 0x7f) 0
      ]
    _ -> 0
-- | Converts a "tag" `Word32` into a 4 `Char` `String`.
tag_to_string :: Word32 -> String
tag_to_string tag = [
    w2c (shiftR tag 24 .&. 0x7f),
    w2c (shiftR tag 16 .&. 0x7f),
    w2c (shiftR tag 8 .&. 0x7f),
    w2c (shiftR tag 0 .&. 0x7f)
  ]

c2w :: Char -> Word32
c2w = toEnum . fromEnum
w2c :: Word32 -> Char
w2c = toEnum . fromEnum

------
--- Haskell-to-C conversion
------

-- | Temporarily allocates a `Buffer'` corresponding to the given `Buffer`
-- to be processed entirely within the given callback.
withBuffer :: Buffer -> (Buffer' -> IO a) -> IO a
withBuffer buf cb = withNewBuffer $ \buf' -> bufferWithText buf' (text buf) $ do
    hb_buffer_set_content_type buf' $ case contentType buf of
        Nothing -> 0
        Just ContentTypeUnicode -> 1
        Just ContentTypeGlyphs -> 2
    hb_buffer_set_direction buf' $ dir2int $ direction buf
    case script buf of
        Just script' -> hb_buffer_set_script buf' $ script_from_string script'
        Nothing -> return ()
    case language buf of
        Just lang' -> hb_buffer_set_language buf' =<< hb_language_from_string lang'
        Nothing -> return ()
    hb_buffer_set_flags buf' $ Prelude.foldl (.|.) 0 [
        if beginsText buf then 1 else 0,
        if endsText buf then 2 else 0,
        if preserveDefaultIgnorables buf then 4 else 0,
        if removeDefaultIgnorables buf then 8 else 0,
        if don'tInsertDottedCircle buf then 16 else 0
      ]
    hb_buffer_set_cluster_level buf' $ case clusterLevel buf of
        ClusterMonotoneGraphemes -> 0
        ClusterMonotoneChars -> 1
        ClusterChars -> 2
    hb_buffer_set_invisible_glyph buf' $ c2w $ invisibleGlyph buf
    hb_buffer_set_replacement_codepoint buf' $ c2w $ replacementCodepoint buf
    hb_buffer_set_not_found_glyph buf' $ c2w $ notFoundGlyph buf
    case (contentType buf, direction buf, script buf, language buf) of
        (Just ContentTypeUnicode, Nothing, _, _) -> hb_buffer_guess_segment_properties buf'
        (Just ContentTypeUnicode, _, Nothing, _) -> hb_buffer_guess_segment_properties buf'
        (Just ContentTypeUnicode, _, _, Nothing) -> hb_buffer_guess_segment_properties buf'
        _ -> return ()

    throwFalse $ hb_buffer_allocation_successful buf'
    cb buf'
foreign import ccall "hb_buffer_set_content_type" hb_buffer_set_content_type
    :: Buffer' -> Int -> IO ()
foreign import ccall "hb_buffer_set_script" hb_buffer_set_script
    :: Buffer' -> Word32 -> IO ()
foreign import ccall "hb_buffer_set_language" hb_buffer_set_language
    :: Buffer' -> Language -> IO ()
foreign import ccall "hb_buffer_set_flags" hb_buffer_set_flags :: Buffer' -> Int -> IO ()
foreign import ccall "hb_buffer_set_cluster_level" hb_buffer_set_cluster_level
    :: Buffer' -> Int -> IO ()
foreign import ccall "hb_buffer_set_invisible_glyph" hb_buffer_set_invisible_glyph
    :: Buffer' -> Word32 -> IO ()
foreign import ccall "hb_buffer_set_replacement_codepoint" hb_buffer_set_replacement_codepoint
    :: Buffer' -> Word32 -> IO ()
foreign import ccall "hb_buffer_set_not_found_glyph" hb_buffer_set_not_found_glyph
    :: Buffer' -> Word32 -> IO ()
foreign import ccall "hb_buffer_guess_segment_properties" hb_buffer_guess_segment_properties
    :: Buffer' -> IO ()
foreign import ccall "hb_buffer_allocation_successful" hb_buffer_allocation_successful
    :: Buffer' -> IO Bool

------
--- C-to-Haskell conversion
------

-- | Holds information about the glyphs & their relation to input text.
data GlyphInfo = GlyphInfo {
    codepoint :: Word32,
    -- ^ Glyph index (or unicode codepoint)
    cluster :: Word32,
    -- ^ The index of the character in the original text that corresponds to
    -- this `GlyphInfo`. More than one `GlyphInfo` may have the same `cluster`
    -- value if they resulted from the same character, & when more than one
    -- character gets merged into the same glyph `GlyphInfo` will have the
    -- smallest cluster value of them.
    -- By default some characters are merged into the same cluster even when
    -- they are seperate glyphs, `Buffer`'s `clusterLevel` property allows
    -- selecting more fine grained cluster handling.
    unsafeToBreak :: Bool,
    -- ^ Indicates that if input text is broken at the beginning of the cluster
    -- this glyph is part of, then both sides need to be re-shaped,
    -- as the result might be different.
    -- On the flip side, it means that when this flag is not present,
    -- then it is safe to break the glyph-run at the beginning of this cluster,
    -- and the two sides will represent the exact same result one would get
    -- if breaking input text at the beginning of this cluster and shaping
    -- the two sides separately. This can be used to optimize paragraph layout,
    -- by avoiding re-shaping of each line after line-breaking.
    unsafeToConcat :: Bool,
    -- ^ Indicates that if input text is changed on one side of the beginning
    -- of the cluster this glyph is part of, then the shaping results for
    -- the other side might change.
    -- Note that the absence of this flag will NOT by itself mean that
    -- it IS safe to concat text. Only two pieces of text both of which
    -- clear of this flag can be concatenated safely.
    -- See https://harfbuzz.github.io/harfbuzz-hb-buffer.html#HB_GLYPH_FLAG_UNSAFE_TO_CONCAT
    -- for more details.
    safeToInsertTatweel :: Bool
    -- ^ In scripts that use elongation (Arabic, Mongolian, Syriac, etc.),
    -- this flag signifies that it is safe to insert a U+0640 TATWEEL character
    -- before this cluster for elongation.
    -- This flag does not determine the script-specific elongation places,
    -- but only when it is safe to do the elongation without interrupting text shaping.
} deriving (Show, Read, Eq, Generic)
instance NFData GlyphInfo
-- | Decodes multiple `GlyphInfo`s from a dereferenced `Word32` list according to
-- Harfbuzz's ABI.
decodeInfos :: [Word32] -> [GlyphInfo]
decodeInfos (codepoint':cluster':mask:_:_:rest) =
    GlyphInfo codepoint' cluster' (mask `testBit` 1) (mask `testBit` 2)
        (mask `testBit` 3):decodeInfos rest
decodeInfos _ = []
-- | Decodes `Buffer'`'s glyph information array.
glyphInfos buf' = do
    arr <- throwNull $ hb_buffer_get_glyph_infos buf' nullPtr
    length <- hb_buffer_get_length buf'
    words <- iterateLazy arr (fromEnum length * 5)
    return $ noCache decodeInfos words
foreign import ccall "hb_buffer_get_glyph_infos" hb_buffer_get_glyph_infos
    :: Buffer' -> Ptr Word -> IO (Ptr Word32)
foreign import ccall "hb_buffer_get_length" hb_buffer_get_length :: Buffer' -> IO Word
-- NOTE: The array returned from FFI is valid as long as the buffer is.

-- | Holds positions of the glyph in both horizontal & vertical directions.
-- All positions are relative to current point.
data GlyphPos = GlyphPos {
    x_advance :: Int32,
    -- ^ How much the line advances after drawing this glyph when setting text
    -- in horizontal direction.
    y_advance :: Int32,
    -- ^ How much the line advances after drawing this glyph when setting text
    -- in vertical direction.
    x_offset :: Int32,
    -- ^ How much the glyph moves on the X-axis before drawing it, this should
    -- not effect how much the line advances.
    y_offset :: Int32
    -- ^ How much the glyph moves on the Y-axis before drawing it, this should
    -- not effect how much the line advances.
} deriving (Show, Read, Eq, Generic)
instance NFData GlyphPos
-- | Decode multiple `GlyphPos`s from a dereferenced list according to
-- Harfbuzz's ABI.
decodePositions (x_advance':y_advance':x_offset':y_offset':_:rest) =
    GlyphPos x_advance' y_advance' x_offset' y_offset':decodePositions rest
decodePositions _ = []
-- | Decodes `Buffer'`'s glyph position array.
-- If buffer did not have positions before, they will be initialized to zeros.'
glyphsPos buf' = do
    arr <- throwNull $ hb_buffer_get_glyph_positions buf' nullPtr
    length <- hb_buffer_get_length buf'
    words <- iterateLazy arr (fromEnum length * 5)
    return $ noCache decodePositions words
foreign import ccall "hb_buffer_get_glyph_positions" hb_buffer_get_glyph_positions
    :: Buffer' -> Ptr Word -> IO (Ptr Int32)
-- NOTE: The array returned from FFI is valid as long as the buffer is.

-- | Decodes a `Buffer'` back to corresponding pure-functional `Buffer`.
thawBuffer :: Buffer' -> IO Buffer
thawBuffer buf' = do
    glyphInfos' <- glyphInfos buf'
    contentType' <- hb_buffer_get_content_type buf'
    direction' <- hb_buffer_get_direction buf'
    script' <- hb_buffer_get_script buf'
    language'' <- hb_buffer_get_language buf'
    language' <- peekCString language''
    flags' <- hb_buffer_get_flags buf'
    clusterLevel' <- hb_buffer_get_cluster_level buf'
    invisibleGlyph' <- hb_buffer_get_invisible_glyph buf'
    replacementCodepoint' <- hb_buffer_get_replacement_codepoint buf'
    return defaultBuffer {
        text = Lazy.pack $ Prelude.map (w2c . codepoint) glyphInfos',
        contentType = case contentType' of
            1 -> Just ContentTypeUnicode
            2 -> Just ContentTypeGlyphs
            _ -> Nothing,
        direction = int2dir direction',
        script = Just $ tag_to_string script',
        language = Just language',
        beginsText = testBit flags' 0, endsText = testBit flags' 1,
        preserveDefaultIgnorables = testBit flags' 2,
        removeDefaultIgnorables = testBit flags' 3,
        don'tInsertDottedCircle = testBit flags' 4,
        clusterLevel = case clusterLevel' of
            1 -> ClusterMonotoneChars
            2 -> ClusterChars
            _ -> ClusterMonotoneGraphemes,
        invisibleGlyph = w2c invisibleGlyph',
        replacementCodepoint = w2c replacementCodepoint'
    }
foreign import ccall "hb_buffer_get_content_type" hb_buffer_get_content_type
    :: Buffer' -> IO Int
foreign import ccall "hb_buffer_get_direction" hb_buffer_get_direction :: Buffer' -> IO Int
foreign import ccall "hb_buffer_get_script" hb_buffer_get_script :: Buffer' -> IO Word32
foreign import ccall "hb_buffer_get_language" hb_buffer_get_language :: Buffer' -> IO CString
foreign import ccall "hb_buffer_get_flags" hb_buffer_get_flags :: Buffer' -> IO Int
foreign import ccall "hb_buffer_get_cluster_level" hb_buffer_get_cluster_level
    :: Buffer' -> IO Int
foreign import ccall "hb_buffer_get_invisible_glyph" hb_buffer_get_invisible_glyph
    :: Buffer' -> IO Word32
foreign import ccall "hb_buffer_get_replacement_codepoint" hb_buffer_get_replacement_codepoint
    :: Buffer' -> IO Word32
