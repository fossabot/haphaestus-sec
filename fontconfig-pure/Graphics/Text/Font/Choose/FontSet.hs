{-# LANGUAGE OverloadedStrings #-}
module Graphics.Text.Font.Choose.FontSet where

import Graphics.Text.Font.Choose.Pattern
import Graphics.Text.Font.Choose.Result (throwFalse, throwNull)

import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (pokeElemOff, sizeOf)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray, allocaArray)
import Control.Monad (forM)
import Control.Exception (bracket)
import System.IO.Unsafe (unsafeInterleaveIO)

-- For CSS bindings
import Stylist.Parse (StyleSheet(..), parseProperties)
import Data.CSS.Syntax.Tokens (Token(..), serialize)
import Data.Text (unpack, Text)
import Graphics.Text.Font.Choose.Range (iRange)
import Graphics.Text.Font.Choose.CharSet (parseCharSet)
import Data.List (intercalate)

-- | An `FontSet` contains a list of `Pattern`s.
-- Internally fontconfig uses this data structure to hold sets of fonts.
-- Externally, fontconfig returns the results of listing fonts in this format.
type FontSet = [Pattern]

------
--- Low-level
------
data FontSet'
type FontSet_ = Ptr FontSet'

withNewFontSet :: (FontSet_ -> IO a) -> IO a
withNewFontSet = bracket fcFontSetCreate fcFontSetDestroy
foreign import ccall "FcFontSetCreate" fcFontSetCreate :: IO FontSet_
foreign import ccall "FcFontSetDestroy" fcFontSetDestroy :: FontSet_ -> IO ()

withFontSet :: FontSet -> (FontSet_ -> IO a) -> IO a
withFontSet fonts cb = withNewFontSet $ \fonts' -> do
    forM fonts $ \font -> do
        font' <- patternAsPointer font
        throwFalse <$> fcFontSetAdd fonts' font'
    cb fonts'
foreign import ccall "FcFontSetAdd" fcFontSetAdd :: FontSet_ -> Pattern_ -> IO Bool

withFontSets :: [FontSet] -> (Ptr FontSet_ -> Int -> IO a) -> IO a
withFontSets fontss cb = let n = length fontss in
    allocaArray n $ \fontss' ->
        withFontSets' fontss 0 fontss' $ cb fontss' n
withFontSets' :: [FontSet] -> Int -> Ptr FontSet_ -> IO a -> IO a
withFontSets' [] _ _ cb = cb
withFontSets' (fonts:fontss) i fontss' cb = withFontSet fonts $ \fonts' -> do
    pokeElemOff fontss' i fonts'
    withFontSets' fontss (succ i) fontss' cb

thawFontSet :: FontSet_ -> IO FontSet
thawFontSet fonts' = do
    n <- get_fontSet_nfont fonts'
    if n == 0 then return []
    else
        forM [0..pred n] (\i -> thawPattern' =<< get_fontSet_font fonts' i)
  where
    thawPattern' pat = do
        fcPatternReference pat
        unsafeInterleaveIO $ thawPattern pat
foreign import ccall "get_fontSet_nfont" get_fontSet_nfont :: FontSet_ -> IO Int
foreign import ccall "get_fontSet_font" get_fontSet_font :: FontSet_ -> Int -> IO Pattern_

thawFontSet_ :: IO FontSet_ -> IO FontSet
thawFontSet_ cb = bracket (throwNull <$> cb) fcFontSetDestroy thawFontSet

------
--- CSS Bindings
------

-- | `StyleSheet` wrapper to parse @font-face rules.
data FontFaceParser a = FontFaceParser { cssFonts :: FontSet, cssInner :: a}

parseFontFaceSrc (Function "local":Ident name:RightParen:Comma:rest) =
    ("local:" ++ unpack name):parseFontFaceSrc rest
parseFontFaceSrc (Function "local":String name:RightParen:Comma:rest) =
    ("local:" ++ unpack name):parseFontFaceSrc rest
parseFontFaceSrc (Function "local":Ident name:RightParen:[]) = ["local:" ++ unpack name]
parseFontFaceSrc (Function "local":String name:RightParen:[]) = ["local:" ++ unpack name]

parseFontFaceSrc (Url link:toks)
    | Comma:rest <- skipMeta toks = unpack link:parseFontFaceSrc rest
    | [] <- skipMeta toks = [unpack link]
    | otherwise = [""] -- Error indicator!
  where
    skipMeta (Function "format":Ident _:RightParen:rest) = skipMeta rest
    skipMeta (Function "format":String _:RightParen:rest) = skipMeta rest
    skipMeta (Function "tech":Ident _:RightParen:rest) = skipMeta rest
    skipMeta (Function "tech":String _:RightParen:rest) = skipMeta rest
    skipMeta toks = toks

parseFontFaceSrc _ = [""]

properties2font :: [(Text, [Token])] -> Pattern
properties2font (("font-family", [String font]):props) =
    setValue "family" Strong (unpack font) $ properties2font props
properties2font (("font-family", [Ident font]):props) =
    setValue "family" Strong (unpack font) $ properties2font props

properties2font (("font-stretch", [tok]):props) | Just x <- parseFontStretch tok =
    setValue "width" Strong x $ properties2font props
properties2font (("font-stretch", [start, end]):props)
    | Just x <- parseFontStretch start, Just y <- parseFontStretch end =
        setValue "width" Strong (x `iRange` y) $ properties2font props

properties2font (("font-weight", [tok]):props) | Just x <- parseFontWeight tok =
    setValue "width" Strong x $ properties2font props
properties2font (("font-weight", [start, end]):props)
    | Just x <- parseFontStretch start, Just y <- parseFontStretch end =
        setValue "weight" Strong (x `iRange` y) $ properties2font props

properties2font (("font-feature-settings", toks):props)
    | (features, True, []) <- parseFontFeatures toks =
        setValue "fontfeatures" Strong (intercalate "," $ map fst features) $
            properties2font props

properties2font (("font-variation-settings", toks):props)
    | (_, True, []) <- parseFontVars toks =
        setValue "variable" Strong True $ properties2font props

properties2font (("unicode-range", toks):props)
    | Just chars <- parseCharSet $ unpack $ serialize toks =
        setValue "charset" Strong chars $ properties2font props

-- Ignoring metadata & trusting in FreeType's broad support for fonts.
properties2font (("src", toks):props)
    | fonts@(_:_) <- parseFontFaceSrc toks, "" `notElem` fonts =
        setValue "web-src" Strong (intercalate "\t" fonts) $ properties2font props

properties2font (_:props) = properties2font props
properties2font [] = []

instance StyleSheet a => StyleSheet (FontFaceParser a) where
    setPriorities v (FontFaceParser x self) = FontFaceParser x $ setPriorities v self
    addRule (FontFaceParser x self) rule = FontFaceParser x $ addRule self rule

    addAtRule (FontFaceParser fonts self) "font-face" toks =
        let ((props, _), toks') = parseProperties toks
        in (FontFaceParser (properties2font props:fonts) self, toks')
    addAtRule (FontFaceParser x self) key toks =
        let (a, b) = addAtRule self key toks in (FontFaceParser x a, b)
