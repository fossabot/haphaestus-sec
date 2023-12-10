-- Here to break recursive imports...
module Graphics.Text.Font.Choose.FontSet.API where

import Graphics.Text.Font.Choose.FontSet
import Graphics.Text.Font.Choose.Pattern
import Graphics.Text.Font.Choose.Config
import Graphics.Text.Font.Choose.ObjectSet
import Graphics.Text.Font.Choose.CharSet
import Graphics.Text.Font.Choose.Result (Word8, throwPtr)

import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import System.IO.Unsafe (unsafePerformIO)

-- | Selects fonts matching pattern from sets,
-- creates patterns from those fonts containing only the objects in object_set
-- and returns the set of unique such patterns.
fontSetList :: Config -> [FontSet] -> Pattern -> ObjectSet -> FontSet
fontSetList config fontss pattern objs = unsafePerformIO $ withForeignPtr config $ \config' ->
    withFontSets fontss $ \fontss' n -> withPattern pattern $ \pattern' ->
        withObjectSet objs $ \objs' ->
            thawFontSet_ $ fcFontSetList config' fontss' n pattern' objs'
-- | Variant of `fontSetList` operating upon register default `Config`.
fontSetList' :: [FontSet] -> Pattern -> ObjectSet -> FontSet
fontSetList' fontss pattern objs = unsafePerformIO $ withFontSets fontss $ \fontss' n ->
    withPattern pattern $ \pattern' -> withObjectSet objs $ \objs' ->
        thawFontSet_ $ fcFontSetList nullPtr fontss' n pattern' objs'
foreign import ccall "FcFontSetList" fcFontSetList ::
    Config_ -> Ptr FontSet_ -> Int -> Pattern_ -> ObjectSet_ -> IO FontSet_

-- | Finds the font in sets most closely matching pattern
-- and returns the result of `fontRenderPrepare` for that font and
-- the provided pattern. This function should be called only after
-- `configSubstitute` and `defaultSubstitute` have been called for pattern;
-- otherwise the results will not be correct.
fontSetMatch :: Config -> [FontSet] -> Pattern -> Maybe Pattern
fontSetMatch config fontss pattern = unsafePerformIO $ withForeignPtr config $ \config' ->
    withFontSets fontss $ \fontss' n -> withPattern pattern $ \pattern' -> 
        alloca $ \res' -> do
            ret <- fcFontSetMatch config' fontss' n pattern' res'
            throwPtr res' $ thawPattern_ $ pure ret
-- | Variant of `fontSetMatch` operating upon registered default `Config`.
fontSetMatch' :: [FontSet] -> Pattern -> Maybe Pattern
fontSetMatch' fontss pattern = unsafePerformIO $ withFontSets fontss $ \fontss' n ->
    withPattern pattern $ \pattern' -> alloca $ \res' -> do
        ret <- fcFontSetMatch nullPtr fontss' n pattern' res'
        throwPtr res' $ thawPattern_ $ pure ret
foreign import ccall "FcFontSetMatch" fcFontSetMatch ::
    Config_ -> Ptr FontSet_ -> Int -> Pattern_ -> Ptr Word8 -> IO Pattern_

-- | Returns the list of fonts from sets sorted by closeness to pattern.
-- If trim is `True`, elements in the list which don't include Unicode coverage
-- not provided by earlier elements in the list are elided.
-- The union of Unicode coverage of all of the fonts is returned in csp.
-- This function should be called only after `configSubstitute` and
-- `defaultSubstitute` have been called for p;
-- otherwise the results will not be correct.
-- The returned FcFontSet references `Pattern` structures which may be shared by
-- the return value from multiple `fontSort` calls, applications cannot modify
-- these patterns. Instead, they should be passed, along with pattern to
-- `fontRenderPrepare` which combines them into a complete pattern.
fontSetSort :: Config -> [FontSet] -> Pattern -> Bool -> Maybe (FontSet, CharSet)
fontSetSort config fontss pattern trim = unsafePerformIO $
    withForeignPtr config $ \config' -> withFontSets fontss $ \fontss' n ->
        withPattern pattern $ \pattern' -> alloca $ \csp' -> alloca $ \res' -> do
            ret' <- fcFontSetSort config' fontss' n pattern' trim csp' res'
            throwPtr res' $ do
                x <- thawFontSet_ $ pure ret'
                y <- thawCharSet' csp'
                return (x, y)
-- | Variant of `fontSetSort` operating upon registered default `Config`.
fontSetSort' :: [FontSet] -> Pattern -> Bool -> Maybe (FontSet, CharSet)
fontSetSort' fontss pattern trim = unsafePerformIO $
    withFontSets fontss $ \fontss' n -> withPattern pattern $ \pattern' ->
        alloca $ \csp' -> alloca $ \res' -> do
            ret' <- fcFontSetSort nullPtr fontss' n pattern' trim csp' res'
            throwPtr res' $ do
                x <- thawFontSet_ $ pure ret'
                y <- thawCharSet' csp'
                return (x, y)
foreign import ccall "FcFontSetSort" fcFontSetSort :: Config_ -> Ptr FontSet_
    -> Int -> Pattern_ -> Bool -> Ptr CharSet_ -> Ptr Word8 -> IO FontSet_
