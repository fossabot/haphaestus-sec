{-# LANGUAGE DeriveGeneric, OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Graphics.Text.Font.Choose.Pattern (Pattern(..), Binding(..), equalSubset,
    normalizePattern, filter, defaultSubstitute, nameParse, nameUnparse, format,
    Pattern_, withPattern, thawPattern, thawPattern_, patternAsPointer,
    fcPatternReference,

    setValue, setValues, unset, getValues, getValues', getValue, getValue', getValue0,
    parseFontFamily, parseFontFeatures, parseFontVars, parseLength,
    parseFontStretch, parseFontWeight) where

import Prelude hiding (filter)
import Data.List (nub)

import Graphics.Text.Font.Choose.Value
import Graphics.Text.Font.Choose.ObjectSet (ObjectSet, ObjectSet_, withObjectSet)
import Data.Hashable (Hashable(..))
import GHC.Generics (Generic)
import Graphics.Text.Font.Choose.Result (throwFalse, throwNull, throwInt)

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr,
        newForeignPtr, withForeignPtr, mallocForeignPtrBytes)
import Foreign.Marshal.Alloc (alloca, allocaBytes, free)
import Foreign.Storable (Storable(..))
import Foreign.C.String (CString, withCString, peekCString)
import Debug.Trace (trace) -- For reporting internal errors!
import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO)

import Control.Monad (forM, join)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Control.Exception (bracket)

-- Imported for CSS bindings
import Data.CSS.Syntax.Tokens (Token(..), NumericValue(..))
import Data.Text (unpack, Text)
import Stylist (PropertyParser(..))
import Data.Scientific (toRealFloat)
import Data.List (intercalate)
import Graphics.Text.Font.Choose.Weight (weightFromOpenType)

-- | An `Pattern`` holds a set of names with associated value lists;
-- each name refers to a property of a font.
-- `Pattern`s are used as inputs to the matching code as well as
-- holding information about specific fonts.
-- Each property can hold one or more values;
-- conventionally all of the same type, although the interface doesn't demand that.
type Pattern = [(String, [(Binding, Value)])]
-- | How important is it to match this property of the Pattern.
data Binding = Strong | Weak | Same deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable Binding where
    hash Strong = 0
    hash Weak = 1
    hash Same = 2

-- | Replaces the values under the given "key" in given "pattern"
-- with given "binding" & "value".
setValue :: ToValue x => String -> Binding -> x -> Pattern -> Pattern
setValue key b value pat = (key, [(b, toValue value)]):unset key pat
-- | Replaces the values under the given "key" in given "pattern"
-- with given "binding" & "value"s.
setValues :: ToValue x => String -> Binding -> [x] -> Pattern -> Pattern
setValues key b values pat = (key, [(b, toValue v) | v <- values]):unset key pat
-- | Retrieves all values in the given pattern under a given key.
getValues :: String -> Pattern -> [Value]
getValues key pat | Just ret <- lookup key pat = map snd ret
    | otherwise = []
-- | Retrieves all values under a given key & coerces to desired `Maybe` type.
getValues' key pat = mapMaybe fromValue $ getValues key pat
-- | Retrieves first value in the given pattern under a given key.
getValue :: String -> Pattern -> Value
getValue key pat | Just ((_, ret):_) <- lookup key pat = ret
    | otherwise = ValueVoid
-- Retrieves first value under a given key & coerces to desired `Maybe` type.
getValue' :: ToValue x => String -> Pattern -> Maybe x
getValue' key pat = fromValue $ getValue key pat
-- Retrieves first value under a given key & coerces to desired type throw
-- or throw `ErrTypeMismatch`
getValue0 :: ToValue x => String -> Pattern -> x
getValue0 key pat = fromValue' $ getValue key pat

-- | Deletes all entries in the given pattern under a given key.
unset key mapping = [(key', val') | (key', val') <- mapping, key' /= key]

-- | Restructures a `Pattern` so each key repeats at most once.
normalizePattern :: Pattern -> Pattern
normalizePattern pat =
    [(key, [val | (key', vals) <- pat, key' == key, val <- vals]) | key <- nub $ map fst pat]

-- | Returns whether pa and pb have exactly the same values for all of the objects in os.
equalSubset :: Pattern -> Pattern -> ObjectSet -> Bool
equalSubset a b objs = unsafePerformIO $ withPattern a $ \a' -> withPattern b $ \b' ->
    withObjectSet objs $ fcPatternEqualSubset a' b'
foreign import ccall "FcPatternEqualSubset" fcPatternEqualSubset ::
    Pattern_ -> Pattern_ -> ObjectSet_ -> IO Bool

-- | Returns a new pattern that only has those objects from p that are in os.
-- If os is NULL, a duplicate of p is returned.
filter :: Pattern -> ObjectSet -> Pattern
filter pat objs =
    unsafePerformIO $ withPattern pat $ \pat' -> withObjectSet objs $ \objs' ->
        thawPattern_ $ fcPatternFilter pat' objs'
foreign import ccall "FcPatternFilter" fcPatternFilter ::
    Pattern_ -> ObjectSet_ -> IO Pattern_

-- | Supplies default values for underspecified font patterns:
-- * Patterns without a specified style or weight are set to Medium
-- * Patterns without a specified style or slant are set to Roman
-- * Patterns without a specified pixel size are given one computed from any
-- specified point size (default 12), dpi (default 75) and scale (default 1).
defaultSubstitute :: Pattern -> Pattern
defaultSubstitute pat = unsafePerformIO $ withPattern pat $ \pat' -> do
    ret <- fcDefaultSubstitute pat'
    thawPattern pat'
foreign import ccall "FcDefaultSubstitute" fcDefaultSubstitute :: Pattern_ -> IO ()

-- Is this correct memory management?
-- | Converts name from the standard text format described above into a pattern.
nameParse :: String -> Pattern
nameParse name = unsafePerformIO $ withCString name $ \name' ->
    thawPattern_ $ fcNameParse name'
foreign import ccall "FcNameParse" fcNameParse :: CString -> IO Pattern_

-- | Converts the given pattern into the standard text format described above.
nameUnparse :: Pattern -> String
nameUnparse pat = unsafePerformIO $ withPattern pat $ \pat' ->
    bracket (throwNull <$> fcNameUnparse pat') free peekCString
foreign import ccall "FcNameUnparse" fcNameUnparse :: Pattern_ -> IO CString

-- | Converts given pattern into text described fy given format specifier.
-- See for details: https://www.freedesktop.org/software/fontconfig/fontconfig-devel/fcpatternformat.html
format :: Pattern -> String -> String
format pat fmt =
    unsafePerformIO $ withPattern pat $ \pat' -> withCString fmt $ \fmt' -> do
        bracket (throwNull <$> fcPatternFormat pat' fmt') free peekCString
foreign import ccall "FcPatternFormat" fcPatternFormat ::
    Pattern_ -> CString -> IO CString

------
--- Low-level
------

data Pattern'
type Pattern_ = Ptr Pattern'

withPattern :: Pattern -> (Pattern_ -> IO a) -> IO a
withPattern pat cb = withNewPattern $ \pat' -> do
    forM pat $ \(obj, vals) -> withCString obj $ \obj' -> do
        forM vals $ \(strength, val) -> throwFalse <$> withValue val
            (fcPatternAdd_ pat' obj' (strength == Strong) True)
    cb pat'
-- Does Haskell FFI support unboxed structs? Do I really need to write a C wrapper?
foreign import ccall "my_FcPatternAdd" fcPatternAdd_ ::
    Pattern_ -> CString -> Bool -> Bool -> Value_ -> IO Bool

patternAsPointer :: Pattern -> IO Pattern_
patternAsPointer = flip withPattern $ \ret -> do
    fcPatternReference ret
    return ret
foreign import ccall "FcPatternReference" fcPatternReference :: Pattern_ -> IO ()

data PatternIter'
type PatternIter_ = Ptr PatternIter'
foreign import ccall "size_PatternIter" patIter'Size :: Int
thawPattern :: Pattern_ -> IO Pattern
thawPattern pat' = do
    iter <- mallocForeignPtrBytes patIter'Size
    pat <- gcPattern pat'
    with2ForeignPtrs pat iter fcPatternIterStart
    ret <- go pat iter
    return $ normalizePattern ret
  where
    go :: ForeignPtr Pattern' -> ForeignPtr PatternIter' -> IO Pattern
    go pat iter = unsafeInterleaveIO $ do
        ok <- with2ForeignPtrs pat iter fcPatternIterIsValid
        if ok then do
            x <- with2ForeignPtrs pat iter thawPattern'
            ok' <- with2ForeignPtrs pat iter fcPatternIterNext
            xs <- if ok' then go pat iter else return []
            return (x : xs)
        else return []
    with2ForeignPtrs a b cb = withForeignPtr a $ \a' -> withForeignPtr b $ cb a'
foreign import ccall "FcPatternIterStart" fcPatternIterStart ::
    Pattern_ -> PatternIter_ -> IO ()
foreign import ccall "FcPatternIterIsValid" fcPatternIterIsValid ::
    Pattern_ -> PatternIter_ -> IO Bool
foreign import ccall "FcPatternIterNext" fcPatternIterNext ::
    Pattern_ -> PatternIter_ -> IO Bool

thawPattern' :: Pattern_ -> PatternIter_ -> IO (String, [(Binding, Value)])
thawPattern' pat' iter' = do
    obj <- peekCString =<< throwNull <$> fcPatternIterGetObject pat' iter'
    count <- fcPatternIterValueCount pat' iter'
    values <- forM [0..pred count] $ \i ->
        allocaBytes value'Size $ \val' -> alloca $ \binding' -> do
            res <- fcPatternIterGetValue pat' iter' i val' binding'
            throwInt res $ do
                binding <- peek binding'
                val' <- thawValue val'
                return $ case val' of
                    Just val | binding >= 0 && binding <= 2 -> Just (toEnum binding, val)
                    Just val -> Just (Same, val)
                    Nothing -> Nothing
    return (obj, catMaybes $ map join values)
foreign import ccall "FcPatternIterGetObject" fcPatternIterGetObject ::
    Pattern_ -> PatternIter_ -> IO CString
foreign import ccall "FcPatternIterValueCount" fcPatternIterValueCount ::
    Pattern_ -> PatternIter_ -> IO Int
foreign import ccall "FcPatternIterGetValue" fcPatternIterGetValue ::
    Pattern_ -> PatternIter_ -> Int -> Value_ -> Ptr Int -> IO Int

thawPattern_ cb = bracket (throwNull <$> cb) fcPatternDestroy thawPattern

withNewPattern cb = bracket (throwNull <$> fcPatternCreate) fcPatternDestroy cb
foreign import ccall "FcPatternCreate" fcPatternCreate :: IO Pattern_
foreign import ccall "FcPatternDestroy" fcPatternDestroy :: Pattern_ -> IO ()
foreign import ccall "&FcPatternDestroy" fcPatternDestroy' ::
    FunPtr (Pattern_ -> IO ())

gcPattern :: Pattern_ -> IO (ForeignPtr Pattern')
gcPattern pat' = do
    fcPatternReference pat'
    newForeignPtr fcPatternDestroy' pat'

------
--- Pattern
------

parseFontFamily :: [Token] -> ([String], Bool, [Token])
parseFontFamily (String font:Comma:tail) = let (fonts, b, tail') = parseFontFamily tail
    in (unpack font:fonts, b, tail')
parseFontFamily (Ident font:Comma:tail) = let (fonts, b, tail') = parseFontFamily tail
    in (unpack font:fonts, b, tail')
parseFontFamily (String font:tail) = ([unpack font], True, tail)
parseFontFamily (Ident font:tail) = ([unpack font], True, tail)
parseFontFamily toks = ([], False, toks) -- Invalid syntax!

parseFontFeatures :: [Token] -> ([(String, Int)], Bool, [Token])
parseFontFeatures (String feat:toks) | feature@(_:_:_:_:[]) <- unpack feat = case toks of
    Comma:tail -> let (feats, b, tail') = parseFontFeatures tail in ((feature, 1):feats, b, tail')
    Ident "on":Comma:tail -> let (f, b, t) = parseFontFeatures tail in ((feature, 1):f, b, t)
    Ident "on":tail -> ([(feature, 1)], True, tail)
    Ident "off":Comma:tail -> let (f, b, t) = parseFontFeatures tail in ((feature, 1):f, b, t)
    Ident "off":tail -> ([(feature, 1)], True, tail)
    Number _ (NVInteger x):Comma:tail ->
        let (feats, b, tail') = parseFontFeatures tail in ((feature, fromEnum x):feats, b, tail')
    Number _ (NVInteger x):tail -> ([(feature, fromEnum x)], True, tail)
parseFontFeatures toks = ([], False, toks)

parseFontVars :: [Token] -> ([(String, Double)], Bool, [Token])
parseFontVars (String var':Number _ x:Comma:tail) | var@(_:_:_:_:[]) <- unpack var' =
    let (vars, b, tail') = parseFontVars tail in ((var, nv2double x):vars, b, tail')
parseFontVars (String var':Number _ x:tail) | var@(_:_:_:_:[]) <- unpack var' =
    ([(var, nv2double x)], True, tail)
parseFontVars toks = ([], False, toks)

parseLength :: Double -> NumericValue -> Text -> Double
parseLength super length unit = convert (nv2double length) unit
  where
    convert = c
    c x "pt" = x -- Unit FontConfig expects!
    c x "pc" = x/6 `c` "in"
    c x "in" = x/72 `c` "pt"
    c x "Q" = x/40 `c` "cm"
    c x "mm" = x/10 `c` "cm"
    c x "cm" = x/2.54 `c` "in"
    c x "px" = x/96 `c` "in" -- Conversion factor during early days of CSS, got entrenched.
    c x "em" = x * super
    c x "%" = x/100 `c` "em"
    c _ _ = 0/0 -- NaN

parseFontStretch :: Token -> Maybe Int -- Result in percentages
parseFontStretch (Percentage _ x) = Just $ fromEnum $ nv2double x
parseFontStretch (Ident "ultra-condensed") = Just 50
parseFontStretch (Ident "extra-condensed") = Just 63 -- 62.5%, but round towards 100%
parseFontStretch (Ident "condensed") = Just 75
parseFontStretch (Ident "semi-condensed") = Just 88 -- 87.5% actually...
parseFontStretch (Ident "normal") = Just 100
parseFontStretch (Ident "initial") = Just 100
parseFontStretch (Ident "semi-expanded") = Just 112 -- 112.5% actually...
parseFontStretch (Ident "expanded") = Just 125
parseFontStretch (Ident "extra-expanded") = Just 150
parseFontStretch (Ident "ultra-expanded") = Just 200
parseFontStretch _ = Nothing

-- Conversion between CSS scale & FontConfig scale is non-trivial, use lookuptable.
parseFontWeight :: Token -> Maybe Int
parseFontWeight (Ident k) | k `elem` ["initial", "normal"] = Just 80
parseFontWeight (Ident "bold") = Just 200
parseFontWeight (Number _ (NVInteger x)) = Just $ weightFromOpenType $ fromEnum x
parseFontWeight _ = Nothing

nv2double (NVInteger x) = fromInteger x
nv2double (NVNumber x) = toRealFloat x

sets a b c d = Just $ setValues a b c d
set a b c d = Just $ setValue a b c d
seti a b c d = Just $ setValue a b (c :: Int) d
unset' a b = Just $ unset a b

getSize pat | ValueDouble x <- getValue "size" pat = x
    | otherwise = 10

instance PropertyParser Pattern where
    temp = []

    longhand _ self "font-family" toks
        | (fonts, True, []) <- parseFontFamily toks = sets "family" Strong fonts self

    -- font-size: initial should be configurable!
    longhand super self "font-size" [Dimension _ x unit]
        | let y = parseLength (getSize super) x unit, not $ isNaN y =
            set "size" Strong y self
    longhand super self "font-size" [Percentage x y] =
        longhand super self "font-size" [Dimension x y "%"]

    longhand _ self "font-style" [Ident "initial"] = seti "slant" Strong 0 self
    longhand _ self "font-style" [Ident "normal"] = seti "slant" Strong 0 self
    longhand _ self "font-style" [Ident "italic"] = seti "slant" Strong 100 self
    longhand _ self "font-style" [Ident "oblique"] = seti "slant" Strong 110 self

    -- Conversion between CSS scale & FontConfig scale is non-trivial, use lookuptable.
    longhand _ self "font-weight" [tok]
        | Just x <- parseFontWeight tok = seti "weight" Strong x self
    longhand super self "font-weight" [Number _ (NVInteger x)]
        | x > 920 = longhand super self "font-weight" [Number "" $ NVInteger 950]
        | otherwise = longhand super self "font-weight" [Number "" $ NVInteger $ (x `div` 100) * 100]
    longhand _ self "font-weight" [Ident "lighter"]
        | ValueInt x <- getValue "weight" self, x > 200 = seti "weight" Strong 200 self
        -- minus 100 adhears to the CSS standard awefully well in this new scale.
        | ValueInt x <- getValue "weight" self = seti "weight" Strong (max (x - 100) 0) self
        | otherwise = seti "weight" Strong 0 self
    longhand _ self "font-weight" [Ident "bolder"]
        | ValueInt x <- getValue "weight" self, x <= 65 = seti "weight" Strong 80 self
        | ValueInt x <- getValue "weight" self, x <= 150 = seti "weight" Strong 200 self
        | ValueInt x <- getValue "weight" self, x < 210 = seti "weight" Strong 210 self
        | ValueInt _ <- getValue "weight" self = Just self -- As bold as it goes...
        | otherwise = seti "weight" Strong 200 self

    longhand _ self "font-feature-settings" [Ident k]
        | k `elem` ["initial", "normal"] = unset' "fontfeatures" self
    longhand _ self "font-feature-settings" toks
        | (features, True, []) <- parseFontFeatures toks =
            set "fontfeatures" Strong (intercalate "," $ map fst features) self

    longhand _ self "font-variation-settings" [Ident k]
        | k `elem` ["initial", "normal"] = unset' "variable" self
    longhand _ self "font-variation-settings" toks
        | (_, True, []) <- parseFontVars toks = set "variable" Strong True self

    longhand _ s "font-stretch" [tok]
        | Just x <- parseFontStretch tok = seti "width" Strong x s

    longhand _ _ _ _ = Nothing
