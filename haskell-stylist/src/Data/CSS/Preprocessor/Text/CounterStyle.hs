{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Data.CSS.Preprocessor.Text.CounterStyle(CounterStyle(..), CounterSystem(..),
    defaultCounter, decimalCounter, simpChineseInformal, cjkDecimal, ethiopic,
    isValid, parseCounterStyle, CounterStore'(..), parseCounter, defaultCounterStore,
    counterRender, counterRenderMarker, ranges', speakAs', CounterStore) where
import Data.CSS.Syntax.Tokens
import Data.CSS.Syntax.StyleSheet

import Data.FileEmbed (embedStringFile, makeRelativeToProject)

import qualified Data.Text as Txt
import Data.Text (Text, unpack)
import qualified Data.HashMap.Lazy as HM
import Data.HashMap.Lazy (HashMap)
import Data.Maybe (isJust, fromJust)

-- NOTE: No support for image "symbols" yet.
data CounterStyle = CounterStyle {
    system :: CounterSystem,
    negativePrefix :: Text,
    negativeSuffix :: Text,
    prefix :: Text,
    suffix :: Text,
    ranges :: Maybe [(Int, Int)],
    padLength :: Int,
    padChar :: Text,
    fallback :: Maybe CounterStyle,
    symbols :: [Text],
    additiveSymbols :: [(Int, Text)],
    speakAs :: Maybe Text
}
data CounterSystem = Cyclic | Fixed Int | Symbolic | Alphabetic | Numeric
        | Additive | Chinese { isSimplified :: Bool } | Ethiopic

defaultCounter, decimalCounter :: CounterStyle
ethiopic, simpChineseInformal, cjkDecimal :: CounterStyle
defaultCounter = CounterStyle {
    system = Symbolic,
    negativePrefix = "-",
    negativeSuffix = "",
    prefix = "",
    suffix = ". ",
    ranges = Nothing,
    padLength = 0,
    padChar = "",
    fallback = Just decimalCounter,
    symbols = [], -- Must be overriden!
    additiveSymbols = [], -- Alternative requirement!
    speakAs = Nothing
}
decimalCounter = defaultCounter {
    system = Numeric,
    symbols = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"],
    fallback = Nothing
}
-- These are here mostly for testing...
cjkDecimal = defaultCounter {
    system = Numeric,
    ranges = Just [(0, maxBound)],
    symbols = ["〇", "一", "二", "三", "四", "五", "六", "七", "八", "九"],
    suffix = "、"
}
simpChineseInformal = defaultCounter {
    system = Chinese True,
    negativePrefix = "负",
    symbols = ["零", "一", "二", "三", "四", "五", "六", "七", "八", "九"],
    additiveSymbols = [(0, ""), (10, "十"), (100, "百"), (1000, "千")],
    suffix = "、",
    fallback = Just cjkDecimal
}
ethiopic = defaultCounter {
    system = Ethiopic,
    symbols = ["", "፩", "፪", "፫", "፬", "፭", "፮", "፯", "፰", "፱"],
    additiveSymbols = [(0, ""), (10, "፲"), (20, "፳"), (30, "፴"), (40, "፵"),
                       (50, "፶"), (60, "፷"), (70, "፸"), (80, "፹"), (90, "፺")],
    suffix = "/ "
}

isValid :: CounterStyle -> Bool
isValid CounterStyle { system = Additive, additiveSymbols = [] } = False
isValid self@CounterStyle {
        system = Chinese _, symbols = syms, additiveSymbols = markers
    } = length syms == 10 && length markers >= 4 && ranges self == Nothing
isValid CounterStyle {
        system = Ethiopic, symbols = units, additiveSymbols = tens
    } = length units == 10 && length tens == 10
isValid CounterStyle { symbols = [] } = False
isValid _ = True

type CounterStore = HashMap Text CounterStyle
parseCounterProperty :: CounterStore -> (Text, [Token]) ->
        CounterStyle -> CounterStyle
parseCounterProperty _ ("system", [Ident "cyclic"]) self = self {system = Cyclic}
parseCounterProperty _ ("system", [Ident "fixed"]) self = self {system = Fixed 1}
parseCounterProperty _ ("system", [Ident "fixed", Number _ (NVInteger x)]) self
    = self { system = Fixed $ fromInteger x }
parseCounterProperty _ ("system", [Ident "symbolic"]) self =
    self {system = Symbolic }
parseCounterProperty _ ("system", [Ident "alphabetic"]) self =
    self { system = Alphabetic }
parseCounterProperty _ ("system", [Ident "numeric"]) self =
    self { system = Numeric }
parseCounterProperty _ ("system", [Ident "-argo-chinese", Ident x]) self =
    self { system = Chinese (x == "simplified") }
parseCounterProperty _ ("system", [Ident "-argo-ethiopic"]) self =
    self { system = Ethiopic }
-- Handled by caller so property overrides work correctly.
parseCounterProperty _ ("system", [Ident "extends", Ident _]) self = self

parseCounterProperty _ ("negative", [x]) self | Just pre <- parseSymbol x =
    self { negativePrefix = pre, negativeSuffix = "" }
parseCounterProperty _ ("negative", [x, y]) self
    | Just pre <- parseSymbol x, Just suf <- parseSymbol y =
        self { negativePrefix = pre, negativeSuffix = suf }

parseCounterProperty _ ("prefix", [x]) self | Just pre <- parseSymbol x =
    self { prefix = pre }
parseCounterProperty _ ("suffix", [x]) self | Just suf <- parseSymbol x =
    self { suffix = suf }

parseCounterProperty _ ("range", [Ident "auto"]) self = self {ranges = Nothing}
parseCounterProperty _ ("range", toks) self | Just rs <- parseRanges (Comma:toks) =
    self { ranges = Just rs }

parseCounterProperty _ ("pad", [Number _ (NVInteger x), y]) self
    | Just char <- parseSymbol y = self {
            padLength = fromInteger x, padChar = char
          }
parseCounterProperty styles ("fallback", [Ident name]) self = self {
        fallback = Just $ HM.lookupDefault decimalCounter name styles
      }
parseCounterProperty _ ("symbols", toks) self | all (isJust . parseSymbol) toks
    = self { symbols = map (fromJust . parseSymbol) toks }
parseCounterProperty _ ("additive-symbols", toks) self
    | Just syms <- parseAdditiveSymbols (Comma:toks) =
        self { additiveSymbols = syms }

parseCounterProperty _ ("speak-as", [Ident "auto"]) self =
    self { speakAs = Nothing }
parseCounterProperty styles ("speak-as", [Ident x]) self
    | x `elem` ["bullets", "numbers", "words", "spell-out"] =
        self { speakAs = Just x }
    | Just super <- HM.lookup x styles = self { speakAs = speakAs super }
    | otherwise = self
parseCounterProperty _ _ self = self

parseRanges :: [Token] -> Maybe [(Int, Int)]
parseRanges (Comma:a:b:toks) | Just self <- parseRanges toks = case (a, b) of
    (Ident "infinite", Ident "infinite") -> Just ((minBound, maxBound):self)
    (Ident "infinite", Number _ (NVInteger x)) ->
        Just ((minBound, fromInteger x):self)
    (Number _ (NVInteger x), Ident "infinite") ->
        Just ((fromInteger x, maxBound):self)
    (Number _ (NVInteger x), Number _ (NVInteger y)) ->
        Just ((fromInteger x, fromInteger y):self)
    _ -> Nothing
parseRanges [] = Just []
parseRanges _ = Nothing

parseAdditiveSymbols :: [Token] -> Maybe [(Int, Text)]
parseAdditiveSymbols (Comma:Number _ (NVInteger x):y:toks)
    | Just self <- parseAdditiveSymbols toks, Just sym <- parseSymbol y =
        Just ((fromInteger x, sym):self)
parseAdditiveSymbols [] = Just []
parseAdditiveSymbols _ = Nothing

parseCounterStyle :: CounterStore -> [Token] -> (CounterStore, [Token])
parseCounterStyle store (Whitespace:toks) = parseCounterStyle store toks
parseCounterStyle store (Ident name:toks)
    | ((props, ""), toks') <- parseProperties toks =
        let super = case Prelude.lookup "system" props of
                Just [Ident "extends", Ident name'] ->
                    HM.lookupDefault decimalCounter name' store
                _ -> defaultCounter
            style = foldr (parseCounterProperty store) super props
        in (HM.insert name style store, toks')
parseCounterStyle store toks = (store, skipAtRule toks)

parseSymbol :: Token -> Maybe Text
parseSymbol (Ident x) = Just x
parseSymbol (String x) = Just x
parseSymbol _ = Nothing

data CounterStore' = CounterStore { unwrap :: CounterStore }
instance StyleSheet CounterStore' where
    addRule self _ = self
    addAtRule (CounterStore self) "counter-style" toks =
        let (self', toks') = parseCounterStyle self toks
        in (CounterStore self', toks')
    addAtRule self _ toks = (self, skipAtRule toks)

defaultCounterStore :: CounterStore'
defaultCounterStore =
    parse (CounterStore HM.empty) $ Txt.pack
        $(makeRelativeToProject "src/Data/CSS/Preprocessor/Text/counter-styles.css" >>=
          embedStringFile)

---

fallbackSym :: Text
fallbackSym = "\0"

counterRenderCore :: CounterStyle -> Int -> Text
counterRenderCore CounterStyle { system = Fixed n, symbols = syms } x
    | x - n < length syms && x >= n = syms !! (x - n)
    | otherwise = fallbackSym
counterRenderCore _ x | x < 0 = fallbackSym
counterRenderCore CounterStyle { system = Cyclic, symbols = syms } x =
    syms !! (pred x `rem` length syms)
counterRenderCore CounterStyle { system = Symbolic, symbols = syms } x =
    succ (quot x' n) `Txt.replicate` (syms !! rem x' n)
  where (n, x') = (length syms, pred x)
counterRenderCore CounterStyle { system = Alphabetic, symbols = syms } x = inner x
  where
    n = length syms
    inner 0 = ""
    inner y = let x' = pred y in inner (quot x' n) `Txt.append` (syms !! rem x' n)
counterRenderCore CounterStyle { system = Numeric, symbols = syms } 0 = syms !! 0
counterRenderCore CounterStyle { system = Numeric, symbols = syms } x = inner x
  where
    n = length syms
    inner 0 = ""
    inner y = inner (quot y n) `Txt.append` (syms !! rem y n)
counterRenderCore CounterStyle { system = Additive, additiveSymbols = syms } 0
    | Just sym <- Prelude.lookup 0 syms = sym
    | otherwise = fallbackSym
counterRenderCore CounterStyle { system = Additive, additiveSymbols = syms } w
    | '\0' `elem'` inner syms w = fallbackSym
    | otherwise = inner syms w
  where
    elem' ch txt = elem ch $ unpack txt
    inner _ 0 = ""
    inner ((0, _):syms') x = inner syms' x
    inner ((weight, _):syms') x | weight > x = inner syms' x
    inner ((weight, sym):syms') x =
        Txt.replicate reps sym `Txt.append` inner syms' x'
      where
        reps = quot x weight
        x' = x - weight * reps
    inner [] _ = "\0" -- Run fallback counter!
-- Following https://w3c.github.io/csswg-drafts/css-counter-styles-3/#limited-chinese
-- 1. If the counter value is 0, the representation is the character for 0
-- specified for the given counter style. Skip the rest of this algorithm.
counterRenderCore CounterStyle { system = Chinese _, symbols = (sym:_) } 0 = sym
counterRenderCore CounterStyle {
        system = Chinese simplified, symbols = syms, additiveSymbols = markers
    } x = Txt.concat $ map renderDigit $ collapse0s $
        reverse $ enumerate $ decimalDigits x
  where
    -- Implements step 4.
    collapse0s ((i, 0):digits) = inner digits
      where
        inner ((_, 0):digits') = inner digits'
        -- Drop trailing 0s
        inner [] = []
        -- Collapse any remaining (consecutive?) zeroes into a single 0 digit.
        inner digits' = (i, 0):collapse0s digits'
    collapse0s (digit:digits) = digit:collapse0s digits
    collapse0s [] = []
    renderDigit (_, 0) = syms !! 0 -- Don't append digit marker for zero, step 2.
    -- 3. For the informal styles, if the counter value is between 10 and 19,
    -- remove the 10s digit (leave the digit marker).
    renderDigit (1,1) | simplified, [_, _] <- decimalDigits x = markers' !! 1
    -- Select characters per steps 2 & 5
    renderDigit (place, digit) = Txt.concat [syms !! digit, markers' !! place]
    markers' = map snd markers
-- Following https://w3c.github.io/csswg-drafts/css-counter-styles-3/#ethiopic-numeric-counter-style
-- 1. If the number is 1, return "፩" (U+1369).
counterRenderCore CounterStyle { system = Ethiopic, symbols = (_:sym:_) } 1 = sym
counterRenderCore CounterStyle {
        system = Ethiopic, symbols = unitSyms, additiveSymbols = tenSyms
    } x = Txt.concat $ renderPairs True $
        reverse $ enumerate $ pairDigits $ decimalDigits x
  where
    -- 2. Split the number into groups of two digits,
    -- starting with the least significant decimal digit.
    pairDigits (units:tens:digits) = (tens,units):pairDigits digits
    pairDigits [units] = (0, units):[]
    pairDigits [] = []

    renderPairs isBigEnd (group:groups) =
        renderPair isBigEnd group:renderPairs False groups
    renderPairs _ [] = []
    -- Handle step 4's exceptions.
    renderPair' :: Bool -> (Int, (Int, Int)) -> Text
    renderPair' _ (_,(0, 0)) = ""
    renderPair' True (_, (0,1)) = ""
    renderPair' _ (i, (0,1)) | odd i = ""
    -- Step 5
    renderPair' _ (_, (tens, units)) =
        (map snd tenSyms !! tens) `Txt.append` (unitSyms !! units)
    -- Step 6 & 7
    renderPair _ (i, (0,0)) | odd i = ""
    renderPair isBigEnd (0, group) = renderPair' isBigEnd (0, group)
    renderPair isBigEnd (i, group)
        | odd i = renderPair' isBigEnd (i, group) `Txt.append` "፻"
        | even i = renderPair' isBigEnd (i, group) `Txt.append` "፼"
    renderPair _ _ = "" -- Silence warnings, above case should not fallthrough.
decimalDigits :: Int -> [Int]
decimalDigits 0 = []
decimalDigits x = rem x 10:decimalDigits (quot x 10)
enumerate :: [a] -> [(Int, a)]
enumerate = zip $ enumFrom 0

counterRenderMarker :: CounterStyle -> Int -> Text
counterRenderMarker self x =
    Txt.concat [prefix self, counterRender self x, suffix self]
counterRender :: CounterStyle -> Int -> Text
counterRender self@CounterStyle { fallback = Just self' } x
    | not $ inRange x $ ranges' self = counterRender self' x
    | counterRenderCore self x == fallbackSym = counterRender self' x
  where
    inRange y ((start, end):rest)
        | y >= start && y <= end = True
        | otherwise = inRange y rest
    inRange _ [] = False
counterRender self@CounterStyle { padLength = m, padChar = pad } x
    | Fixed _ <- system self = text -- Handles negatives specially here.
    | x < 0 = Txt.concat [
        negativePrefix self,
        counterRender self { ranges = Just [(0, maxBound)] } $ -x, -- No fallback!
        negativeSuffix self
      ]
    | text == fallbackSym = Txt.pack $ show x -- NOTE: Shouldn't happen
    | n < m = Txt.replicate (m - n) pad `Txt.append` text
    | otherwise = text
  where
    text = counterRenderCore self x
    n = Txt.length text

infiniteRange :: [(Int, Int)]
infiniteRange = [(minBound, maxBound)]
ranges' :: CounterStyle -> [(Int, Int)]
ranges' CounterStyle { ranges = Just ret } = ret
ranges' CounterStyle { system = Cyclic } = infiniteRange
ranges' CounterStyle { system = Numeric } = infiniteRange
ranges' CounterStyle { system = Fixed _ } = infiniteRange
ranges' CounterStyle { system = Alphabetic } = [(1, maxBound)]
ranges' CounterStyle { system = Symbolic } = [(1, maxBound)]
ranges' CounterStyle { system = Additive } = [(0, maxBound)]
ranges' CounterStyle { system = Chinese _ } = [(-9999, 9999)]
ranges' CounterStyle { system = Ethiopic } = [(1, maxBound)]

speakAs' :: CounterStyle -> Text
speakAs' CounterStyle { speakAs = Just ret } = ret
speakAs' CounterStyle { system = Alphabetic } = "spell-out"
speakAs' CounterStyle { system = Cyclic } = "bullets"
speakAs' _ = "numbers"

---

parseCounter :: CounterStore -> [Token] -> Maybe (CounterStyle, [Token])
parseCounter _ (Function "symbols":Ident name:toks)
    | Just system' <- Prelude.lookup name [
        ("cyclic", Cyclic), ("numeric", Numeric), ("alphabetic", Alphabetic),
        ("symbolic", Symbolic), ("fixed", Fixed 1)
      ], Just (syms, toks') <- parseArgs toks =
        Just (defaultCounter { system = system', symbols = syms }, toks')
  where
    parseArgs (String sym:toks') | Just (syms,tail') <- parseArgs toks' =
        Just (sym:syms, tail')
    parseArgs (RightParen:toks') = Just ([],toks')
    parseArgs _ = Nothing
parseCounter store (Ident name:toks)
    | Just ret <- HM.lookup name store, isValid ret = Just (ret, toks)
    | otherwise = Just (decimalCounter, toks)
parseCounter _ (String sym:toks) =
    Just (defaultCounter {system = Cyclic, symbols = [sym], suffix = " "}, toks)
parseCounter _ _ = Nothing
