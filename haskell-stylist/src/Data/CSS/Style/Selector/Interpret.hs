{-# LANGUAGE OverloadedStrings #-}
-- | Evaluates CSS selectors over an element.
-- INTERNAL MODULE.
module Data.CSS.Style.Selector.Interpret(
        compile, SelectorFunc,
        InterpretedRuleStore(..)
    ) where

import Data.CSS.Style.Common
import Stylist (compileAttrTest, matched, hasWord)

import Data.Text (unpack)
import Data.List
import Data.Maybe
import Data.Bits (xor)

-- For pseudoclasses
import Data.CSS.Syntax.Selector (parseSelectors)
import Data.CSS.Syntax.Tokens (Token(..), NumericValue(..))

-- | A compiled(?) CSS selector.
type SelectorFunc = Element -> Bool
type AttrsFunc = [Attribute] -> Bool
-- Mostly here for the sake of pseudoclasses.
data IL = Tagname Text | NS Text | Fail | Recursive Bool [Selector] | Nth Bool Integer Integer | Root

-- | Converts a parsed CSS selector into a callable function.
compile :: Selector -> SelectorFunc
compile (Element sel) = compileInner sel
compile (Child upSel sel) = direct parent (compile upSel) $ compileInner sel
compile (Descendant up sel) = indirect parent (compile up) $ compileInner sel
compile (Adjacent up sel) = direct previous (compile up) $ compileInner sel
compile (Sibling up sel) = indirect previous (compile up) $ compileInner sel

compileInner :: [SimpleSelector] -> SelectorFunc
compileInner sel = compileInner' $ lowerInner sel
compileInner' :: ([IL], [(Text, Maybe Text, String -> Bool)]) -> SelectorFunc
compileInner' (Tagname tag:tests, attrs) = testTag tag $ compileInner' (tests, attrs)
compileInner' (NS ns:tests, attrs) = testNS ns $ compileInner' (tests, attrs)
compileInner' (Fail:_, _) = \_ -> False
compileInner' (Recursive negate' sels:tests, attrs) =
    recursiveSelect negate' (map compile sels) $ compileInner' (tests, attrs)
compileInner' (Nth ofType n 0:tests, attrs) =
    nthChild ofType (fromInteger n) $ compileInner' (tests, attrs)
compileInner' (Nth ofType a b:tests, attrs) =
    nthChild' ofType (fromInteger a) (fromInteger b) $ compileInner' (tests, attrs)
compileInner' (Root:tests, attrs) = testRoot $ compileInner' (tests, attrs)
compileInner' ([], attrs) = testAttrs (compileAttrs $ sortAttrs attrs) matched
compileAttrs :: [(Text, Maybe Text, String -> Bool)] -> AttrsFunc
compileAttrs ((tag, Nothing, test):attrs) = testAttr tag test $ compileAttrs attrs
compileAttrs ((tag, Just ns, test):attrs) = testAttrNS ns tag test $ compileAttrs attrs
compileAttrs [] = matched

lowerInner :: [SimpleSelector] -> ([IL], [(Text, Maybe Text, String -> Bool)])
lowerInner (Namespace ns:sel) = (NS ns:tests, attrs) where (tests, attrs) = lowerInner sel
lowerInner (Tag tag:sel) = (Tagname tag:tests, attrs) where (tests, attrs) = lowerInner sel
lowerInner (Id i:s) = (tests, ("id", Nothing, hasWord $ unpack i):attrs) where (tests, attrs) = lowerInner s
lowerInner (Class c:s) = (tests, ("class", Nothing, hasWord $ unpack c):attrs) where (tests, attrs) = lowerInner s
lowerInner (Property ns prop test:s) = (tests, (prop, ns, compileAttrTest test):attrs)
    where (tests, attrs) = lowerInner s
-- psuedos, TODO handle argumented psuedoclasses.
lowerInner (Psuedoclass c args:s)
    | c `elem` ["is", "where"], (sels, []) <- parseSelectors args =
        (Recursive False sels:tests, attrs) where (tests, attrs) = lowerInner s
lowerInner (Psuedoclass "not" args:s) | (sels, []) <- parseSelectors args =
    (Recursive True sels:tests, attrs) where (tests, attrs) = lowerInner s
lowerInner (Psuedoclass "nth-child" args:s) =
    (parseNth False (filter (== Whitespace) args):tests, attrs) where (tests, attrs) = lowerInner s
lowerInner (Psuedoclass "nth-of-type" args:s) =
    (parseNth True (filter (== Whitespace) args):tests, attrs) where (tests, attrs) = lowerInner s
lowerInner (Psuedoclass "root" []:s) = (Root:tests, attrs) where (tests, attrs) = lowerInner s
lowerInner (Psuedoclass c []:s) =
    (tests, ("", Nothing, hasWord $ unpack c):attrs) where (tests, attrs) = lowerInner s
lowerInner (Psuedoclass _ _:_) = ([Fail], [])
lowerInner [] = ([], [])

sortAttrs :: [(Text, Maybe Text, b)] -> [(Text, Maybe Text, b)]
sortAttrs = sortBy compareAttrs where compareAttrs (x, x', _) (y, y', _) = (x, x') `compare` (y, y')

--------
---- Runtime
--------
testTag :: Text -> SelectorFunc -> SelectorFunc
testTag tag success el | name el == tag = success el
    | otherwise = False
testNS :: Text -> SelectorFunc -> SelectorFunc
testNS ns success el | namespace el == ns = success el
    | otherwise = False
testAttrs :: AttrsFunc -> SelectorFunc -> SelectorFunc
testAttrs attrsTest success el | attrsTest $ attributes el = success el
    | otherwise = False
direct :: (Element -> Maybe Element) -> SelectorFunc -> SelectorFunc -> SelectorFunc
direct traverser upTest test el | Just up <- traverser el = test el && upTest up
    | otherwise = False
indirect :: (Element -> Maybe Element) -> SelectorFunc -> SelectorFunc -> SelectorFunc
indirect traverser upTest test el | Nothing <- traverser el = False
    | not $ test el = False
    | upTest (fromJust $ traverser el) = True
    | otherwise = indirect traverser upTest test $ fromJust $ traverser el

testAttr :: Text -> (String -> Bool) -> AttrsFunc -> AttrsFunc
testAttr expected test next attrs@(Attribute attr _ value : attrs')
    | attr < expected = testAttr expected test next attrs'
    | attr > expected = False
    | attr == expected && test value = next attrs
    | otherwise = False
testAttr _ _ _ [] = False
testAttrNS :: Text -> Text -> (String -> Bool) -> AttrsFunc -> AttrsFunc
testAttrNS expectedNS expected test next attrs@(Attribute attr ns value : attrs')
    | (attr, ns) < (expected, expectedNS) = testAttrNS expectedNS expected test next attrs'
    | (attr, ns) > (expected, expectedNS) = False
    | (attr, ns) == (expected, expectedNS) && test value = next attrs
    | otherwise = False
testAttrNS _ _ _ _ [] = False

--- Pseudoclasses
recursiveSelect :: Bool -> [SelectorFunc] -> SelectorFunc -> SelectorFunc
recursiveSelect negate' sels success el | negate' `xor` any ($ el) sels = success el
    | otherwise = False

parseNth :: Bool -> [Token] -> IL
parseNth ofType [Ident "odd"] = Nth ofType 2 1
parseNth ofType [Ident "even"] = Nth ofType 2 0
parseNth x [Dimension _ (NVInteger a) "n", Number _ (NVInteger b)] = Nth x a b
parseNth x [Number _ (NVInteger b), Dimension _ (NVInteger a) "n"] = Nth x a b
parseNth x [Dimension _ (NVInteger a) "n", Delim '+', Number _ (NVInteger b)] = Nth x a b
parseNth x [Number _ (NVInteger b), Delim '+', Dimension _ (NVInteger a) "n"] = Nth x a b
parseNth x [Dimension _ (NVInteger a) "n", Delim '-', Number _ (NVInteger b)] = Nth x a $ negate b
parseNth x [Number _ (NVInteger b), Delim '-', Dimension _ (NVInteger a) "n"] = Nth x a $ negate b
parseNth _ _ = Fail

nthChild :: Bool -> Int -> (Element -> Bool) -> Element -> Bool
nthChild ofType n success el | countPrev ofType el == n = success el
    | otherwise = False
nthChild' :: Bool -> Int -> Int -> (Element -> Bool) -> Element -> Bool
nthChild' ofType a b success el | countPrev ofType el `rem` a == b = success el
    | otherwise = False
countPrev :: Bool -> Element -> Int
countPrev ofType el =
    length [el' | el' <- maybeStar previous el, name el == name el' || not ofType]
maybeStar :: (t -> Maybe t) -> t -> [t]
maybeStar cb x | Just y <- cb x = x : maybeStar cb y
    | otherwise = [x]

testRoot :: (Element -> Bool) -> Element -> Bool
testRoot cb el | Just _ <- parent el = cb el
    | otherwise = False
--------
---- RuleStore wrapper
--------
-- | Compiles & fully evaluates CSS selectors.
data InterpretedRuleStore inner = InterpretedRuleStore inner
instance RuleStore inner => RuleStore (InterpretedRuleStore inner) where
    new = InterpretedRuleStore new
    addStyleRule (InterpretedRuleStore self) priority rule =
        InterpretedRuleStore $ addStyleRule self priority $ rule {
            compiledSelector = compile $ selector rule
        }
    lookupRules (InterpretedRuleStore self) el = filter call $ lookupRules self el
        where call (StyleRule' _ test _) = test el
