{-# LANGUAGE OverloadedStrings #-}
-- | Applies CSS selection, cascade, & inheritance.
-- INTERNAL MODULE.
module Data.CSS.Style.Cascade(
        query, cascade, cascadeWithParent,
        TrivialPropertyParser(..), PropertyParser(..), Props
    ) where

import Data.CSS.Style.Common
import Data.CSS.Syntax.Tokens
import Stylist (PropertyParser(..), Props)

-- TODO do performance tests to decide beside between strict/lazy,
--      or is another Map implementation better?
import Data.Hashable (Hashable)
import Data.HashMap.Strict as HM
import qualified Data.HashMap.Lazy as HML
import Data.Text (unpack, pack, isPrefixOf)

-- | Gather properties into a hashmap.
data TrivialPropertyParser = TrivialPropertyParser (HashMap String [Token]) deriving (Show, Eq)
instance PropertyParser TrivialPropertyParser where
    temp = TrivialPropertyParser empty
    longhand _ (TrivialPropertyParser self) key value =
        Just $ TrivialPropertyParser $ insert (unpack key) value self

--------
---- Query/Psuedo-elements
--------

-- | Looks up style rules for an element, grouped by psuedoelement.
query :: RuleStore s => s -> Element -> HashMap Text [StyleRule']
query self el = Prelude.foldr yield empty $ lookupRules self el
    where yield rule store = insertWith (++) (psuedoElement rule) [resolveAttr rule el] store

--------
---- Cascade/Inheritance
--------

-- | Applies cascade for the given `StyleRule'`s & explicit styles,
-- parsed to a value of the same `PropertyParser` type passed in & inheriting from it.
cascade :: PropertyParser p => [StyleRule'] -> Props -> p -> p
cascade styles overrides base =
    construct base $ HML.toList $ cascadeRules (getVars base ++ overrides) styles
-- | Variant of `cascade` which allows configuring base styles seperate from parent.
cascadeWithParent :: PropertyParser p => [StyleRule'] -> Props -> p -> p -> p
cascadeWithParent styles overrides parent' base = constructWithParent parent' base $
    toPrioList (priority base) $ cascadeRules (getVars base ++ overrides) styles

cascadeRules :: Props -> [StyleRule'] -> HashMap Text [Token]
cascadeRules overrides rules = cascadeProperties overrides $ concat $ Prelude.map properties rules
cascadeProperties :: Props -> Props -> HashMap Text [Token]
cascadeProperties overrides props = HML.fromList (props ++ overrides)

toPrioList :: Hashable k => [k] -> HashMap k v -> [(k, v)]
toPrioList (key:keys) map
    | Just val <- key `HM.lookup` map =
        (key, val):toPrioList keys (delete key map)
    | otherwise = toPrioList keys map
toPrioList [] map = toList map

constructWithParent :: PropertyParser p => p -> p -> Props -> p
constructWithParent parent' base props = dispatch parent' child props
    where child = setVars [item | item@(n, _) <- props, isPrefixOf "--" n] base
construct :: PropertyParser p => p -> Props -> p
construct base props = constructWithParent base (inherit base) props
dispatch :: PropertyParser p => p -> p -> Props -> p
dispatch base child ((key, value):props)
    | Just child' <- longhand base child key value = dispatch base child' props
    | otherwise = dispatch base child props
dispatch _ child [] = child

--------
---- attr()
--------
resolveAttr :: StyleRule' -> Element -> StyleRule'
resolveAttr self el = self {
        inner = StyleRule sel [(n, resolveAttr' v $ attrs2Dict el) | (n, v) <- attrs] psuedo
    } where StyleRule sel attrs psuedo = inner self

attrs2Dict :: Element -> HashMap Text String
attrs2Dict el = fromList [(a, b) | Attribute a _ b <- attributes el]

resolveAttr' :: [Token] -> HashMap Text String  -> [Token]
resolveAttr' (Function "attr":Ident attr:RightParen:toks) attrs =
    String (pack $ lookupDefault "" attr attrs) : resolveAttr' toks attrs
resolveAttr' (tok:toks) attrs = tok : resolveAttr' toks attrs
resolveAttr' [] _ = []
