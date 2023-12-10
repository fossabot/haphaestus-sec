{-# LANGUAGE OverloadedStrings #-}
module Data.CSS.Style.Selector.LowerWhere(
        WhereLowerer(..)
    ) where

import Data.CSS.Syntax.Selector
import Data.CSS.Style.Common

lowerSelector :: Selector -> [Selector]
lowerSelector (Element [Psuedoclass c args])
    | c `elem` ["is", "where"], (args', []) <- parseSelectors args = args'
lowerSelector (Element sel) = [Element $ lowerSelector' sel]
lowerSelector (Child sel x) = [Child sel' $ lowerSelector' x | sel' <- lowerSelector sel]
lowerSelector (Descendant sel x) = [Descendant sel' $ lowerSelector' x | sel' <- lowerSelector sel]
lowerSelector (Adjacent sel x) = [Adjacent sel' $ lowerSelector' x | sel' <- lowerSelector sel]
lowerSelector (Sibling sel x) = [Sibling sel' $ lowerSelector' x | sel' <- lowerSelector sel]

lowerSelector' :: [SimpleSelector] -> [SimpleSelector]
lowerSelector' (Psuedoclass c args:sel)
    | c `elem` ["is", "where"], ([Element arg'], []) <- parseSelectors args =
        arg' ++ lowerSelector' sel
lowerSelector' (test:tests) = test : lowerSelector' tests
lowerSelector' [] = []

data WhereLowerer s = WhereLowerer s

instance RuleStore s => RuleStore (WhereLowerer s) where
    new = WhereLowerer new
    addStyleRule (WhereLowerer self) priority rule =
        WhereLowerer $ foldl addStyleRule' self $ lowerSelector sel
      where
        addStyleRule' self' sel' = addStyleRule self' priority $ rule {
            inner = StyleRule sel' props psuedo
        }
        StyleRule sel props psuedo = inner rule
    lookupRules (WhereLowerer self) el = lookupRules self el
