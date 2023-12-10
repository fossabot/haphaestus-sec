-- | Abstracts away tree traversals.
-- Mostly used by callers including (soon) XML Conduit Stylist,
-- but also used internally for generating counter text.
--
-- Backwards compatability module, this API has been moved out into "stylist-traits".
-- Though it also contains integration between the styletree & styling APIs.
{-# LANGUAGE OverloadedStrings #-}
module Data.CSS.StyleTree(StyleTree(..), treeOrder, treeOrder',
    Path, treeMap, treeFlatten, preorder, preorder', postorder,
    stylize, inlinePseudos) where

import Stylist.Tree -- Mainly for reexports

import Stylist
import Data.CSS.Style
import Data.CSS.Syntax.StyleSheet (parseProperties')
import Data.CSS.Syntax.Tokens
import Data.Text (Text, pack)
import Data.HashMap.Strict as M (toList)
import Data.Maybe (fromMaybe)

stylize :: PropertyParser s => QueryableStyleSheet s -> StyleTree Element -> StyleTree [(Text, s)]
stylize = preorder . stylize'
stylize' :: PropertyParser s => QueryableStyleSheet s -> Maybe [(Text, s)] -> Maybe [(Text, s)] ->
        Element -> [(Text, s)]
stylize' stylesheet parent' _ el = ("", base) : [
        (k, cascade' v [] base) | (k, v) <- M.toList $ queryRules stylesheet el
    ] where
        base = cascade stylesheet el overrides $ fromMaybe temp $ lookup "" =<< parent'
        overrides = concat [fst $ parseProperties' $ tokenize $ pack val
            | Attribute "style" _ val <- attributes el]

inlinePseudos :: PropertyParser s => StyleTree [(Text, VarParser s)] -> StyleTree s
inlinePseudos (StyleTree self childs) = StyleTree {
        style = fromMaybe temp $ innerParser <$> lookup "" self,
        children = pseudo "before" ++ map inlinePseudos childs ++ pseudo "after"
    } where
        pseudo n
            | Just sty <- innerParser <$> lookup n self,
                Just style' <- longhand sty sty "::" [Ident n] = [StyleTree style' []]
            | Just sty <- innerParser <$> lookup n self = [StyleTree sty []]
            | otherwise = []
