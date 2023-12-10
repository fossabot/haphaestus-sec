module Data.CSS.Syntax.AtLayer(parseAtLayer, Tree(..),
    registerLayer, layerPath, uniqueName, emptyTree) where

import Data.HashMap.Lazy as M (HashMap, (!?), insert, size, empty)
import Data.Text as T hiding (reverse, replicate, length)
import Data.CSS.Syntax.Tokens

import Stylist.Parse

parseAtLayer :: StyleSheet s => [Text] -> [Token] -> Tree ->
    ([Text] -> [Int] -> s) -> (Tree, Maybe s, [Token])
parseAtLayer namespace (Whitespace:toks) tree cb = parseAtLayer namespace toks tree cb
parseAtLayer namespace (Ident layer:toks) tree cb = inner toks [layer] tree
    where
        inner (Delim '.':Ident sublayer:toks') layers tree' =  inner toks' (sublayer:layers) tree'
        inner (Whitespace:toks') layers tree' = inner toks' layers tree'
        inner (Comma:toks') layers tree' =
            let (ret, tail') = parseLayerStmt namespace toks' $registerLayer (namespaced layers) tree'
            in (ret, Nothing, tail')
        inner (LeftCurlyBracket:toks') layers  tree' =
            let (ret, styles, tail') = parseLayerBlock (namespaced layers) toks' tree' cb
            in (ret, Just styles, tail')
        inner (Semicolon:toks') layers tree' = (registerLayer (namespaced layers) tree', Nothing, toks')
        inner [] layers tree' = (registerLayer (namespaced layers) tree', Nothing, [])
        inner toks' _ _ = (tree, Nothing, skipAtRule toks')
        namespaced layers = namespace ++ reverse layers
parseAtLayer ns (LeftCurlyBracket:toks) tree cb = 
    let (ret, styles, tail') = parseLayerBlock (uniqueName ns tree) toks tree cb
    in (ret, Just styles, tail')
parseAtLayer _ toks tree _ = (tree, Nothing, skipAtRule toks)

parseLayerStmt :: [Text] -> [Token] -> Tree -> (Tree, [Token])
parseLayerStmt namespace (Whitespace:toks) tree = parseLayerStmt namespace toks tree
parseLayerStmt namespace (Ident layer:toks) tree = inner toks [layer] tree
    where
        inner (Delim '.':Ident sublayer:toks') layers tree' = inner toks' (sublayer:layers) tree'
        inner (Comma:toks') layers tree' =
            parseLayerStmt namespace toks' $ registerLayer (namespaced layers) tree'
        inner (Whitespace:toks') layers tree' = inner toks' layers tree'
        inner (Semicolon:toks') layers tree' = (registerLayer (namespaced layers) tree', toks')
        inner [] layers tree' = (registerLayer (namespaced layers) tree', [])
        inner toks' _ _ = (tree, skipAtRule toks')
        namespaced layers = namespace ++ reverse layers
parseLayerStmt _ toks tree = (tree, skipAtRule toks)

parseLayerBlock :: StyleSheet s => [Text] -> [Token] -> Tree ->
    ([Text] -> [Int] -> s) -> (Tree, s, [Token])
parseLayerBlock layers toks tree cb = (tree', parse' styles block, toks')
    where
        (block, toks') = scanBlock toks
        tree' = registerLayer layers tree
        styles = cb layers $ layerPath layers tree'

newtype Tree = Tree (HashMap Text (Int, Tree))
registerLayer :: [Text] -> Tree -> Tree
registerLayer (layer:sublayers) (Tree self)
    | Just (ix, subtree) <- self !? layer = Tree $ insert layer (ix, registerLayer sublayers subtree) self
    | otherwise = Tree $ insert layer (succ $ size self, registerLayer sublayers $ Tree M.empty) self
registerLayer [] self = self

layerPath :: [Text] -> Tree -> [Int]
layerPath (layer:sublayers) (Tree self)
    | Just (ix, subtree) <- self !? layer = ix:layerPath sublayers subtree
    | otherwise = [] -- Should have registered first...
layerPath [] _ = []

uniqueName :: [Text] -> Tree -> [Text]
uniqueName (namespace:namespaces) (Tree self) 
    | Just (_, subtree) <- self !? namespace = namespace:uniqueName namespaces subtree
    | otherwise = replicate (length namespaces + 2) T.empty -- Should have registered first
uniqueName [] (Tree self) = [T.pack $ show $ size self]

emptyTree :: Tree
emptyTree = Tree M.empty
