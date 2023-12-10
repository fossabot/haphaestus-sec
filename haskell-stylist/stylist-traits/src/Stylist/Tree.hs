-- | Abstracts away tree traversals.
-- Mostly used by callers including (soon) XML Conduit Stylist,
-- but also used internally for generating counter text.
module Stylist.Tree(StyleTree(..), treeOrder, treeOrder',
    Path, treeMap, treeFind, treeFlatten, treeFlattenAll, preorder, preorder', postorder) where

-- | A generic tree, variable numbers of children.
data StyleTree p = StyleTree {
    style :: p,
    children :: [StyleTree p]
}

-- | Indices within the tree.
type Path = [Integer]
-- | Preorder traversal of the tree.
treeOrder :: (c -> c -> Path -> p -> (c, p')) ->
    c -> StyleTree p -> StyleTree p'
treeOrder cb ctxt tree = StyleTree
    (snd $ cb ctxt ctxt [] $ style tree)
    (snd $ treeOrder' cb ctxt ctxt [0] $ children tree)
-- | Preorder traversal of the tree managing per-layer contexts.
treeOrder' :: (c -> c -> Path -> p -> (c, p')) ->
    c -> c -> Path -> [StyleTree p] -> (c, [StyleTree p'])
treeOrder' cb prevContext context (num:path) (node:nodes) = (tailContext, StyleTree node' children' : nodes')
    where
        (selfContext, node') = cb prevContext context (num:path) $ style node
        (childContext, children') = treeOrder' cb selfContext selfContext (0:num:path) $ children node
        (tailContext, nodes') = treeOrder' cb selfContext childContext (num + 1:path) nodes
treeOrder' _ _ context _ [] = (context, [])
treeOrder' _ _ _ [] _ = error "Invalid path during tree traversal!"

-- | Runs a callback over all `style` properties in the tree.
treeMap :: (p -> p') -> StyleTree p -> StyleTree p'
treeMap cb = treeOrder (\_ _ _ p -> ((), cb p)) ()

-- | Flatten a styletree into a list.
treeFlatten :: StyleTree p -> [p]
treeFlatten = treeFlatten' . children
-- | Flatten a list of styletrees into a list.
treeFlatten' :: [StyleTree p] -> [p]
treeFlatten' (StyleTree p []:ps) = p : treeFlatten' ps
treeFlatten' (StyleTree _ childs:sibs) = treeFlatten' childs ++ treeFlatten' sibs
treeFlatten' [] = []

-- | Flatten a styletree into a list, including parent nodes.
treeFlattenAll :: StyleTree p -> [p]
treeFlattenAll = treeFlattenAll' . children
-- | Flatten styletrees into a list, including parent nodes.
treeFlattenAll' :: [StyleTree p] -> [p]
treeFlattenAll' (StyleTree p []:ps) = p : treeFlattenAll' ps
treeFlattenAll' (StyleTree p childs:sibs) = p : treeFlattenAll' childs ++ treeFlattenAll' sibs
treeFlattenAll' [] = []

-- | Find the styltree node matching the given predicate.
treeFind :: StyleTree p -> (p -> Bool) -> [p]
treeFind p test = filter test $ treeFlattenAll p

-- | Preorder traversal over a tree, without tracking contexts.
preorder :: (Maybe b -> Maybe b -> a -> b) -> StyleTree a -> StyleTree b
preorder cb self = head $ preorder' cb Nothing Nothing [self]
-- | Variant of `preorder` with given parent & previous-sibling.
preorder' :: (Maybe b -> Maybe b -> a -> b) ->
        Maybe b -> Maybe b -> [StyleTree a] -> [StyleTree b]
preorder' cb parent previous (self:sibs) = let self' = cb parent previous $ style self
        in StyleTree self' (preorder' cb (Just self') Nothing $ children self) :
            preorder' cb parent (Just self') sibs
preorder' _ _ _ [] = []

-- | Postorder traversal over the tree.
postorder :: (a -> [b] -> [b]) -> StyleTree a -> [StyleTree b]
postorder cb (StyleTree self childs) =
    [StyleTree self' children' | self' <- cb self $ Prelude.map style children']
  where children' = concat $ Prelude.map (postorder cb) childs
