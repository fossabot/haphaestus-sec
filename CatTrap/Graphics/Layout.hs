{-# LANGUAGE OverloadedStrings, RecordWildCards, StandaloneDeriving #-}
-- | Generic layout logic, handling a hierarchy of varying formulas.
-- Unless callers have more specific needs they probably wish to use this abstraction.
-- Attempts to follow the CSS specs.
-- See `boxLayout` for a main entrypoint,
-- & `Graphics.Layout.CSS` to receive CSS input.
module Graphics.Layout(LayoutItem(..), UserData,
        layoutGetBox, layoutGetChilds, layoutGetInner,
        boxMinWidth, boxMaxWidth, boxNatWidth, boxWidth,
        boxNatHeight, boxMinHeight, boxMaxHeight, boxHeight,
        boxSplit, boxPaginate, boxPosition, boxLayout,
        glyphs, codepoints, fragmentFont, glyphsPerFont) where

import Data.Text.ParagraphLayout.Rich (Paragraph(..), ParagraphOptions(..),
                                ParagraphLayout(..), layoutRich)
import Data.Text.ParagraphLayout (paginate, PageContinuity(..), PageOptions(..))
import Stylist (PropertyParser(temp))
import Control.Parallel.Strategies
import Control.DeepSeq (NFData(..))

import Graphics.Layout.Box as B
import Graphics.Layout.Grid as G
import Graphics.Layout.Flow as F
import Graphics.Layout.Inline as I
import Graphics.Layout.CSS.Font (Font'(..))
import Graphics.Layout.Flex as Fl

import Data.Maybe (fromMaybe)

-- To gather glyphs for atlases.
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import qualified Data.Text.Glyphize as Hb
import Graphics.Text.Font.Choose (Pattern)

-- For comparisons
import Data.Array.Byte (ByteArray(..))
import Data.Text.Array (Array(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Additional data routed through Balkon.
type UserData m n x = ((Font', Int), PaddedBox m n, x)

-- | A tree of different layout algorithms.
-- More to come...
data LayoutItem m n x =
    -- | A block element. With margins, borders, & padding.
    LayoutFlow x (PaddedBox m n) [LayoutItem m n x]
    -- | A grid or table element.
    | LayoutGrid x (Grid m n) [GridItem] [LayoutItem m n x]
    -- | Some richtext. (Balkón holds children)
    | LayoutInline x (Paragraph (UserData m n x)) PageOptions
    -- | Results laying out richtext, has fixed width.
    -- Generated from `LayoutInline` for the sake of pagination.
    | LayoutInline' x (ParagraphLayout (UserData m n x)) PageOptions
    -- | A branch with constant bounding box.
    -- Generated from `LayoutInline` when attaching position info.
    | LayoutConst x (PaddedBox m n) [LayoutItem m n x]
    -- | Children of a `LayoutInline` or `LayoutInline'`.
    | LayoutSpan (FragmentTree (UserData m n x))
    | LayoutFlex x (FlexParent (LayoutItem m n x) m)
    deriving (Show, Eq)
-- | An empty box.
nullLayout :: (PropertyParser x, Zero m, Zero n) => LayoutItem m n x
nullLayout = LayoutFlow temp zero []

instance (Zero m, CastDouble m, NFData m, Zero n, CastDouble n, NFData n) =>
        NFData (LayoutItem m n x) where
    rnf = rnf . layoutGetBox -- Avoid auxiliary properties that don't cleanly `rnf`

-- | Retrieve the surrounding box for a layout item.
layoutGetBox :: (Zero m, Zero n, CastDouble m, CastDouble n) =>
        LayoutItem m n x -> PaddedBox m n
layoutGetBox (LayoutFlow _ ret _) = ret
layoutGetBox (LayoutGrid _ self _ _) = zero {
    B.min = Size (fromDouble $ trackMin toDouble $ inline self)
            (fromDouble $ trackMin toDouble $ block self),
    B.size = Size (fromDouble $ trackNat toDouble $ inline self)
            (fromDouble $ trackNat toDouble $ block self),
    B.max = Size (fromDouble $ trackNat toDouble $ inline self)
            (fromDouble $ trackNat toDouble $ block self)
}
layoutGetBox (LayoutInline _ self _) = zero {
    B.min = inlineMin self, B.size = inlineSize self, B.max = inlineSize self
}
layoutGetBox (LayoutInline' _ self _) = zero {
    B.min = layoutSize self, B.size = layoutSize self, B.max = layoutSize self
}
layoutGetBox (LayoutSpan self) = treeBox self
layoutGetBox (LayoutConst _ ret _) = ret
layoutGetBox (LayoutFlex _ self) = flexGetBox layoutGetBox' self
layoutGetBox' :: (Zero m, Zero n, CastDouble m, CastDouble n) =>
        LayoutItem m n x -> PaddedBox Double Double
layoutGetBox' = mapX' toDouble . mapY' toDouble . layoutGetBox
-- | Retrieve the subtree under a node.
layoutGetChilds (LayoutFlow _ _ ret) = ret
layoutGetChilds (LayoutGrid _ _ _ ret) = ret
layoutGetChilds (LayoutSpan (Leaf _)) = []
layoutGetChilds (LayoutSpan (Branch _ ret)) = map LayoutSpan ret
layoutGetChilds (LayoutInline _ self _) = map LayoutSpan $ inlineChildren self
layoutGetChilds (LayoutInline' _ self _) = map LayoutSpan $ layoutChildren self
layoutGetChilds (LayoutConst _ _ childs) = childs
layoutGetChilds (LayoutFlex _ x) = map Fl.flexInner $ concat $ Fl.children x
-- | Retrieve the caller-specified data attached to a layout node.
layoutGetInner (LayoutFlow ret _ _) = ret
layoutGetInner (LayoutGrid ret _ _ _) = ret
layoutGetInner (LayoutInline ret _ _) = ret
layoutGetInner (LayoutInline' ret _ _) = ret
layoutGetInner (LayoutConst ret _ _) = ret
layoutGetInner (LayoutSpan x) = treeInner x
layoutGetInner (LayoutFlex ret _ ) = ret

-- | Retrieve the font associated with inline layout.
fragmentFont x = let (ret, _, _) = treeInner' x in ret

-- | map-ready wrapper around `setCellBox` sourcing from a child node.
setCellBox' (child, cell) = setCellBox cell $ layoutGetBox child

-- | Update a (sub)tree to compute & cache minimum legible sizes.
boxMinWidth :: (Zero y, CastDouble y, NFData y) =>
        Maybe Double -> LayoutItem y Length x -> LayoutItem y Length x
boxMinWidth parent (LayoutFlow val self childs) = LayoutFlow val self' childs'
  where
    self' = self { B.min = mapSizeX (B.mapAuto min') (B.min self) }
    min' = flowMinWidth parent' self childs''
    childs'' = map (mapX' $ lowerLength selfWidth) $ map layoutGetBox childs'
    childs' = parMap' (boxMinWidth $ Just selfWidth) childs
    selfWidth = width $ mapX' (lowerLength parent') self
    parent' = fromMaybe 0 parent
boxMinWidth parent (LayoutGrid val self cells0 childs) = LayoutGrid val self' cells' childs'
  where
    self' = Size (inline self) { trackMins = cells } (block self)
    cells = sizeTrackMins parent' (inline self) $ map inline cells'
    cells' = map setCellBox' $ zip childs' cells0 -- Flatten subgrids
    childs'' = map (mapX' $ lowerLength selfWidth) $ map layoutGetBox childs'
    childs' = parMap' (boxMinWidth $ Just selfWidth) childs
    selfWidth = trackNat (lowerLength parent') $ inline self
    parent' = fromMaybe (gridEstWidth self cells0) parent
    zeroBox :: PaddedBox Double Double
    zeroBox = zero
boxMinWidth _ self@(LayoutInline _ _ _) = self
boxMinWidth _ self@(LayoutInline' _ _ _) = self
boxMinWidth _ (LayoutConst val self' childs) =
    LayoutConst val self' $ map (boxMinWidth Nothing) childs
boxMinWidth _ self@(LayoutSpan _) = self
boxMinWidth size self@(LayoutFlex a b) = LayoutFlex a $ flexMap (boxMinWidth size) b
-- | Update a (sub)tree to compute & cache ideal width.
boxNatWidth :: (Zero y, CastDouble y, NFData y) =>
        Maybe Double -> LayoutItem y Length x -> LayoutItem y Length x
boxNatWidth parent (LayoutFlow val self childs) = LayoutFlow val self' childs'
  where
    self' = self { B.nat = Size size' $ block $ B.nat self }
    size' = flowNatWidth parent' self childs''
    childs'' = map (mapX' $ lowerLength selfWidth) $ map layoutGetBox childs'
    childs' = parMap' (boxNatWidth $ Just selfWidth) childs
    selfWidth = width $ mapX' (lowerLength parent') self
    parent' = fromMaybe 0 parent
boxNatWidth parent (LayoutGrid val self cells0 childs) = LayoutGrid val self' cells' childs'
  where
    self' = Size (inline self) { trackNats = cells } (block self)
    cells = sizeTrackNats parent' (inline $ self) $ map inline cells'
    cells' = map setCellBox' $ zip childs' cells0 -- Flatten subgrids
    childs'' = map (mapX' $ lowerLength selfWidth) $ map layoutGetBox childs'
    childs' = parMap' (boxNatWidth $ Just selfWidth) childs
    selfWidth = trackNat (lowerLength parent') $ inline self
    parent' = fromMaybe (gridEstWidth self cells0) parent
    zeroBox :: PaddedBox Double Double
    zeroBox = zero
boxNatWidth _ self@(LayoutInline _ _ _) = self
boxNatWidth _ self@(LayoutInline' _ _ _) = self
boxNatWidth _ (LayoutConst val self' childs) =
    LayoutConst val self' $ map (boxNatWidth Nothing) childs
boxNatWidth _ self@(LayoutSpan _) = self
boxNatWidth size (LayoutFlex a b ) = LayoutFlex a $ flexMap (boxNatWidth size) b
-- | Update a (sub)tree to compute & cache maximum legible width.
boxMaxWidth :: (CastDouble y, Zero y, NFData y) =>
        PaddedBox a Double -> LayoutItem y Length x -> LayoutItem y Length x
boxMaxWidth parent (LayoutFlow val self childs) = LayoutFlow val self' childs'
  where
    childs' = parMap' (boxMaxWidth self'') childs
    self'' = mapX' (lowerLength $ inline $ B.size parent) self'
    self' = self { B.max = Size (Pixels max') (block $ B.max self) }
    max' = flowMaxWidth parent self
boxMaxWidth parent (LayoutGrid val self cells childs) = LayoutGrid val self cells childs'
  where -- Propagate parent track as default.
    childs' = parMap' inner $ zip cells childs
    inner (Size cellx celly, child) =
        boxMaxWidth (cellSize (inline self) cellx `size2box` cellSize (block self) celly) child
    size2box x y = zeroBox { B.min = Size x y, B.max = Size x y, B.size = Size x y }
boxMaxWidth parent self@(LayoutInline _ _ _) = self
boxMaxWidth parent self@(LayoutInline' _ _ _) = self
boxMaxWidth _ (LayoutConst val self' childs) = LayoutConst val self' $
    map (boxMaxWidth $ mapY' toDouble $ mapX' toDouble self') childs
boxMaxWidth parent self@(LayoutSpan _) = self
boxMaxWidth parent (LayoutFlex a b) = LayoutFlex a $ (case Fl.direction b of
    Fl.Row -> flip flexWrap (inline $ B.size parent)
    Fl.Column -> id) $ flexMap (boxMaxWidth parent) b
-- | Update a (sub)tree to compute & cache final width.
boxWidth :: (Zero y, CastDouble y, NFData y) =>
        PaddedBox b Double -> LayoutItem y Length x -> LayoutItem y Double x
boxWidth parent (LayoutFlow val self childs) = LayoutFlow val self' childs'
  where
    childs' = parMap' (boxWidth self') childs
    self' = (mapX' (lowerLength $ inline $ size parent) self) {
        size = Size size' $ block $ B.max self
      }
    size' = flowWidth parent self
boxWidth parent (LayoutGrid val self cells childs) = LayoutGrid val self' cells' childs'
  where -- Propagate parent track as default
    (cells', childs') = unzip $ parMap' recurse $ zip cells childs
    recurse (cell, child) = (cell', child')
      where
        cell' = setCellBox cell $ layoutGetBox child'
        child' = boxWidth (gridItemBox self cell) child
    self' = flip Size (block self) Track {
        cells = map Left widths,
        trackMins = trackMins $ inline self, trackNats = trackNats $ inline self,
        gap = lowerLength outerwidth $ gap $ inline self
    }
    outerwidth = inline $ size parent
    widths = sizeTrackMaxs (inline $ size parent) $ inline self
boxWidth parent (LayoutInline val (Paragraph a b c d) paging) =
    LayoutInline val (paragraphMap (mapX' $ lowerLength width) $
        Paragraph a b c d { paragraphMaxWidth = round width }) paging
  where width = B.inline $ B.size parent
boxWidth p (LayoutInline' a b c) =
    LayoutInline' a (layoutMap (mapX' $ lowerLength $ B.inline $ B.size p) b) c
boxWidth p (LayoutConst val self childs) = LayoutConst val (mapX' cb self) $
    map (boxWidth $ mapY' toDouble $ mapX' cb self) childs
  where cb = lowerLength $ width p
boxWidth parent (LayoutSpan self') =
    LayoutSpan $ treeMap (mapX' $ lowerLength $ width parent) self'
boxWidth parent (LayoutFlex a b) = LayoutFlex a $ flexMap (boxWidth parent) b

-- | Update a (sub)tree to compute & cache ideal legible height.
boxNatHeight :: Double -> LayoutItem Length Double x -> LayoutItem Length Double x
boxNatHeight parent (LayoutFlow val self childs) = LayoutFlow val self' childs'
  where
    self' = self { size = mapSizeY (mapAuto size') (size self) }
    size' = flowNatHeight parent self childs''
    childs'' = map (mapY' (lowerLength parent)) $ map layoutGetBox childs'
    childs' = parMap' (boxNatHeight $ inline $ size self) childs
boxNatHeight parent (LayoutGrid val self cells childs) = LayoutGrid val self' cells childs'
  where
    self' = Size (inline self) (block self) { trackNats = heights }
    heights = sizeTrackNats parent (block self) $ map block cells'
    cells' = map setCellBox' $ zip childs' cells -- Flatten subgrids
    childs' = parMap' (boxNatHeight width) childs
    width = trackNat id $ inline self
boxNatHeight parent self@(LayoutInline _ _ _) = self
boxNatHeight parent self@(LayoutInline' _ _ _) = self
boxNatHeight p (LayoutConst val self' childs) = LayoutConst val self' $
    map (boxNatHeight $ width $ mapY' (lowerLength p) self') childs
boxNatHeight parent self@(LayoutSpan _) = self
boxNatHeight parent (LayoutFlex a b) = LayoutFlex a $ flexMap (boxNatHeight parent) b
-- | Update a (sub)tree to compute & cache minimum legible height.
boxMinHeight :: Double -> LayoutItem Length Double x -> LayoutItem Length Double x
boxMinHeight parent (LayoutFlow val self childs) = LayoutFlow val self' childs'
  where
    childs' = parMap' (boxMinHeight $ inline $ size self) childs
    self' = self { B.min = Size (inline $ B.min self) (Pixels min') }
    min' = flowMinHeight parent self
boxMinHeight parent (LayoutGrid val self cells childs) = LayoutGrid val self' cells' childs'
  where
    (cells', childs') = unzip $ parMap' recurse $ zip cells childs
    recurse (cell, child) = (cell', child') -- Propagate track into subgrids.
      where
        cell' = setCellBox cell (layoutGetBox child')
        child' = boxMinHeight width child
    self' = Size (inline self) (block self) { trackMins = heights }
    heights = sizeTrackMins width (block self) $ map block cells
    width = trackNat id $ inline self
boxMinHeight parent self@(LayoutInline _ _ _) = self
boxMinHeight _ self@(LayoutInline' _ _ _) = self
boxMinHeight p (LayoutConst val self' childs) = LayoutConst val self' $
    map (boxMinHeight $ width $ mapY' (lowerLength p) self') childs
boxMinHeight parent self@(LayoutSpan _) = self
boxMinHeight parent self@(LayoutFlex a b) = LayoutFlex a $ flexMap (boxMinHeight parent) b
-- | Update a subtree to compute & cache maximum legible height.
boxMaxHeight :: PaddedBox Double Double -> LayoutItem Length Double x ->
        LayoutItem Length Double x
boxMaxHeight parent (LayoutFlow val self childs) = LayoutFlow val self' childs'
  where
    childs' = parMap' (boxMaxHeight $ mapY' (lowerLength width) self') childs
    self' = self { B.max = Size (inline $ B.max self) (Pixels max') }
    max' = flowMaxHeight (inline $ size parent) self
    width = inline $ size self
boxMaxHeight parent (LayoutGrid val self cells childs) = LayoutGrid val self cells' childs'
  where
    (cells', childs') = unzip $ parMap' recurse $ zip cells childs
    recurse (cell, child) = (cell', child') -- Propagate track into subgrids
      where
        cell' = setCellBox cell (layoutGetBox child')
        child' = boxMaxHeight (gridItemBox self cell) child
    heights = sizeTrackMaxs (inline $ size parent) (block self)
    width = inline $ size parent
boxMaxHeight _ (LayoutInline val self' paging) = LayoutInline val self' paging
boxMaxHeight _ (LayoutInline' val self' paging) = LayoutInline' val self' paging
boxMaxHeight p (LayoutConst val self' childs) = LayoutConst val self' $
    map (boxMaxHeight $ mapY' (lowerLength $ width p) self') childs
boxMaxHeight parent (LayoutSpan self') = LayoutSpan self'
boxMaxHeight parent (LayoutFlex a b) = LayoutFlex a $ flexMap (boxMaxHeight parent) b
-- | Update a (sub)tree to compute & cache final height.
boxHeight :: PaddedBox Double Double -> LayoutItem Length Double x -> LayoutItem Double Double x
boxHeight parent (LayoutFlow val self childs) = LayoutFlow val self' childs'
  where
    childs' = parMap' (boxHeight self') childs
    self' = (mapY' (lowerLength $ inline $ size parent) self) {
        size = Size (inline $ size self) size'
      }
    size' = flowHeight parent self
    width = inline $ size self
boxHeight parent (LayoutGrid val self cells0 childs) = LayoutGrid val self' cells' childs'
  where
    (cells', childs') = unzip $ parMap' recurse $ zip cells0 childs
    recurse (cell, child) = (cell', child') -- Propagate track into subgrids.
      where
        cell' = setCellBox cell (layoutGetBox child')
        child' = boxHeight (layoutGetBox $ LayoutGrid val self' [] []) child
    self' = Size (inline self) Track {
        gap = lowerLength width $ gap $ block self,
        cells = map lowerSize $ cells $ block self,
        trackMins = trackMins $ block self, trackNats = trackNats $ block self
      }
    heights = sizeTrackMaxs (inline $ size parent) $ block self
    lowerSize (Left x) = Left $ lowerLength width x
    lowerSize (Right x) = Right x
    width = inline $ size parent
boxHeight p (LayoutInline val self' paging) =
    LayoutInline val (paragraphMap (mapY' $ lowerLength $ width p) self') paging
boxHeight p (LayoutInline' val self' paging) =
    LayoutInline' val (layoutMap (mapY' $ lowerLength $ width p) self') paging
boxHeight p (LayoutConst val self childs) =
    let self' = mapY' (lowerLength $ width p) self
    in LayoutConst val self' $ map (boxHeight self') childs
boxHeight p (LayoutSpan self') =
    LayoutSpan $ treeMap (mapY' $ lowerLength $ width p) self'
boxHeight p (LayoutFlex a b) = LayoutFlex a $
    flexResolve (innerMain . layoutGetBox) (width p) $ flexMap (boxHeight p) b

-- | Split a (sub)tree to fit within max-height.
-- May take full page height into account.
boxSplit :: PropertyParser x => Double -> Double -> LayoutItem Double Double x ->
    (LayoutItem Double Double x, Maybe (LayoutItem Double Double x))
boxSplit maxheight _ node | height (layoutGetBox node) <= maxheight = (node, Nothing)
boxSplit maxheight pageheight (LayoutFlow val self childs)
    | (next:_) <- childs1, ((y,_):_) <- childs0',
        (tail,Just nextpage) <- boxSplit (maxheight - y) pageheight next =
            (LayoutFlow val self {
                size = (size self) { B.block = y }
            } (childs0 ++ [tail]),
             Just $ LayoutFlow val self {
                size = (size self) { B.block = B.block (size self) - y }
             } (nextpage:childs1))
    | otherwise =
        (LayoutFlow val self { size = (size self) { B.block = maxheight } } childs0,
         Just $ LayoutFlow val self childs1) -- TODO recompute height
  where
    childs0 = map snd childs0'
    childs1 = map snd childs1'
    (childs0', childs1') = break overflowed $ inner 0 childs
    overflowed (y, _) = y >= maxheight
    inner start (child:childs) = (start', child):inner start' childs -- TODO margin collapse?
        where start' = start + height (layoutGetBox child)
    inner _ [] = []
boxSplit _ _ self@(LayoutConst _ _ _) = (self, Nothing) -- Doesn't split.
boxSplit _ _ self@(LayoutGrid _ _ _ _) = (self, Nothing) -- TODO
boxSplit maxheight pageheight (LayoutInline a self b) =
    boxSplit maxheight pageheight $ LayoutInline' a (layoutRich self) b
boxSplit maxheight pageheight (LayoutInline' a self paging) =
    case paginate paging {
            pageCurrentHeight = toEnum $ fromEnum maxheight,
            pageNextHeight = toEnum $ fromEnum pageheight
      } self of
        (Continue, self', next) -> (wrap self', wrap <$> next)
        (Break, _, _) -> (nullLayout, Just $ wrap self)
  where
    wrap self' = LayoutInline' a self' paging
boxSplit _ _ self@(LayoutSpan _) = (self, Nothing) -- Can't split!
boxSplit maxheight pageheight (LayoutFlex a self) =
    -- FIXME: What if any children are too big for the page?
    let (p0, p1) = flexSplit ( B.size . layoutGetBox ) maxheight pageheight self
    in if null $ Fl.children p1
    then (LayoutFlex a p0, Nothing)
    else (LayoutFlex a p0, Just $ LayoutFlex a p1)
-- | Generate a list of pages from a node, splitting subtrees where necessary.
boxPaginate pageheight node
    | (page, Just overflow) <- boxSplit pageheight pageheight node =
        page:boxPaginate pageheight overflow
    | otherwise = [node]

-- | Compute position of all nodes in the (sub)tree relative to a base coordinate.
boxPosition :: (PropertyParser x, Eq x) => (Double, Double) ->
    LayoutItem Double Double x -> LayoutItem Double Double ((Double, Double), x)
boxPosition (x,y) (LayoutFlow val box [LayoutFlex val' self]) =
    LayoutFlow ((x,y), val) box [ -- Obtaining size from parent
        LayoutFlex (pos', val') $
            flexPosition boxPosition boxSize pos' (B.size box) self
    ]
  where
    boxSize box' = let b = layoutGetBox box' in B.width b `Size` B.height b
    pos' = (x + B.leftSpace box, y + B.rightSpace box)
boxPosition pos self@(LayoutFlex val self') =
    LayoutFlex (pos, val) $ flexPosition boxPosition boxSize pos size self'
  where
    boxSize box' = let b = layoutGetBox box' in B.width b `Size` B.height b
    size = B.size $ layoutGetBox self
boxPosition pos@(x, y) (LayoutFlow val self childs) = LayoutFlow (pos, val) self childs'
  where
    childs' = parMap' recurse $ zip pos' childs
    recurse ((Size x' y'), child) = boxPosition (x + x', y + y') child
    pos' = positionFlow $ map layoutGetBox childs
boxPosition pos@(x, y) (LayoutGrid val self cells childs) = LayoutGrid (pos, val) self cells childs'
  where
    childs' = parMap' recurse $ zip pos' childs
    recurse ((x', y'), child) = boxPosition (x + x', y + y') child
    pos' = gridPosition self cells
boxPosition pos@(x, y) (LayoutInline val self paging) =
    boxPosition pos $ LayoutInline' val (layoutRich self) paging
boxPosition pos@(x, y) self@(LayoutInline' val _ _) =
    boxPosition pos $ LayoutConst val (layoutGetBox self) $ layoutGetChilds self
boxPosition pos (LayoutConst val self childs) =
    LayoutConst (pos, val) self $ parMap' (boxPosition pos) childs
boxPosition pos (LayoutSpan self) = LayoutSpan $ positionTree pos self
-- | Compute sizes & position information for all nodes in the (sub)tree.
boxLayout :: (PropertyParser x, Eq x) => PaddedBox Double Double ->
        LayoutItem Length Length x -> Bool -> 
        [LayoutItem Double Double ((Double, Double), x)]
boxLayout parent self paginate = self9
  where
    self0 = boxMinWidth Nothing self
    self1 = boxNatWidth Nothing self0
    self2 = boxMaxWidth parent self1
    self3 = boxWidth parent self2
    self4 = boxNatHeight (inline $ size parent) self3
    self5 = boxMinHeight (inline $ size parent) self4
    self6 = boxMaxHeight parent self5
    self7 = boxHeight parent self6
    self8 | paginate = boxPaginate (block $ size parent) self7
        | otherwise = [self7]
    self9 = map (boxPosition (0, 0)) self8

-- | Compute a mapping from a layout tree indicating which glyphs for which fonts
-- are required.
-- Useful for assembling glyph atlases.
glyphsPerFont :: (CastDouble x, CastDouble y, Eq x, Eq y, Eq z) =>
        LayoutItem x y z -> M.Map (Pattern, Double) IS.IntSet
glyphsPerFont (LayoutSpan self@(Leaf _)) | (_:_) <- glyphs =
        (pattern font, fontSize font) `M.singleton` IS.fromList glyphs
    | otherwise = M.empty
  where
    glyphs = map fromEnum $ codepoints self
    (font, _) = fragmentFont self
glyphsPerFont node = M.unionsWith IS.union $ map glyphsPerFont $ layoutGetChilds node

parMap' :: NFData b => (a -> b) -> [a] -> [b]
parMap' = parMap rdeepseq

------
--- Orphan typeclass instances
------

instance Show (Paragraph x) where
    show (Paragraph arr _ _ _) = show $ asBA arr
deriving instance Show PageOptions
instance Eq (Paragraph x) where
    Paragraph a _ _ _ == Paragraph b _ _ _ = asBA a  == asBA b
deriving instance Eq PageOptions

asBA = unsafeCoerce :: Array -> ByteArray
