{-# LANGUAGE TupleSections #-}
-- | Sizes inline text & extracts positioned children,
-- wraps Balkón for the actual logic.
module Graphics.Layout.Inline(paragraphMap, layoutMap, treeMap,
    inlineMin, inlineSize, inlineChildren, layoutSize, layoutChildren,
    treeBox, positionTree, treeInner, treeInner', glyphs, codepoints,
    FragmentTree(..)) where

import Data.Text.ParagraphLayout.Rich (Paragraph(..), ParagraphOptions(..),
                                Fragment(..), ParagraphLayout(..), AncestorBox(..),
                                InnerNode(..), Box(..), RootNode(..),
                                layoutRich, boxSpacing, BoxSpacing(..),
                                activateBoxSpacing, paragraphSafeWidth)
import Data.Text.ParagraphLayout.Rect (Rect(..),
                                width, height, x_max, x_min, y_min, y_max)
import qualified Data.Text.Glyphize as HB
import Data.Int (Int32)
import Data.Word (Word32)
import Debug.Trace (trace) -- To warn about unexpected branches!

import Graphics.Layout.Box hiding (min, max, width, height)
import qualified Graphics.Layout.Box as Box
import Graphics.Layout.CSS.Font (hbUnit)

-- | Convert from Harfbuzz units to device pixels as a Double
hbScale :: Int32 -> Double
hbScale = (/hbUnit) . fromIntegral
-- | Convert from Harfbuzz units to device pixels as a Double or Length.
c :: CastDouble a => Int32 -> a
c = fromDouble . hbScale
-- | Convert from a CastDouble in device pixels to Harfbuzz units.
unscale :: CastDouble x => x -> Int32
unscale = floor . (*hbUnit) . toDouble

-- | Compute minimum width & height for some richtext.
inlineMin :: (CastDouble x, CastDouble y) =>
        Paragraph (a, PaddedBox x y, c) -> Size x y
inlineMin = layoutSize' . flip layoutRich' 0
-- | Compute width & height of some richtext at configured width.
inlineSize :: (CastDouble x, CastDouble y) =>
        Paragraph (a, PaddedBox x y, c) -> Size x y
inlineSize self@(Paragraph _ _ _ opts) =
    layoutSize' . layoutRich' self $ paragraphMaxWidth opts
-- | Retrieve children out of some richtext,
-- associating given userdata with them.
inlineChildren :: (CastDouble x, CastDouble y, Eq x, Eq y, Eq a, Eq c) =>
        Paragraph (a, PaddedBox x y, c) -> [FragmentTree (a, PaddedBox x y, c)]
inlineChildren self = layoutChildren $ layoutRich $ lowerSpacing self

-- | Retrieve a laid-out paragraph's rect & convert to CatTrap types.
layoutSize :: (CastDouble x, CastDouble y) => ParagraphLayout a -> Size x y
layoutSize = layoutSize' . paragraphRect
layoutSize' r = Size (c $ width r) (c $ height r)
-- | Retrieve a laid-out paragraph's children & associate with given userdata.
layoutChildren :: Eq a => ParagraphLayout a -> [FragmentTree a]
layoutChildren self = reconstructTree self

-- | Layout a paragraph at given width & retrieve resulting rect.
-- LEGACY.
layoutRich' :: (CastDouble m, CastDouble n) =>
        Paragraph (a, PaddedBox m n, c) -> Int32 -> Rect Int32
layoutRich' (Paragraph a b c d) width =
    (paragraphRect layout) { x_size = paragraphSafeWidth layout}
  where
    layout = layoutRich$lowerSpacing$Paragraph a b c d {paragraphMaxWidth=width}

-- | Copy surrounding whitespace into Balkon properties.
lowerSpacing :: (CastDouble m, CastDouble n) =>
    Paragraph (a, PaddedBox m n, c) -> Paragraph (a, PaddedBox m n, c)
lowerSpacing (Paragraph a b (RootBox c) d) = Paragraph a b (RootBox $ inner c) d
  where
    inner (Box childs opts) = flip Box opts $ map inner' childs
    inner' (InlineBox e@(_, f, _) child opts) = InlineBox e (inner child) $
        flip activateBoxSpacing opts $
            BoxSpacingLeftRight (leftSpace box) (rightSpace box)
      where box = mapX' unscale $ mapY' unscale f
    inner' self@(TextSequence _ _) = self


-- | A tree extracted from Balkón's inline layout.
data FragmentTree x = Branch (AncestorBox x) [FragmentTree x]
    | Leaf (Fragment x)
    deriving (Show, Eq)

-- | Apply an operation to the 2nd field of the paragraph's userdata,
-- for it's entire subtree.
paragraphMap :: (b -> b') -> Paragraph (a, b, c) -> Paragraph (a, b', c)
paragraphMap cb (Paragraph a b (RootBox c) d) =
    Paragraph a b (RootBox $ inner c) d
  where
    inner (Box childs opts) = flip Box opts $ map inner' childs
    inner' (InlineBox (e, f, g) child opts) =
        InlineBox (e, cb f, g) (inner child) opts
    inner' (TextSequence (e, f, g) leaf) = TextSequence (e, cb f, g) leaf

-- | Apply an operation to the 2nd field of a laid-out paragraph's userdata,
-- for it's entire subtree.
layoutMap :: (b -> b') -> ParagraphLayout (a, b, c) -> ParagraphLayout (a, b', c)
layoutMap cb (ParagraphLayout a b c) = ParagraphLayout a b $ map inner c
  where
    inner self@Fragment { fragmentUserData = (a, b, c) } = self {
        fragmentUserData = (a, cb b, c),
        fragmentAncestorBoxes = map inner' $ fragmentAncestorBoxes self
      }
    inner' self@AncestorBox { boxUserData = (a, b, c) } = self {
        boxUserData = (a, cb b, c)
      }

-- | Apply an operation to the 2nd field of the tree extracted from a laid-out
-- paragraph, for all nodes.
treeMap :: (b -> b') -> FragmentTree (a, b, c) -> FragmentTree (a, b', c)
treeMap cb (Branch self@AncestorBox { boxUserData = (a, b, c) } childs) =
    Branch self { boxUserData = (a, cb b, c) } $ map (treeMap cb) childs
treeMap cb (Leaf self@Fragment { fragmentUserData = (a, b, c) }) =
    Leaf self { fragmentUserData = (a, cb b, c), fragmentAncestorBoxes = [] }

-- | Retrieve the rect for a fragment & convert to CatTrap types.
fragmentSize :: (CastDouble x, CastDouble y) =>
        FragmentTree (a, PaddedBox x y, c) -> Size x y
fragmentSize self = Size (c $ width r) (c $ height r)
    where r = treeRect self
-- | Compute the unioned rect for a subtree.
treeRect :: (CastDouble m, CastDouble n) =>
        FragmentTree (a, PaddedBox m n, c) -> Rect Int32
treeRect (Branch AncestorBox { boxUserData = (_, box', _)} childs) =
        unions $ map treeRect childs
    where
        box :: PaddedBox Int32 Int32
        box = mapX' unscale $ mapY' unscale box'
treeRect (Leaf self) = fragmentRect self

-- | Compute the paddedbox for a subtree.
treeBox :: (CastDouble m, CastDouble n) =>
    FragmentTree (a, PaddedBox m n, c) -> PaddedBox m n
treeBox self@(Branch AncestorBox { boxUserData = (_, box', _)} _) = box' {
    Box.min = size', Box.max = size', Box.size = size', Box.nat = size
  } where
    size' = mapSizeX fromDouble $ mapSizeY fromDouble size
    size = mapSizeX (subtract $ hSpace box) $ mapSizeY (subtract $ vSpace box)$
         mapSizeX toDouble $ mapSizeY toDouble $ fragmentSize self
    box = mapX' toDouble $ mapY' toDouble box'
treeBox self@(Leaf Fragment { fragmentUserData = (_, box', _)}) = box' {
    Box.min = size', Box.max = size', Box.size = size', Box.nat = size
  } where
    size' = mapSizeX fromDouble $ mapSizeY fromDouble size
    size = mapSizeX (subtract $ hSpace box) $ mapSizeY (subtract $ vSpace box) $
        mapSizeX toDouble $ mapSizeY toDouble $ fragmentSize self
    box = mapX' toDouble $ mapY' toDouble box'

-- | Variant of `fragmentSize` asserting to the typesystem that both fields
-- of the resulting `Size` are of the same type.
fragmentSize' :: CastDouble x => FragmentTree (a, PaddedBox x x, c) -> Size x x
fragmentSize' = fragmentSize -- Work around for typesystem.
-- | Retrieve the position of a fragment.
fragmentPos :: (Double, Double) -> Fragment a -> (Double, Double)
fragmentPos (x, y) self = (x + hbScale (x_min r), y + hbScale (y_min r))
    where r = fragmentRect self

-- | Extract the tree datastructure out of Balkón's ParagraphLayout
reconstructTree :: Eq x => ParagraphLayout x -> [FragmentTree x]
reconstructTree ParagraphLayout { paragraphFragments = frags } =
    reconstructTree' [frag {
            fragmentAncestorBoxes = reverse $ fragmentAncestorBoxes frag
        } | frag <- frags]
-- | Extract the tree datastructure out of Balkón's fragments.
reconstructTree' :: Eq x => [Fragment x] -> [FragmentTree x]
reconstructTree' (self@Fragment { fragmentAncestorBoxes = [] }:frags) =
    Leaf self:reconstructTree' frags
reconstructTree' frags@(Fragment {
        fragmentAncestorBoxes = branch:_, fragmentLine = line
  }:_) =
    Branch branch (reconstructTree' [ child { fragmentAncestorBoxes = ancestors }
            | child@Fragment { fragmentAncestorBoxes = _:ancestors } <- childs])
        :reconstructTree' sibs
  where
    (childs, sibs) = span sameBranch frags
    -- Cluster ancestor branches, breaking them per-line.
    sameBranch Fragment {fragmentAncestorBoxes=branch':_, fragmentLine=line'} =
        branch == branch' && line == line'
    -- Leaves are always in their own branch.
    sameBranch Fragment { fragmentAncestorBoxes = [] } = False
reconstructTree' [] = []

-- | Add an X,Y offset to all positions, annotating the userdata.
positionTree :: (CastDouble m, CastDouble n) => (Double, Double) ->
        FragmentTree (a, PaddedBox m n, c) ->
        FragmentTree (a, PaddedBox m n, ((Double, Double), c))
positionTree (x, y) self@(Branch (AncestorBox (a, b, c) d e f g) childs) =
    Branch (AncestorBox (a, b, (pos, c)) d e f g) $ map (positionTree pos) childs
  where
    pos = (x + hbScale (x_min rect), y + hbScale (y_min rect))
    rect = treeRect self
positionTree (x, y) self@(Leaf (Fragment (a, b, c) d _ f g h i)) =
    Leaf (Fragment (a, b, (pos, c)) d [] f g h i)
  where
    pos = (x + hbScale (x_min rect), y + hbScale (y_min rect))
    rect = treeRect self
-- | Retrieve 3rd userdata field.
treeInner :: FragmentTree (a, b, c) -> c
treeInner (Branch AncestorBox { boxUserData = (_, _, ret) } _) = ret
treeInner (Leaf Fragment { fragmentUserData = (_, _, ret) }) = ret
-- | Retrieve userdata field.
treeInner' :: FragmentTree a -> a
treeInner' (Branch self _) = boxUserData self
treeInner' (Leaf self) = fragmentUserData self

-- | Retrieve Harfbuzz data out of the tree extracted from Balkón.
glyphs :: FragmentTree x -> [(HB.GlyphInfo, HB.GlyphPos)]
glyphs (Branch _ _) = []
glyphs (Leaf self) = fragmentGlyphs self
-- | Retrieve the Unicode codepoints out of the tree extracted from Balkón.
codepoints :: FragmentTree x -> [Word32]
codepoints self = map HB.codepoint $ map fst $ glyphs self

------
--- Taken from Balkón
------
-- | Calculate the smallest rectangle that completely contains all the given
-- rectangles.
unions [] = trace "No rects to union!" $ Rect 0 0 0 0
unions rects = foldr1 union rects

-- | Calculate the smallest rectangle that completely contains the given two
-- rectangles.
--
-- The origin of the resulting rectangle will be the corner with the lowest
-- X coordinate and the highest Y coordinate, regardless of the origin of the
-- input rectangles.
union :: (Num a, Ord a) => Rect a -> Rect a -> Rect a
union a b = Rect x_low y_high dx (-dy) where
    x_low = x_min a `min` x_min b
    y_low = y_min a `min` y_min b
    x_high = x_max a `max` x_max b
    y_high = y_max a `max` y_max b
    dx = x_high - x_low
    dy = y_high - y_low
