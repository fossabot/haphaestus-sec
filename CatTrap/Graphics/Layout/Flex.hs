module Graphics.Layout.Flex where

import Graphics.Layout.Box as B (Length(..), lowerLength, Size(..), PaddedBox(..),
        maxWidth, width, minWidth, maxHeight, height, minHeight, CastDouble(..), Zero(..))
import Data.List (intersperse)
import GHC.Real (infinity)
import Data.Maybe (fromMaybe)

data FlexParent a b = FlexParent {
    direction :: Direction,
    reverseRows :: Bool,
    wrap :: FlexWrapping,
    justify :: Justification,
    alignLines :: Maybe Justification, -- `Nothing` is "stretch"
    baseGap :: b,
    crossGap :: b,
    children :: [[FlexChild a b]], -- 2D list to store lines once split.
    pageWidth :: Double -- Pagination argument
} deriving (Eq, Show, Read)
data FlexChild a b = FlexChild {
    grow :: Double,
    shrink :: Double,
    basis :: b,
    alignment :: Alignment,
    flexInner :: a
} deriving (Eq, Show, Read)

data Direction = Row | Column deriving (Eq, Show, Read)
data FlexWrapping = NoWrap | Wrap | WrapReverse deriving (Eq, Show, Read)
data Justification = JStart | JEnd | JCenter | JSpaceBetween | JSpaceAround | JSpaceEvenly
    deriving (Eq, Show, Read)
data Alignment = AlStretch | AlStart | AlEnd | AlCenter | AlBaseline
    deriving (Eq, Show, Read)

flexMap :: (a -> b) -> FlexParent a c -> FlexParent b c
flexMap cb self = FlexParent {
    direction = direction self, reverseRows = reverseRows self, wrap = wrap self,
    justify = justify self, alignLines = alignLines self,
    baseGap = baseGap self, crossGap = crossGap self, pageWidth = pageWidth self,
    children = [[FlexChild {
        grow = grow kid, shrink = shrink kid, basis = basis kid,
        alignment = alignment kid,
        flexInner = cb $ flexInner kid -- The important line!
    } | kid <- row] | row <- children self]
  }
flexResolve :: CastDouble b => (a -> Direction -> Double) -> Double ->
        FlexParent a b -> FlexParent a Double
flexResolve cb size self = FlexParent {
    direction = direction self, reverseRows = reverseRows self, wrap = wrap self,
    justify = justify self, alignLines = alignLines self,
    baseGap = toDoubleWithin size $ baseGap self,
    crossGap = toDoubleWithin size $ crossGap self,
    pageWidth = pageWidth self,
    children = [[FlexChild {
        grow = grow kid, shrink = shrink kid,
        basis = toDoubleWithinAuto (flexInner kid `cb` direction self) size $ basis kid,
        alignment = alignment kid, flexInner = flexInner kid
    } | kid <- row] | row <- children self]
  }

flexMaxBasis :: FlexParent a Double -> Double
flexMaxBasis self = maximum [basis child | row <- children self, child <- row]
flexSumBasis :: FlexParent a Double -> Double
flexSumBasis self = maximum [Prelude.sum $
        intersperse (baseGap self) $ map basis row | row <- children self]

-- NOTE: shrink propery may yield negative sizes. Caller will want to enforce min-sizes.
flexWrap :: CastDouble b => FlexParent a b -> Double -> FlexParent a b
flexWrap self size
    | NoWrap <- wrap self = post self
    | Wrap <- wrap self = post self'
    | WrapReverse <- wrap self = post self' { children=reverse $ children self' }
  where
    self' = self {
        children = concatMap wrapRow $ children self
    }
    wrapRow :: CastDouble b => [FlexChild a b] -> [[FlexChild a b]]
    wrapRow [] = []
    wrapRow kids@(kid:_) = let (row, rest) = splitRow' kids $ basis' kid
        in row:wrapRow rest
    splitRow, splitRow' :: CastDouble b => [FlexChild a b] -> Double ->
            ([FlexChild a b], [FlexChild a b])
    -- This wrapper function ensures we don't end up with empty rows, or infinite loops.
    splitRow' (kid:kids) end =
        let (kids', rest) = splitRow kids (end + baseGap' self + basis' kid)
        in (kid:kids', rest)
    splitRow' [] _ = ([], [])
    splitRow (kid:kids) end
        | end > size = ([], kid:kids)
        | otherwise = let (kids', rest) = splitRow kids (end + baseGap' self + basis' kid)
            in (kid:kids', rest)
    splitRow [] _ = ([], [])

    post :: CastDouble b => FlexParent a b -> FlexParent a b
    post flex
        | reverseRows self = post' flex { children = map reverse $ children flex }
        | otherwise = post' flex
    post' :: CastDouble b => FlexParent a b -> FlexParent a b
    post' flex = flex { children = map resizeRow $ children flex }
    resizeRow :: CastDouble b => [FlexChild a b] -> [FlexChild a b]
    resizeRow row
        | rowSize > size = [kid {
                basis = fromDouble $ basis' kid - shrink kid * nanguard sfr
            } | kid <- row]
        | rowSize < size = [kid {
                basis = fromDouble $ basis' kid + grow kid * nanguard gfr
            } | kid <- row]
        | otherwise = row
      where
        rowSize = Prelude.sum $ intersperse (baseGap' self) $ map basis' row
        sfr = (rowSize - size)/(Prelude.sum $ map shrink row)
        gfr = (size - rowSize)/(Prelude.sum $ map grow row)
        nanguard x | isNaN x = 0
            | isInfinite x = 0
            | otherwise = x
    baseGap' :: CastDouble b => FlexParent a b -> Double
    baseGap' = toDouble . baseGap
    basis' :: CastDouble b => FlexChild a b -> Double
    basis' = toDouble . basis

flexRowSize :: (a -> Double) -> [FlexChild a b] -> Double
flexRowSize cb row = maximum $ map (cb . flexInner) row
flexRowsSize :: (a -> Double) -> FlexParent a Double -> Double
flexRowsSize cb FlexParent { crossGap = gap, children = kids } =
    sum $ intersperse gap $ flexRowSize cb `map` kids

justifyOffset, justifySpacing :: Double -> [Double] -> Double -> Justification -> Double
justifyOffset _ _ _ JStart = 0
justifyOffset outersize ks g JEnd = outersize - innersize g ks
justifyOffset outersize ks g JCenter = half $ outersize - innersize g ks
justifyOffset _ _ _ JSpaceBetween = 0
justifyOffset outersize ks g JSpaceAround =
    half $ (outersize - innersize g ks)/length' ks
justifyOffset _ ks _ _ | length ks <= 1 = 0 -- No gaps to space, avoid numeric errors.
justifyOffset size ks g JSpaceEvenly = (size - innersize g ks)/(length' ks + 1)
justifySpacing size ks g JSpaceBetween = (size - innersize g ks)/(length' ks - 1)
justifySpacing size ks g JSpaceAround = (size - innersize g ks)/length' ks
justifySpacing size ks g JSpaceEvenly = (size - innersize g ks)/(length' ks + 1)
justifySpacing _ _ _ _ = 0

flexJustify :: (a -> Double) -> Double -> [a] -> Double -> Justification -> [(Double, a)]
flexJustify cb size kids gap just = inner kids offs
  where
    offs = justifyOffset size kids' gap just
    spacing = justifySpacing size kids' gap just
    kids' = map cb kids
    inner (k:ks) start = (start, k):inner ks (start + cb k + gap)
    inner [] _ = []

alignOffset :: Double -> Double -> Alignment -> Double
alignOffset _ _ AlStretch = 0 -- Needs special handling elsewhere
alignOffset _ _ AlStart = 0
alignOffset outer inner AlEnd = outer - inner
alignOffset outer inner AlCenter = half $ outer - inner
alignOffset outer inner AlBaseline = half $ outer - inner -- FIXME: Implement properly!

innersize gap = sum . intersperse gap
half = (/2)
length' :: [a] -> Double
length' = toEnum . length

------
--- Mapping Box Model axes <-> Flex Box axes
------

outerMinMain, outerMain, outerMaxMain :: Num m => PaddedBox m m -> Direction -> m
outerMinMain box Row = minWidth box
outerMinMain box Column = minHeight box
outerMain box Row = width box
outerMain box Column = height box
outerMaxMain box Row = maxWidth box
outerMaxMain box Column = maxHeight box

outerMinCross, outerCross, outerMaxCross :: Num m => PaddedBox m m -> Direction -> m
outerMinCross box Row = minHeight box
outerMinCross box Column = minWidth box
outerCross box Row = height box
outerCross box Column = width box
outerMaxCross box Row = maxHeight box
outerMaxCross box Column = maxWidth box

innerMinMain, innerMain, innerMaxMain :: Num m => PaddedBox m m -> Direction -> m
innerMinMain box = sizeMain $ B.min box
innerMain box = sizeMain $ B.size box
innerMaxMain box = sizeMain $ B.max box

innerMinCross, innerCross, innerMaxCross :: Num m => PaddedBox m m -> Direction -> m
innerMinCross box = sizeCross $ B.min box
innerCross box = sizeCross $ B.size box
innerMaxCross box = sizeCross $ B.max box

sizeMain, sizeCross :: Num m => Size m m -> Direction -> m
sizeMain self Row = inline self
sizeMain self Column = block self
sizeCross self Row = block self
sizeCross self Column = inline self

flexGetBox :: (Zero m, CastDouble m, Zero n, CastDouble n) =>
    (a -> PaddedBox Double Double) -> FlexParent a m -> PaddedBox m n
flexGetBox cb self = zero {
    B.min = flexMaxBasis self' `size` flexRowsSize (cb' innerMinCross) self',
    B.max = fromRational infinity `size` fromRational infinity,
    B.nat = flexSumBasis self' `size` flexRowsSize (cb' innerCross) self',
    B.size = flexSumBasis self' `size` flexRowsSize (cb' innerCross) self'
  } where
    size main cross
        | Row <- direction self = fromDouble main `Size` fromDouble cross
        | otherwise = fromDouble cross `Size` fromDouble main
    cb' cb_ = flip cb_ (direction self) . cb
    self' = flexResolve (innerMain . cb) 0 self

flexSplit :: (a -> Size Double Double) -> Double -> Double -> FlexParent a Double ->
    (FlexParent a Double, FlexParent a Double)
flexSplit cb h _ self@FlexParent { direction = Row, pageWidth = w } =
    (self' { children = page0 }, self' { children = page1 })
  where
    self' = flexWrap self w
    (page0, page1) = splitRows (-crossGap self) $ children self
    splitRows start (row:rows)
        | start >= h = ([], row:rows)
        | otherwise =
            let (rows', rest) = flip splitRows rows $
                    start + crossGap self + flexRowSize (inline . cb) row
            in (row:rows', rest)
    splitRows _ [] = ([], [])
flexSplit cb h h' self@FlexParent { direction = Column, pageWidth = w }
    | measure h = (flexWrap self h, self { children = [] })
    -- If it fits on neither page... Row-direction is more versatile!
    | not $ measure h' = flexSplit cb h h' self { direction = Row }
    | otherwise = (self { children = [] }, flexWrap self h')
  where
    measure space = (block . cb) `flexRowsSize` flexWrap self space <= w

flexPosition :: ((Double, Double) -> a -> b) -> (a -> Size Double Double) ->
        (Double, Double) -> Size Double Double ->
        FlexParent a Double -> FlexParent b Double
flexPosition cb cb' (x,y) size self@FlexParent { direction = dir } = self {
    children = map rowPosition $ flexJustify rowsize (sizeCross size dir)
            (children self) (crossGap self) (justify self)
  } where
    rowsize = flexRowSize $ flip sizeCross dir . cb'
    -- TODO: Handle stretch properly
    rowPosition (rpos, row) =
        let rsize = flexRowSize (flip sizeCross dir . cb') row
        in map (alignChild rsize rpos) $ flexJustify basis rsize row
                (baseGap self) (fromMaybe JSpaceAround $ alignLines self)
    alignChild rsize rpos (kpos, kid@FlexChild {
        flexInner = kid', alignment = align'
      }) = kid {
        flexInner = flip cb kid' $ sz kpos $
                rpos + alignOffset rsize (flip sizeCross dir $ cb' kid') align'
      }
    sz m c | Row <- direction self = (x + m, y + c)
        | otherwise = (x + c, y + m)
