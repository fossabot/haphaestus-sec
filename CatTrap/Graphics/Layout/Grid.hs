{-# LANGUAGE RecordWildCards, OverloadedStrings, DeriveGeneric #-}
-- | Sizes grid cells & positions elements to them.
module Graphics.Layout.Grid(Grid(..), Track(..), GridItem(..), GridItem'(..), Alignment(..),
        buildTrack, buildGrid, setCellBox, enumerate, gridItemBox, cellSize,
        trackMin, trackNat, gridEstWidth, sizeTrackMins, sizeTrackNats, sizeTrackMaxs,
        trackPosition, gridPosition, trackLayout, gridLayout) where

import Data.Either (fromRight)
import Data.Text (Text)
import Data.List (intersperse)
import Graphics.Layout.Box as B

import Debug.Trace (trace)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | An element which positions it's children within a grid.
type Grid m n = Size (Track m) (Track n)
-- | The sizes to which children are alonged on a single axis.
data Track x = Track {
    -- | The desired size of each cell.
    -- If Left specifies ratio of excess space to use.
    cells :: [Either x Double],
    -- | The minimum amount of space each cell should take.
    trackMins :: [Double],
    -- | The ideal amount of space each cell should take.
    trackNats :: [Double],
    -- | How much space to add between cells.
    gap :: x
} deriving (Show, Read, Eq, Ord)
-- | Which cells a child should be aligned to.
type GridItem = Size GridItem' GridItem'
-- | How a grid child should be aligned per-axis.
data GridItem' = GridItem {
    -- | On which cell should this child start.
    cellStart :: Int,
    -- | Before which cell should this child end.
    cellEnd :: Int,
    -- | How to redistribute excess space.
    alignment :: Alignment,
    -- | The minimum amount of space to allocate to this child.
    minSize :: Double,
    -- | The maximum aount of space to allocate to this child.
    natSize :: Double
} deriving (Read, Show, Ord, Eq, Generic)
instance NFData GridItem'
-- | How to redistribute excess space.
data Alignment = Start | Mid | End deriving (Read, Show, Enum, Ord, Eq, Generic)
instance NFData Alignment

-- | Constructs a track with default (to-be-computed) values & given cell sizes.
buildTrack :: CastDouble x => [Either x Double] -> Track x
buildTrack cells = Track cells [] [] $ fromDouble 0
-- | Constructs a grid with default (to-be-computed) values & given cell sizes.
buildGrid :: (CastDouble m, CastDouble n) =>
        [Either m Double] -> [Either n Double] -> Grid m n
buildGrid rows cols = Size (buildTrack cols) (buildTrack rows)

-- | Verify that the track is properly formed & can be validly processed.
verifyTrack :: Track x -> [GridItem'] -> Bool
verifyTrack track cells' = and [
    cellStart cell < length (cells track) && cellStart cell >= 0 &&
    cellEnd cell < length (cells track) && cellEnd cell > cellStart cell
  | cell <- cells']
-- | Verify that the grid is properly formed & can be validly processed.
verifyGrid :: Grid m n -> [GridItem] -> Bool
verifyGrid grid cells =
    verifyTrack (inline grid) (map inline cells) && verifyTrack (block grid) (map block cells)

-- | Compute the minimum size for the track given cell sizes.
-- Refers to computed min sizes if cached.
trackMin :: (n -> Double) -> Track n -> Double
trackMin cb self@Track { trackMins = [] } =
    sum $ intersperse (cb $ gap self) [cb x | Left x <- cells self]
trackMin cb self = sum $ intersperse (cb $ gap self) $ trackMins self
-- | Compute the natural size for the track given cell sizes.
-- Refers to compute natural sizes if cached.
trackNat :: (n -> Double) -> Track n -> Double
trackNat cb self@Track { trackNats = [] } =
    sum $ intersperse (cb $ gap self) [cb x | Left x <- cells self]
trackNat cb self = sum $ intersperse (cb $ gap self) $ trackNats self

-- | Selects all children entirely on the specified cell.
cellsForIndex :: [GridItem'] -> Int -> [GridItem']
cellsForIndex cells ix =
    [cell | cell <- cells, cellStart cell == ix, cellStart cell == pred (cellEnd cell)]
-- | Sets minimum & natural sizes from the given padded box.
setCellBox :: (CastDouble m, CastDouble n) => GridItem -> PaddedBox m n -> GridItem
setCellBox (Size x y) box = Size x {
    minSize = B.minWidth $ mapX' toDouble box,
    natSize = B.width $ mapX' toDouble box
  } y {
    minSize = B.minHeight $ mapY' toDouble box,
    natSize = B.height $ mapY' toDouble box
  }

-- | Estimate grid width to inform proper width calculation.
gridEstWidth :: Grid y Length -> [GridItem] -> Double
gridEstWidth (Size cols _) childs = trackNat toDouble cols {
    trackMins = sizeTrackMins 0 cols $ map inline childs,
    trackNats = sizeTrackNats 0 cols $ map inline childs
  }
-- | Calculate minimum sizes for all cells in the track.
-- Sized to fit given children.
sizeTrackMins :: Double -> Track Length -> [GridItem'] -> [Double]
sizeTrackMins parent track childs = map inner $ enumerate $ cells track
  where
    inner (_, Left (Pixels x)) = x
    inner (_, Left (Percent x)) = x * parent
    inner arg@(ix, Left Preferred) =
        maximum $ (0:) $ map natSize $ cellsForIndex childs ix
    inner (ix, _) =
        maximum $ (0:) $ map minSize $ cellsForIndex childs ix
-- | Compute natural sizes for all cells in the track.
-- Sized to fit given children.
sizeTrackNats :: Double -> Track Length -> [GridItem'] -> [Double]
sizeTrackNats parent track childs = map inner $ enumerate $ cells track
  where
    inner (_, Left (Pixels x)) = x
    inner (_, Left (Percent x)) = x * parent
    inner arg@(ix, Left Min) =
        maximum $ (0:) $ map minSize $ cellsForIndex childs ix
    inner (ix, _) =
        maximum $ (0:) $ map natSize $ cellsForIndex childs ix
-- | Compute maximum sizes for all cells in the track, sized to the parent element.
sizeTrackMaxs :: Double -> Track Length -> [Double]
sizeTrackMaxs parent track = map (inner fr) $ zip subsizes $ cells track
  where
    subsizes = zip (trackMins track) (trackNats track)
    fr = Prelude.max 0 fr'
    fr' = (parent - estimate)/(countFRs $ cells track)
    estimate = sum $ intersperse (lowerLength parent $ gap track) $
            map (inner 0) $ zip subsizes $ cells track
    inner _ (_, Left (Pixels x)) = x
    inner _ (_, Left (Percent x)) = x*parent
    inner _ ((_, nat), Left Preferred) = nat
    inner _ ((min, _), Left Min) = min
    inner fr ((_, nat), Left Auto) = Prelude.min nat fr
    inner fr (_, Right x) = x*fr

-- | Compute the position of all children within the grid.
trackPosition :: Track Double -> [GridItem'] -> [Double]
trackPosition self childs = map gridCellPosition childs
  where
    gridCellPosition child = track (cellStart child) + align whitespace (alignment child)
      where
        whitespace = track (cellEnd child) - track (cellStart child) - natSize child
    track = flip track' $ cells self
    track' ix (size:sizes) = fromRight 0 size + track' (pred ix) sizes
    track' 0 _ = 0
    track' ix [] = trace "WARNING! Malformed input table!" 0
    align _ Start = 0
    align excess Mid = excess/2
    align excess End = excess
-- | Compute the maximum size along an axis of a child, for it to be sized to.
cellSize :: CastDouble x => Track x -> GridItem' -> Double
cellSize self child = track (cellEnd child) - track (cellStart child)
  where
    track = flip track' $ cells self
    track' ix (size:sizes) =
        (toDouble $ fromRight (fromDouble 0) size) + track' (pred ix) sizes
    track' 0 _ = 0
    track' ix [] = trace "WARNING! Malformed input table!" 0
-- | Compute the maximum size as a PaddedBox of a child, for it to be sized to.
gridItemBox :: (CastDouble x, CastDouble y) => Grid y x -> GridItem -> PaddedBox Double Double
gridItemBox (Size cols rows) cell =
    size2box (cellSize cols (inline cell) `Size` cellSize rows (block cell))
  where
    size2box size = zero { B.min = size, B.max = size, B.size = size }
-- | Compute the position of all children in a grid.
gridPosition :: Grid Double Double -> [GridItem] -> [(Double, Double)]
gridPosition (Size cols rows) childs =
    trackPosition rows (map inline childs) `zip` trackPosition cols (map block childs)
-- | Compute the track sizes & child positions along a single axis.
trackLayout :: Double -> Double -> Track Length -> [GridItem'] ->
        (Track Double, [(Double, GridItem')])
trackLayout parent width self childs = (self', zip positions childs)
  where
    positions = trackPosition self' childs
    self' = self {
        cells = map Left sizes,
        trackMins = mins, trackNats = nats,
        gap = lowerLength width $ gap self
      }
    sizes = sizeTrackMaxs parent self { trackMins = mins, trackNats = nats }
    mins = sizeTrackMins parent self childs
    nats = sizeTrackNats parent self childs
-- | Compute the track sizes & child positions along both axes.
gridLayout :: Size Double Double -> Grid Length Length -> [GridItem] ->
        (Grid Double Double, [((Double, Double), GridItem)])
gridLayout parent (Size cols rows) childs = (self', zip positions childs)
  where
    positions = gridPosition self' childs
    self' = Size cols' { gap = lowerLength width $ gap cols } rows'
    (rows', _) = trackLayout (block parent) width rows $ map block childs
    width = trackNat id cols'
    (cols', _) = trackLayout (inline parent) 0 cols $ map inline childs

-- | Utility for associate an index with each item in a list.
enumerate = zip [0..]

-- | Utility for summing the divisor used to compute the fr unit.
countFRs (Left Auto:rest) = succ $ countFRs rest
countFRs (Right x:rest) = x + countFRs rest
countFRs (_:rest) = countFRs rest
countFRs [] = 0
