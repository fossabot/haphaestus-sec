{-# LANGUAGE RecordWildCards, DeriveGeneric #-}
-- | Datastructures representing the CSS box model,
-- & utilities for operating on them.
module Graphics.Layout.Box(Border(..), mapX, mapY,
        Size(..), mapSizeX, mapSizeY,
        PaddedBox(..), zeroBox, lengthBox, mapX', mapY',
        width, height, minWidth, minHeight, maxWidth, maxHeight,
        leftSpace, rightSpace, topSpace, bottomSpace, hSpace, vSpace,
        Length(..), mapAuto, lowerLength, Zero(..), CastDouble(..)) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- | Amount of space surrounding the box.
data Border m n = Border {
    top :: m, bottom :: m, left :: n, right :: n
} deriving (Eq, Read, Show, Generic)
instance (NFData m, NFData n) => NFData (Border m n)
-- | Convert horizontal spacing via given callback.
mapX :: (n -> nn) -> Border m n -> Border m nn
-- | Convert vertical spacing via given callback.
mapY :: (m -> mm) -> Border m n -> Border mm n
mapX cb self = self { left = cb $ left self, right = cb $ right self }
mapY cb self = self { top = cb $ top self, bottom = cb $ bottom self }

-- | 2D size of a box. Typically inline is width & block is height.
-- This may change as support for vertical layout is added.
data Size m n = Size {inline :: n, block :: m} deriving (Eq, Show, Read, Generic)
instance (NFData m, NFData n) => NFData (Size m n)
-- | Convert inline size via given callback
mapSizeY :: (m -> mm) -> Size m n -> Size mm n
mapSizeY cb self = Size (inline self) (cb $ block self)
-- | Convert block size via given callback
mapSizeX :: (n -> nn) -> Size m n -> Size m nn
mapSizeX cb self = Size (cb $ inline self) (block self)

-- | A box with min & max bounds & surrounding borders. The CSS Box Model.
data PaddedBox m n = PaddedBox {
    -- | The minimum amount of pixels this box should take.
    min :: Size m n,
    -- | The maximum amount of pixels this box should take.
    max :: Size m n,
    -- | The ideal number of pixels this box should take.
    nat :: Size Double Double,
    -- | The amount of pixels this box should take.
    size :: Size m n,
    -- | The amount of space between the box & the border.
    padding :: Border m n,
    -- | The amount of space for the border.
    border :: Border m n,
    -- | The amount of space between the border & anything else.
    margin :: Border m n
} deriving (Eq, Read, Show, Generic)
instance (NFData m, NFData n) => NFData (PaddedBox m n)
-- | An empty box, takes up nospace onscreen.
zeroBox :: PaddedBox Double Double
zeroBox = PaddedBox {
    min = Size 0 0,
    max = Size 0 0,
    nat = Size 0 0,
    size = Size 0 0,
    padding = Border 0 0 0 0,
    border = Border 0 0 0 0,
    margin = Border 0 0 0 0
  }
-- | A box which takes up all available space with no borders.
lengthBox :: PaddedBox Length Length
lengthBox = PaddedBox {
    min = Size Auto Auto,
    max = Size Auto Auto,
    nat = Size 0 0,
    size = Size Auto Auto,
    padding = Border zero zero zero zero,
    border = Border zero zero zero zero,
    margin = Border zero zero zero zero
  }

-- | Convert all sizes along the inline axis via given callback.
mapX' :: (n -> nn) -> PaddedBox m n -> PaddedBox m nn
mapX' cb PaddedBox {..} = PaddedBox {
    min = Size (cb $ inline min) (block min),
    size = Size (cb $ inline size) (block size),
    nat = Size 0 0,
    max = Size (cb $ inline max) (block max),
    padding = mapX cb padding,
    border = mapX cb border,
    margin = mapX cb margin
  }
-- | Convert all sizes along the block axis via given callback.
mapY' :: (m -> mm) -> PaddedBox m n -> PaddedBox mm n
mapY' cb PaddedBox {..} = PaddedBox {
    min = Size (inline min) (cb $ block min),
    size = Size (inline size) (cb $ block size),
    nat = Size 0 0,
    max = Size (inline max) (cb $ block max),
    padding = mapY cb padding,
    border = mapY cb border,
    margin = mapY cb margin
  }

-- | The total size along the inline axis including borders, etc.
width PaddedBox {..} = left margin + left border + left padding +
    inline size + right padding + right border + right margin
-- | The total size along the block axis, including borders, etc.
height PaddedBox {..} = top margin + top border + top padding +
    block size + bottom padding + bottom border + bottom margin
-- | The total minimum size along the inline axis.
minWidth PaddedBox {..} = left margin + left border + left padding +
    inline min + right padding + right border + right margin
-- | The total minimum size along the block axis.
minHeight PaddedBox {..} = top margin + top border + top padding +
    block min + bottom padding + bottom border + bottom margin
-- | The total maximum size along the inline axis.
maxWidth PaddedBox {..} = left margin + left border + left padding +
    inline max + right padding + right border + right margin
-- | The total maximum size along the block axis.
maxHeight PaddedBox {..} = top margin + top border + top padding +
    block max + bottom padding + bottom border + bottom margin

-- | Amount of whitespace to the left, summing margins, borders, & padding.
leftSpace PaddedBox {..} = left margin + left border + left padding
-- | Amount of whitespace to the right, summing margins, borders, & padding.
rightSpace PaddedBox {..} = right margin + right border + right padding
-- | Amount of whitespace to the top, summing margins, borders, & padding.
topSpace PaddedBox {..} = top margin + top border + top padding
-- | Amount of whitespace to the bottom, summing margins, borders, & padding.
bottomSpace PaddedBox {..} = bottom margin + bottom border + bottom padding
-- | Amount of whitespace along the x axis, summing margins, borders, & padding.
hSpace self = leftSpace self + rightSpace self
-- | Amount of whitespace along the y axis, summing margins, borders, & padding.
vSpace self = topSpace self + bottomSpace self

-- | A partially-computed length value.
data Length = Pixels Double -- ^ Absolute number of device pixels.
        | Percent Double -- ^ Multiplier by container width.
        | Auto -- ^ Use normal layout computations.
        | Preferred -- ^ Use computed preferred width.
        | Min -- ^ Use minimum legible width.
        deriving (Eq, Read, Show, Generic)
instance NFData Length

-- | Convert a length given the container's width. Filling in 0 for keywords.
-- If you wish for keywords to be handled differently, callers need to compute
-- that themselves.
lowerLength :: Double -> Length -> Double
lowerLength _ (Pixels x) = x
lowerLength outerwidth (Percent x) = x * outerwidth
lowerLength _ _ = 0

-- | Replace keywords with a given number of pixels.
-- Useful for avoiding messing up percentage calculations in later processing.
mapAuto x Auto = Pixels x
mapAuto x Preferred = Pixels x
mapAuto x Min = Pixels x
mapAuto _ x = x

-- | Typeclass for zeroing out fields, so layout primitives can be more reusable.
class Zero a where
    -- | Return the empty (or zero) value for a CatTrap geometric type.
    zero :: a

instance Zero Double where zero = 0
instance Zero Length where zero = Pixels 0
instance (Zero m, Zero n) => Zero (PaddedBox m n) where
    zero = PaddedBox {
        min = Size zero zero,
        max = Size zero zero,
        nat = Size 0 0,
        size = Size zero zero,
        padding = Border zero zero zero zero,
        border = Border zero zero zero zero,
        margin = Border zero zero zero zero
    }
instance (Zero m, Zero n) => Zero (Border m n) where
    zero = Border zero zero zero zero

-- | Typeclass for converting between doubles & layout types, approximately if needs be.
-- So layout primitives can be more reusable.
class CastDouble a where
    -- | Convert a double to a double or length.
    fromDouble :: Double -> a
    -- | Convert a double or length to a double.
    toDouble :: a -> Double
    toDouble = toDoubleWithin 0
    toDoubleWithin :: Double -> a -> Double
    toDoubleWithin _ = toDouble
    toDoubleWithinAuto :: Double -> Double -> a -> Double
    toDoubleWithinAuto _ = toDoubleWithin

instance CastDouble Double where
    fromDouble = id
    toDouble = id
instance CastDouble Length where
    fromDouble = Pixels
    toDoubleWithin = lowerLength
    toDoubleWithinAuto x _ Auto = x
    toDoubleWithinAuto _ x y = toDoubleWithin x y
