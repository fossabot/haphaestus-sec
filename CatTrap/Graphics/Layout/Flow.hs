-- | Sizes a block element & positions their children.
-- Taking into account size bounds.
module Graphics.Layout.Flow(flowMinWidth, flowNatWidth, flowMaxWidth, flowWidth,
        flowNatHeight, flowMinHeight, flowMaxHeight, flowHeight,
        positionFlow, layoutFlow) where

import Graphics.Layout.Box as B

-- | Compute the minimum width of a block element with children of the given sizes.
flowMinWidth :: Double -> PaddedBox a Length -> [PaddedBox b Double] -> Double
flowMinWidth _ PaddedBox {B.min = Size (Pixels x) _} _ = x
flowMinWidth parent PaddedBox {B.min = Size (Percent x) _} _ = x * parent
flowMinWidth parent self@PaddedBox {B.min = Size Preferred _} childs =
    flowNatWidth parent self childs
flowMinWidth _ _ childs = maximum $ (0:) $ map minWidth childs
-- | Compute the natural width of a block element with children of the given sizes.
flowNatWidth :: Double -> PaddedBox a Length -> [PaddedBox b Double] -> Double
flowNatWidth _ PaddedBox {size = Size (Pixels x) _} _ = x
flowNatWidth parent PaddedBox {size = Size (Percent x) _} _ = x * parent
flowNatWidth parent self@PaddedBox {size = Size Min _, B.min = Size x _} childs
    -- Avoid infinite loops!
    | x /= Preferred = flowMinWidth parent self childs
flowNatWidth parent _ childs = maximum $ (0:) $ map maxWidth childs
-- | Compute the maximum width of a block element inside the given parent size.
flowMaxWidth :: PaddedBox a Double -> PaddedBox b Length -> Double
flowMaxWidth _ PaddedBox {B.max = Size (Pixels x) _} = x
flowMaxWidth parent PaddedBox {B.max = Size (Percent x) _} = x * (inline $ size parent)
flowMaxWidth parent self@PaddedBox {B.max = Size Auto _} = inline (size parent) - ws
    where
        ws = l2d (left $ margin self) + l2d (left $ border self) + l2d (left $ padding self) +
            l2d (right $ padding self) + l2d (right $ border self) + l2d (right $ margin self)
        l2d = lowerLength $ inline $ size parent
flowMaxWidth parent self@PaddedBox {B.max = Size Preferred _} =
    flowNatWidth (inline $ size parent) self []
flowMaxWidth parent self@PaddedBox {B.max = Size Min _} =
    flowMinWidth (inline $ B.min parent) self []
-- | Compute final block element width based on cached width computations &
-- parent size.
flowWidth :: PaddedBox a Double -> PaddedBox b Length -> Double
flowWidth parent self
    | small > large = small
    | natural > large = large
    | inline (size self) == Auto = large -- specialcase
    | natural >= small = natural
    | otherwise = small
  where
    small = flowMinWidth (inline $ B.min parent) self []
    natural = flowNatWidth (inline $ size parent) self []
    large = flowMaxWidth parent self

-- | Compute natural block element height at cached width.
flowNatHeight :: Double -> PaddedBox Length Double -> [PaddedBox Double Double] -> Double
flowNatHeight _ PaddedBox {size = Size _ (Pixels y)} _ = y
flowNatHeight parent PaddedBox {size = Size _ (Percent y)} _ = y * parent
flowNatHeight _ PaddedBox {size = Size _ Min} childs =
    sum $ map minHeight $ marginCollapse childs
flowNatHeight _ PaddedBox {size = Size owidth _} childs =
    sum $ map height $ marginCollapse childs
-- | Compute minimum block height at cached width.
flowMinHeight :: Double -> PaddedBox Length Double -> Double
flowMinHeight _ PaddedBox {B.min = Size _ (Pixels y)} = y
flowMinHeight parent PaddedBox {B.min = Size _ (Percent y)} = y * parent
flowMinHeight parent self = flowNatHeight parent self []
-- | Compute maximum block height at cached width.
flowMaxHeight :: Double -> PaddedBox Length Double -> Double
flowMaxHeight _ PaddedBox {B.max = Size _ (Pixels y)} = y
flowMaxHeight parent PaddedBox {B.max = Size _ (Percent y)} = y * parent
flowMaxHeight parent PaddedBox {B.max = Size _ Auto} = parent
flowMaxHeight parent self@PaddedBox {B.max = Size _ Preferred} = flowNatHeight parent self []
flowMaxHeight parent self@PaddedBox {B.max = Size _ Min} = flowMinHeight parent self
-- | Compute final block height at cached width.
flowHeight :: PaddedBox Double Double -> PaddedBox Length Double -> Double
flowHeight parent self
    | small > large = small
    | natural > large = large
    | natural >= small = natural
    | otherwise = small
  where
    small = flowMinHeight (block $ B.min parent) self
    natural = flowNatHeight (block $ B.nat parent) self []
    large = flowMaxHeight (block $ B.max parent) self

-- | Compute position of all children relative to this block element.
positionFlow :: [PaddedBox Double Double] -> [Size Double Double]
positionFlow childs = scanl inner (Size 0 0) $ marginCollapse childs
  where inner (Size x y) self = Size x $ height self
-- | Compute size given block element in given parent,
-- & position of given children.
layoutFlow :: PaddedBox Double Double -> PaddedBox Length Length ->
        [PaddedBox Length Double] ->
        (PaddedBox Double Double, [(Size Double Double, PaddedBox Double Double)])
layoutFlow parent self childs = (self', zip positions' childs')
  where
    positions' = positionFlow childs'
    childs' = map layoutZooko childs
    self' = self0 {
        B.min = (B.min self0) { block = flowMinHeight (block $ B.min parent) self0 },
        size = (size self0) { block = flowHeight parent self0 },
        B.max = (B.max self0) { block = flowMaxHeight (block $ B.max parent) self0 },
        padding = mapY (lowerLength owidth) $ padding self0,
        border = mapY (lowerLength owidth) $ border self0,
        margin = mapY (lowerLength owidth) $ margin self0
      }
    self0 = self1 {
        size = (size self1) { block = Pixels $ flowNatHeight oheight self1 childs'}
      }
    self1 = self2 {
        size = (size self2) { inline = width' },
        B.max = (B.max self2) { inline = flowMaxWidth parent self2 },
        B.min = (B.min self2) { inline = flowMinWidth owidth self2 [] },
        padding = mapX (lowerLength owidth) $ padding self2,
        border = mapX (lowerLength owidth) $ border self2,
        margin = lowerMargin owidth (owidth - width') $ margin self2
      }
    width' = flowWidth parent self
    self2 = self {
        size = (size self) { inline = Pixels $ flowNatWidth owidth self childs },
        B.min = (B.min self) { inline = Pixels $ flowMinWidth owidth self childs }
      }
    owidth = inline $ size parent
    oheight = block $ size parent
    layoutZooko child = child {
        B.min = Size (inline $ B.min child) (flowMinHeight (block $ B.min self') child),
        size = Size (inline $ size child) (flowHeight self' child),
        B.max = Size (inline $ B.max child) (flowMaxHeight (block $ size self') child),
        padding = mapY (lowerLength owidth) $ padding child,
        border = mapY (lowerLength owidth) $ border child,
        margin = mapY (lowerLength owidth) $ margin child
      }

-- | Removes overlapping margins.
marginCollapse :: [PaddedBox Double n] -> [PaddedBox Double n]
marginCollapse (x'@PaddedBox {margin = xm@Border { bottom = x }}:
        y'@PaddedBox {margin = ym@Border { top = y}}:rest)
    | x > y = x':marginCollapse (y' {margin = ym { top = 0 }}:rest)
    | otherwise = x' { margin = xm { bottom = 0 }}:marginCollapse (y':rest)
marginCollapse rest = rest

-- | Resolves auto paddings or margins to fill given width.
lowerMargin :: Double -> Double -> Border m Length -> Border m Double
lowerMargin _ available (Border top' bottom' Auto Auto) =
    Border top' bottom' (available/2) (available/2)
lowerMargin outerwidth available (Border top' bottom' Auto right') =
    Border top' bottom' available $ lowerLength outerwidth right'
lowerMargin outerwidth available (Border top' bottom' left' Auto) =
    Border top' bottom' (lowerLength outerwidth left') available
lowerMargin outerwidth _ (Border top' bottom' left' right') =
    Border top' bottom' (lowerLength outerwidth left') (lowerLength outerwidth right')
