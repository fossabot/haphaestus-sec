{-# LANGUAGE DeriveGeneric #-}
module Graphics.Text.Font.Choose.Range where

import Foreign.Ptr (Ptr)
import Control.Exception (bracket)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Graphics.Text.Font.Choose.Result (throwNull, throwFalse)

-- | Matches a numeric range.
data Range = Range Double Double deriving (Eq, Show, Ord, Generic)
-- | Matches an integral range.
iRange i j = toEnum i `Range` toEnum j

instance Hashable Range

------
--- Low-level
------
data Range'
type Range_ = Ptr Range'

withRange :: Range -> (Range_ -> IO a) -> IO a
withRange (Range i j) = bracket (throwNull <$> fcRangeCreateDouble i j) fcRangeDestroy
foreign import ccall "FcRangeCreateDouble" fcRangeCreateDouble ::
    Double -> Double -> IO Range_
foreign import ccall "FcRangeDestroy" fcRangeDestroy :: Range_ -> IO ()

thawRange :: Range_ -> IO Range
thawRange range' = alloca $ \i' -> alloca $ \j' -> do
    throwFalse <$> fcRangeGetDouble range' i' j'
    i <- peek i'
    j <- peek j'
    return $ Range i j
foreign import ccall "FcRangeGetDouble" fcRangeGetDouble ::
    Range_ -> Ptr Double -> Ptr Double -> IO Bool
