module Graphics.Text.Font.Choose.Weight where

-- | Returns an double value to use with "weight", from an double in the
-- 1..1000 range, resembling the numbers from OpenType specification's OS2
-- usWeight numbers, which are also similar to CSS font-weight numbers.
-- If input is negative, zero, or greater than 1000, returns -1.
-- This function linearly interpolates between various FC_WEIGHT_* constants.
-- As such, the returned value does not necessarily match any of the predefined
-- constants.
foreign import ccall "FcWeightFromOpenTypeDouble" weightFromOpenTypeDouble ::
    Double -> Double
-- | `weightToOpenTypeDouble` is the inverse of `weightFromOpenType`.
-- If the input is less than FC_WEIGHT_THIN or greater than FC_WEIGHT_EXTRABLACK,
-- returns -1. Otherwise returns a number in the range 1 to 1000.
foreign import ccall "FcWeightToOpenTypeDouble" weightToOpenTypeDouble ::
    Double -> Double
-- | Variant of `weightFromOpenTypeDouble` taking ints.
foreign import ccall "FcWeightFromOpenType" weightFromOpenType :: Int -> Int
-- | Variant of `weightToOpenTypeDouble` taking ints.
foreign import ccall "FcWeightToOpenType" weightToOpenType :: Int -> Int
