{-# LANGUAGE DeriveGeneric, TypeSynonymInstances, FlexibleInstances #-}
module Graphics.Text.Font.Choose.Value (Value(..), Value_, withValue, thawValue,
    value'Size, ToValue(..)) where

import Linear.Matrix (M22)
import Linear.V2 (V2(..))
import Graphics.Text.Font.Choose.CharSet (CharSet, withCharSet, thawCharSet)
import FreeType.Core.Base (FT_Face(..))
import Graphics.Text.Font.Choose.LangSet (LangSet, withLangSet, thawLangSet)
import Graphics.Text.Font.Choose.Range (Range, withRange, thawRange)
import Control.Exception (throw)

import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Array (advancePtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.C.String (withCString, peekCString)

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Graphics.Text.Font.Choose.Result (Word8, throwNull, Error(ErrTypeMismatch))

-- | A dynamic type system for `Pattern`s.
data Value = ValueVoid
    | ValueInt Int
    | ValueDouble Double
    | ValueString String
    | ValueBool Bool
    | ValueMatrix (M22 Double)
    | ValueCharSet CharSet
    | ValueFTFace FT_Face
    | ValueLangSet LangSet
    | ValueRange Range deriving (Eq, Show, Ord, Generic)

instance Hashable Value

-- | Coerces compiletime types to runtime types.
class ToValue x where
    toValue :: x -> Value
    fromValue :: Value -> Maybe x
    fromValue' :: Value -> x -- throws Result.Error
    fromValue' self | Just ret <- fromValue self = ret
    fromValue' _ = throw ErrTypeMismatch

instance ToValue () where
    toValue () = ValueVoid
    fromValue ValueVoid = Just ()
    fromValue _ = Nothing
instance ToValue Int where
    toValue = ValueInt
    fromValue (ValueInt x) = Just x
    fromValue _ = Nothing
instance ToValue Double where
    toValue = ValueDouble
    fromValue (ValueDouble x) = Just x
    fromValue _ = Nothing
instance ToValue String where
    toValue = ValueString
    fromValue (ValueString x) = Just x
    fromValue _ = Nothing
instance ToValue Bool where
    toValue = ValueBool
    fromValue (ValueBool x) = Just x
    fromValue _ = Nothing
instance ToValue (M22 Double) where
    toValue = ValueMatrix
    fromValue (ValueMatrix x) = Just x
    fromValue _ = Nothing
instance ToValue CharSet where
    toValue = ValueCharSet
    fromValue (ValueCharSet x) = Just x
    fromValue _ = Nothing
instance ToValue FT_Face where
    toValue = ValueFTFace
    fromValue (ValueFTFace x) = Just x
    fromValue _ = Nothing
instance ToValue LangSet where
    toValue = ValueLangSet
    fromValue (ValueLangSet x) = Just x
    fromValue _ = Nothing
instance ToValue Range where
    toValue = ValueRange
    fromValue (ValueRange x) = Just x
    fromValue _ = Nothing

------
--- Low-level
------

type Value_ = Ptr Int

foreign import ccall "size_value" value'Size :: Int
pokeUnion ptr x = castPtr (ptr `advancePtr` 1) `poke` x

withValue :: Value -> (Value_ -> IO a) -> IO a
withValue ValueVoid cb = allocaBytes value'Size $ \val' -> do
    poke val' 0
    cb val'
withValue (ValueInt x) cb = allocaBytes value'Size $ \val' -> do
    poke val' 1
    pokeElemOff val' 1 x
    cb val'
withValue (ValueDouble x) cb = allocaBytes value'Size $ \val' -> do
    poke val' 2
    pokeUnion val' x
    cb val'
withValue (ValueString str) cb =
    withCString str $ \str' -> allocaBytes value'Size $ \val' -> do
        poke val' 3
        pokeUnion val' str'
        cb val'
withValue (ValueBool b) cb = allocaBytes value'Size $ \val' -> do
    poke val' 4
    pokeUnion val' b
    cb val'
withValue (ValueMatrix mat) cb =
    withMatrix mat $ \mat' -> allocaBytes value'Size $ \val' -> do
        poke val' 5
        pokeUnion val' mat'
        cb val'
withValue (ValueCharSet charsets) cb =
    withCharSet charsets $ \charsets' -> allocaBytes value'Size $ \val' -> do
        poke val' 6
        pokeUnion val' charsets'
        cb val'
withValue (ValueFTFace x) cb = allocaBytes value'Size $ \val' -> do
    poke val' 7
    pokeUnion val' x
    cb val'
withValue (ValueLangSet langset) cb =
    withLangSet langset $ \langset' -> allocaBytes value'Size $ \val' -> do
        poke val' 8
        pokeUnion val' langset'
        cb val'
withValue (ValueRange range) cb =
    withRange range $ \range' -> allocaBytes value'Size $ \val' -> do
        poke val' 9
        pokeUnion val' range'
        cb val'

foreign import ccall "size_matrix" mat22Size :: Int
withMatrix (V2 (V2 xx yx) (V2 xy yy)) cb = allocaBytes mat22Size $ \mat' -> do
    pokeElemOff mat' 0 xx
    pokeElemOff mat' 1 xy
    pokeElemOff mat' 2 yx
    pokeElemOff mat' 3 yy
    cb mat'

thawValue :: Value_ -> IO (Maybe Value)
thawValue ptr = do
    kind <- peek $ castPtr ptr :: IO Word8
    let val' = castPtr (ptr `advancePtr` 1)
    case kind of
        0 -> return $ Just ValueVoid
        1 -> Just <$> ValueInt <$> peek val'
        2 -> Just <$> ValueDouble <$> peek val'
        3 -> do
            val <- throwNull <$> peek val'
            Just <$> ValueString <$> peekCString val
        4 -> Just <$> ValueBool <$> peek val'
        5 -> do
            mat' <- throwNull <$> peek val'
            xx <- peekElemOff mat' 0
            xy <- peekElemOff mat' 1
            yx <- peekElemOff mat' 2
            yy <- peekElemOff mat' 3
            return $ Just $ ValueMatrix $ V2 (V2 xx xy) (V2 yx yy)
        6 -> do
            val <- throwNull <$> peek val'
            Just <$> ValueCharSet <$> thawCharSet val
        7 -> Just <$> ValueFTFace <$> throwNull <$> peek val'
        8 -> do
            val <- throwNull <$> peek val'
            Just <$> ValueLangSet <$> thawLangSet val
        9 -> do
            val <- throwNull <$> peek val'
            Just <$> ValueRange <$> thawRange val
        _ -> return Nothing
