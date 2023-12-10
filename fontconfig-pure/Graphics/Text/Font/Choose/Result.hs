module Graphics.Text.Font.Choose.Result (Result(..), Word8, resultFromPointer,
    Error(..), throwResult, throwInt, throwPtr, throwFalse, throwNull) where

import Foreign.Storable (peek)
import Foreign.Ptr (Ptr, nullPtr)
import Control.Exception (throwIO, throw, Exception)
import Data.Word (Word8)

data Result = Match | NoMatch | TypeMismatch | ResultNoId | OutOfMemory | Other
    deriving (Eq, Show, Read, Enum, Bounded)

resultFromPointer :: Ptr Word8 -> IO Result
resultFromPointer res = toEnum8 <$> peek res

toEnum8 :: Enum a => Word8 -> a
toEnum8 = toEnum . fromEnum

data Error = ErrTypeMismatch | ErrResultNoId | ErrOutOfMemory deriving (Eq, Show, Read)
instance Exception Error

throwResult :: Result -> IO a -> IO (Maybe a)
throwResult Match x = Just <$> x
throwResult NoMatch _ = return Nothing
throwResult TypeMismatch _ = throwIO ErrTypeMismatch
throwResult ResultNoId _ = throwIO ErrResultNoId
throwResult OutOfMemory _ = throwIO ErrOutOfMemory

throwInt :: Int -> IO a -> IO (Maybe a)
throwInt x
    | x >= 0 && x <= 4 = throwResult $ toEnum x
    | otherwise = throwResult $ Other
throwPtr :: Ptr Word8 -> IO a -> IO (Maybe a)
throwPtr a b = resultFromPointer a >>= flip throwResult b

throwFalse :: Bool -> IO ()
throwFalse True = return ()
throwFalse False = throwIO ErrOutOfMemory
throwFalse' :: IO Bool -> IO ()
throwFalse' = (>>= throwFalse)

throwNull :: Ptr a -> Ptr a
throwNull ptr | ptr == nullPtr = throw ErrOutOfMemory
    | otherwise = ptr
