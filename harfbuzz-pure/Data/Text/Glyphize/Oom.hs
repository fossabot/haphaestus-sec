module Data.Text.Glyphize.Oom where

import Control.Exception
import Foreign.Ptr (nullPtr, Ptr)

-- | Indicates that Harfbuzz has ran out of memory during a computation.
-- Should be extremely rare!
data HarfbuzzError = OutOfMemory deriving (Show)
instance Exception HarfbuzzError

throwFalse :: IO Bool -> IO ()
throwFalse cb = do
    ret <- cb
    if ret then return () else throwIO OutOfMemory

throwNull :: IO (Ptr a) -> IO (Ptr a)
throwNull cb = do
    ptr <- cb
    if ptr == nullPtr then throwIO OutOfMemory else return ptr
