{-# LANGUAGE MagicHash, UnboxedTuples #-}
-- | Published almost entirely for benchmarks, comparing to stdlib!
-- Should have little direct interest to Harfbuzz callers, & I'm not promising a stable API.
-- This is here because, as it turns out, Harfbuzz can return a lot of output!
module Data.Text.Glyphize.Array where

import Foreign.Storable (Storable(..))
import Foreign.ForeignPtr (ForeignPtr, plusForeignPtr, withForeignPtr, mallocForeignPtrArray)
import Foreign.Ptr
import Foreign.Marshal.Array (copyArray)

import GHC.IO (IO(IO))
import GHC.Exts (realWorld#, oneShot)

-- | Clone the given array so it can be freed without the losing access to the data.
-- Uses `memcpy` so it gets very heavily optimized by the OS.
clonePtr :: Storable a => Ptr a -> Int -> IO (ForeignPtr a)
clonePtr ptr l = do
    ret <- mallocForeignPtrArray l
    withForeignPtr ret $ \ptr' -> copyArray ptr' ptr l
    return ret
-- | Iterate over an array in a ForeignPtr, no matter how small or large it is.
peekLazy :: Storable a => ForeignPtr a -> Int -> [a]
peekLazy fp 0 = []
peekLazy fp n
    | n <= chunkSize = withFP $ peekEager [] n
    | otherwise = withFP $ peekEager (plusForeignPtr fp chunkSize `peekLazy` (-) n chunkSize) chunkSize
  where withFP = accursedUnutterablePerformIO . withForeignPtr fp
-- | Variation of peekArray, taking a tail to append to the decoded list.
peekEager :: Storable a => [a] -> Int -> Ptr a -> IO [a]
peekEager acc 0 ptr = return acc
peekEager acc n ptr = let n' = pred n in do
    e <- peekElemOff ptr n'
    peekEager (e:acc) n' ptr
-- | How many words should be decoded by `peekLazy` & `iterateLazy`.
chunkSize :: Int
chunkSize = 1024 -- 4k, benchmarks seem to like it!
-- | Convert an array from C code into a Haskell list,
-- performant no matter how small or large it is.
iterateLazy :: Storable a => Ptr a -> Int -> IO [a]
iterateLazy ptr l
  | l < 0 = putStrLn ("Invalid array length: " ++ show l) >> return []
  | l == 0 = return []
  | otherwise = do
    fp <- clonePtr ptr l
    return $ noCache peekLazy fp $ fromEnum l

-- | This \"function\" has a superficial similarity to 'System.IO.Unsafe.unsafePerformIO' but
-- it is in fact a malevolent agent of chaos. It unpicks the seams of reality
-- (and the 'IO' monad) so that the normal rules no longer apply. It lulls you
-- into thinking it is reasonable, but when you are not looking it stabs you
-- in the back and aliases all of your mutable buffers. The carcass of many a
-- seasoned Haskell programmer lie strewn at its feet.
--
-- Witness the trail of destruction:
--
-- * <https://github.com/haskell/bytestring/commit/71c4b438c675aa360c79d79acc9a491e7bbc26e7>
--
-- * <https://github.com/haskell/bytestring/commit/210c656390ae617d9ee3b8bcff5c88dd17cef8da>
--
-- * <https://github.com/haskell/aeson/commit/720b857e2e0acf2edc4f5512f2b217a89449a89d>
--
-- * <https://ghc.haskell.org/trac/ghc/ticket/3486>
--
-- * <https://ghc.haskell.org/trac/ghc/ticket/3487>
--
-- * <https://ghc.haskell.org/trac/ghc/ticket/7270>
--
-- * <https://gitlab.haskell.org/ghc/ghc/-/issues/22204>
--
-- Do not talk about \"safe\"! You do not know what is safe!
--
-- Yield not to its blasphemous call! Flee traveller! Flee or you will be
-- corrupted and devoured!
--
{-# INLINE accursedUnutterablePerformIO #-}
accursedUnutterablePerformIO :: IO a -> a
accursedUnutterablePerformIO (IO m) = case m realWorld# of (# _, r #) -> r

-- | Harfbuzz produces ~40x as much output data as its input data.
-- In many applications that input data would be a large fraction of its heap.
-- As such, unless callers are processing these results, it is usually more
-- efficient for Haskell to recompute the glyphs than to store them.
--
-- This synonym of `oneShot` is used to instruct Haskell of this fact.
noCache :: (a -> b) -> a -> b
noCache = oneShot
