{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Main where
import Criterion.Main
import Data.Text.Glyphize
import Data.FileEmbed
import System.FilePath ((</>))
import qualified Data.Text.Foreign as Txt
import qualified Data.Text.Lazy as Txt

import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.Word (Word8)

-- Benchmarking these as well...
import Foreign.Marshal.Array (peekArray)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrArray)
import System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO, unsafeInterleaveIO)
import Data.Text.Glyphize.Array

shapeStr txt = shape font defaultBuffer { text = txt } []
  where font = createFont $ createFace $(
                    makeRelativeToProject ("assets" </> "Lora-Regular.ttf") >>=
                    embedFile) 0

dracula = $(makeRelativeToProject ("bench" </> "dracula.txt") >>= embedStringFile)

main = defaultMain [
    bgroup "Dracula" [
        bench "Week-Head" $ whnf shapeStr dracula,
        bench "Normal Form" $ nf shapeStr dracula,
        bench "Paragraphs" $ nf (map shapeStr) $ Txt.lines dracula,
        bench "Words" $ nf (map shapeStr) $ Txt.words dracula,
        bench "Parallelised" $ nf (parMap rdeepseq shapeStr) $ Txt.lines dracula,
        bench "Parallelised words" $ nf (parMap rdeepseq shapeStr) $
            Txt.words dracula
    ],
    bench "Word" $ nf shapeStr "Dracula",
    bgroup "building blocks" [
        bench "peekArray (NF)" $ nfIO $ Txt.useAsPtr (Txt.toStrict dracula) $
            \ptr l -> peekArray (fromEnum l) ptr,
        bench "peekArray" $ whnfIO $ Txt.useAsPtr (Txt.toStrict dracula) $
            \ptr l -> peekArray (fromEnum l) ptr,
        bench "alloc foreign ptr" $ whnfIO (mallocForeignPtrArray $
            fromEnum $ Txt.length dracula :: IO (ForeignPtr Word8)),
        bench "clone ptr" $ whnfIO $ Txt.useAsPtr (Txt.toStrict dracula) $
            \ptr l -> clonePtr ptr $ fromEnum l,
        bench "peek lazy" $ whnfIO (Txt.asForeignPtr (Txt.toStrict dracula) >>=
            \(ptr, l) -> return $ peekLazy ptr $ fromEnum l),
        bench "iterate lazy" $ whnfIO $ Txt.useAsPtr (Txt.toStrict dracula) $
            \ptr l -> iterateLazy ptr $ fromEnum l,
        bench "peek lazy (NF)" $ nfIO $ (Txt.asForeignPtr (Txt.toStrict dracula) >>=
            \(ptr, l) -> return $ peekLazy ptr $ fromEnum l),
        bench "iterate lazy (NF)" $ nfIO $ Txt.useAsPtr (Txt.toStrict dracula) $
            \ptr l -> iterateLazy ptr $ fromEnum l,
        -- These benchmarks give unconfident results, thought they'd be interesting...
        bench "unsafePerformIO" $ whnf unsafePerformIO $ return (),
        bench "unsafeDupablePerformIO" $ whnf unsafeDupablePerformIO $ return (),
        bench "unsafeInterleaveIO" $ whnfIO $ unsafeInterleaveIO $ return (),
        bench "accursedUnutterablePerformIO" $ whnf accursedUnutterablePerformIO $ return (),
        bench "peek kilo array" $ nfIO $ Txt.useAsPtr (Txt.toStrict $ Txt.take 1024 dracula) $
            \ptr l -> peekArray (fromEnum l) ptr,
        bench "lazy kilo array" $ nfIO (Txt.asForeignPtr (Txt.toStrict $ Txt.take 1024 dracula) >>=
                \(ptr, l) -> return $ peekLazy ptr $ fromEnum l)
    ]
  ]
