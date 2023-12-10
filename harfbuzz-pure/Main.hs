{-# LANGUAGE PackageImports, OverloadedStrings #-}
module Main where

import "harfbuzz-pure" Data.Text.Glyphize
import Control.Parallel.Strategies (parMap, rdeepseq)

import Data.Text.Lazy (pack)
import qualified Data.ByteString as BS
import System.Environment (getArgs)

shapeStr font word = shape font defaultBuffer { text = pack word } []

main :: IO ()
main = do
    print versionString
    words <- getArgs
    if Prelude.null words
    then print $ guessSegmentProperties $ defaultBuffer { text = "Testing, testing"}
    else do
      blob <- BS.readFile "assets/Lora-Regular.ttf"
      let font = createFont $ createFace blob 0
      case words of
        "!":words' -> print $ shape font (defaultBuffer { text = pack $ unwords words' }) []
        _ -> print $ parMap rdeepseq (shapeStr font) words
