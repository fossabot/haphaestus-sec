{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.URI.Fetch
-- Input parsing
import System.Environment (getArgs)
import Network.URI (parseURI, nullURI)
import Data.Maybe (fromJust)
-- Where to save files
import System.Directory (getCurrentDirectory)
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = do
    url:encoding:args <- getArgs
    let url' = fromJust $ parseURI url
    putStrLn encoding
    session <- newSession
    dir <- getCurrentDirectory

    resp <- submitURL' session ["*/*"] url' "POST" (C8.pack encoding) $map parseArg args
    res <- saveDownload nullURI dir resp
    putStrLn $ show res

parseArg ('-':arg) | (key, '=':value) <- break (== '=') arg = (key, Left value)
    | otherwise = (arg, Left "")
parseArg ('+':arg) | (key, '=':value) <- break (== '=') arg = (key, Right value)
    | otherwise = (arg, Left "")
parseArg arg | (key, '=':value) <- break (== '=') arg = (key, Left value)
    | otherwise = (arg, Left "")
