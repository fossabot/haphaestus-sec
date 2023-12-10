{-# LANGUAGE PackageImports #-}
module Main where

import "fontconfig-pure" Graphics.Text.Font.Choose as Font

import System.Environment (getArgs)
import Control.Monad (forM)

main :: IO ()
main = do
    args <- getArgs
    let (all, name, objects) = case args of {
        [] -> (False, "serif", []);
        "!":name:objects -> (True, name, objects);
        name:objects -> (False, name, objects)}
    let query = nameParse name
    print query
    let query' = defaultSubstitute $ configSubstitute' query MatchPattern
    print query'

    case fontSort' query' all of
        Just (res, charset) -> do
            print charset
            forM res $ \res' -> print $ fontRenderPrepare' query' res'
            return ()
        Nothing -> putStrLn "No results!"
