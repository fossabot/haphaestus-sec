module Main where

import Network.URI.Fetch
-- Input parsing
import System.Environment (getArgs)
import Network.URI (parseURI, nullURI)
import Data.Maybe (catMaybes)
-- Where to save files
import System.Directory (getCurrentDirectory)

main :: IO ()
main = do
    urls <- getArgs
    let urls' = catMaybes $ map parseURI urls
    session <- newSession
    dir <- getCurrentDirectory

    res <- fetchURLs session ["*/*"] urls' $ saveDownload nullURI dir
    putStrLn $ show res
