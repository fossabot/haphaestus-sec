{-# LANGUAGE FlexibleContexts #-}
module Network.URI.PlugIns.Rewriters(parseRewriter, parseRewriters, Rewriter, applyRewriter) where

import Text.RE.Tools.Edit
import Text.RE.TDFA.String
import Network.URI (URI, uriToString, parseAbsoluteURI)
import Data.Maybe (catMaybes, fromMaybe)

import System.Directory as Dir
import System.FilePath ((</>))
import Control.Concurrent.Async (forConcurrently)

type Rewriter = Edits Maybe RE String
parseRewriter :: FilePath -> IO Rewriter
parseRewriter filepath = do
    source <- readFile filepath
    let parseLine line | [pattern, template] <- words line = compileSearchReplace pattern template
                | [pattern] <- words line = compileSearchReplace pattern "about:blank"
                | otherwise = Nothing
    let edits = catMaybes $ map parseLine $ lines source
    return $ Select $ map Template edits

parseRewriters :: String -> IO Rewriter
parseRewriters app = do
    dir <- Dir.getXdgDirectory Dir.XdgConfig "nz.geek.adrian.hurl"
    exists <- Dir.doesDirectoryExist dir
    if exists then do
        rewriters <- loadRewriters dir

        let inner = dir </> app
        innerExists <- Dir.doesDirectoryExist dir
        if innerExists then do
            appRewriters <- loadRewriters inner
            return $ Select (appRewriters ++ rewriters)
        else return $ Select rewriters
    else return $ Select []
  where
    loadRewriters dir = do
        files <- Dir.listDirectory dir
        raw <- forConcurrently files $ \file -> do
            exists <- doesFileExist file
            if exists then do
                rewriter <- parseRewriter file
                return $ case rewriter of
                    Select x -> x
                    Pipe x -> x
            else return []
        return $ concat raw

applyRewriter :: Rewriter -> URI -> Maybe URI
applyRewriter rewriter uri = parseAbsoluteURI =<<
        applyEdits firstLine rewriter (uriToString id uri "")
