{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.UI.GLUT
import Graphics.GL.Core32

import FreeType.Core.Base
import FreeType.FontConfig (instantiatePattern, bmpAndMetricsForIndex, FTFC_Subpixel(..))

import Graphics.Text.Font.Choose as Font
import Typograffiti (makeDrawGlyphs, allocAtlas, AllocatedRendering(..), move)
import Linear (V2(..))

import Data.Text.Glyphize
import Data.Text.Glyphize.Choose

import System.Environment (getArgs)
import System.Exit (die)
import Control.Monad.Except (runExceptT, liftIO)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    (progName, args) <- getArgsAndInitialize

    w <- createWindow progName

    args <- getArgs
    let query = nameParse $ case args of
            [] -> "serif"
            (q:_) -> q
    let query' = defaultSubstitute $ configSubstitute' query MatchPattern
    font <- case fontSort' query' False of
        Just (f:_, _) -> return f
        _ -> do
            die ("Failed to locate font " ++ show query)

    let buf = defaultBuffer { text = "sphinx of black quartz judge my vow" }
    let glyphs = shape (pattern2hbfont font []) buf []

    ft_With_FreeType $ \ft -> do
        inst <- instantiatePattern ft font (fromMaybe 12 $ getValue' "size" font, 20)
        res <- runExceptT $ do
            render <- makeDrawGlyphs
            let codepoints = map codepoint $ map fst $ glyphs
            atlas <- allocAtlas (liftIO . bmpAndMetricsForIndex inst SubpixelNone) codepoints
            renderer <- render atlas glyphs
            return renderer
        renderer <- case res of
            Left err -> die $ show err
            Right r -> return r

        displayCallback $= do
            clear [ ColorBuffer ]
            Size x y <- get windowSize
            arDraw renderer [move 0 50] $ V2 (fromEnum x) (fromEnum y)
            flush
        mainLoop
