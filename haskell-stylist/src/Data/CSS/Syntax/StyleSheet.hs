{-# LANGUAGE OverloadedStrings #-}
-- | Parses a CSS stylesheet
-- See `StyleSheet` & `parseForURL`.
--
-- Backwards-compatability module, this API has been moved out into "stylist-traits".
module Data.CSS.Syntax.StyleSheet (
        parse, parse', parseForURL, TrivialStyleSheet(..),
        StyleSheet(..), skipAtRule, scanAtRule, scanBlock, skipSpace,
        StyleRule(..),
        -- For parsing at-rules, HTML "style" attribute, etc.
        parseProperties, parseProperties',
        -- for testing
        scanValue
    ) where

import Stylist.Parse
