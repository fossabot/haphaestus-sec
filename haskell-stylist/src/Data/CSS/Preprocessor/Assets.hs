{-# LANGUAGE OverloadedStrings #-}
-- | Utilities for rewriting URLs referenced via CSS properties.
module Data.CSS.Preprocessor.Assets(StyleAssets(..), URIRewriter(..)) where

-- TODO Unit test!
import           Data.Text as Txt hiding (elem)
import           Network.URI
import qualified Data.CSS.Syntax.StyleSheet as CSS
import qualified Data.CSS.Syntax.Tokens as CSSTok
import           Data.List (nub, elem)

-- | Extracts referenced URLs from specified properties.
data StyleAssets = StyleAssets {
    -- | The properties from which to extract URLs.
    filterProps :: [Txt.Text],
    -- | The extracted URLs.
    assets :: [URI]
}

instance CSS.StyleSheet StyleAssets where
    addRule self (CSS.StyleRule _ props _) =
        StyleAssets (filterProps self) $ nub (
            assets self ++ [uri | (prop, val) <- props,
                    prop `elem` filterProps self,
                    CSSTok.Url text <- val,
                    Just uri <- [parseAbsoluteURI $ Txt.unpack text]]
            )


-- | Substitutes in given URLs into a property value.
rewritePropertyVal :: [(URI, URI)] -> [CSSTok.Token] -> [CSSTok.Token] 
rewritePropertyVal rewrites (CSSTok.Url text:vals)
    | Just uri <- parseURIReference $ Txt.unpack text, Just rewrite <- uri `lookup` rewrites =
        CSSTok.Url (Txt.pack $ uriToString id rewrite "") : rewritePropertyVal rewrites vals
    | otherwise = CSSTok.Url "" : rewritePropertyVal rewrites vals
rewritePropertyVal rewrites (val:vals) = val:rewritePropertyVal rewrites vals
rewritePropertyVal _ [] = []

-- | Substitutes in given URLs into the inner stylesheet being parsed.
data URIRewriter s = URIRewriter [(URI, URI)] s
instance CSS.StyleSheet s => CSS.StyleSheet (URIRewriter s) where
    setPriority p (URIRewriter r s) = URIRewriter r $ CSS.setPriority p s
    addRule (URIRewriter r s) (CSS.StyleRule sel props psuedo) =
        URIRewriter r $ CSS.addRule s $ CSS.StyleRule sel [
            (prop, rewritePropertyVal r val) | (prop, val) <- props
        ] psuedo
    addAtRule (URIRewriter r s) name toks =
        let (self', toks') = CSS.addAtRule s name toks in (URIRewriter r self', toks')
