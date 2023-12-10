{-# LANGUAGE OverloadedStrings #-}
-- | Lowers psuedoclasses to rawer syntactic forms.
module Data.CSS.Preprocessor.PsuedoClasses(LowerPsuedoClasses(..),
    psuedoClassesFilter, htmlPsuedoFilter,
    addRewrite, addRewrite', addTest, addContains, PropertyTest,
    addPsuedoEl, addNamespace) where

import Data.CSS.Syntax.StyleSheet
import Data.CSS.Syntax.Selector
import Data.CSS.Syntax.Tokens

import Data.Text as Txt
import Data.Maybe (fromMaybe, listToMaybe)
import Data.HashMap.Lazy as HM
import Data.Function ((&))
import Data.List as L (intercalate)

--------
---- core
--------
type RewriteMap = HashMap Text ([Token] -> [SimpleSelector])
data LowerPsuedoClasses s = LowerPsuedoClasses {
    inner :: s,
    rewriteRules :: RewriteMap,
    psuedoEls :: [Text],
    namespaces :: HashMap Text Text
}

instance StyleSheet s => StyleSheet (LowerPsuedoClasses s) where
    setPriority p self = self { inner = setPriority p $ inner self }
    addRule self rule = self { inner = addRule (inner self) $ lowerRule self rule }

    addAtRule self "namespace" (Ident ns:toks) | (Url url:toks') <- skipSpace toks =
        (addNamespace ns url self, toks')
    addAtRule self name toks = let (inner', toks') = addAtRule (inner self) name toks
        in (self { inner = inner' }, toks')

lowerRule :: LowerPsuedoClasses t -> StyleRule -> StyleRule
lowerRule self@(LowerPsuedoClasses { psuedoEls = psuedos }) (StyleRule sel props "")
    | Just pseudo <- extractPseudoEl psuedos sel =
        lowerRule (addRewrite' pseudo [] self) $ StyleRule sel props pseudo
lowerRule LowerPsuedoClasses { namespaces = ns, rewriteRules = rewrites } (StyleRule sel props pseudoel) =
    StyleRule (lowerSelector ns rewrites sel) props pseudoel

extractPseudoEl :: [Text] -> Selector -> Maybe Text
extractPseudoEl ps (Element sel) = extractPseudoEl' ps sel
extractPseudoEl ps (Child _ sel) = extractPseudoEl' ps sel
extractPseudoEl ps (Descendant _ sel) = extractPseudoEl' ps sel
extractPseudoEl ps (Adjacent _ sel) = extractPseudoEl' ps sel
extractPseudoEl ps (Sibling _ sel) = extractPseudoEl' ps sel
extractPseudoEl' :: [Text] -> [SimpleSelector] -> Maybe Text
extractPseudoEl' ps sel = listToMaybe [p | Psuedoclass p [] <- sel, p `Prelude.elem` ps]

lowerSelector :: HashMap Text Text -> RewriteMap -> Selector -> Selector
lowerSelector ns rewrites (Element sel') = Element $ lowerSelector' ns rewrites sel'
lowerSelector ns rewrites (Child p sel') =
    Child (lowerSelector ns rewrites p) $ lowerSelector' ns rewrites sel'
lowerSelector ns rewrites (Descendant p sel') =
    Descendant (lowerSelector ns rewrites p) $ lowerSelector' ns rewrites sel'
lowerSelector ns rewrites (Adjacent sib sel') =
    Adjacent (lowerSelector ns rewrites sib) $ lowerSelector' ns rewrites sel'
lowerSelector ns rewrites (Sibling sib sel') =
    Sibling (lowerSelector ns rewrites sib) $ lowerSelector' ns rewrites sel'

lowerSelector' :: HashMap Text Text -> RewriteMap -> [SimpleSelector] -> [SimpleSelector]
lowerSelector' namespaces' rewrites (Namespace ns:sels) =
    Namespace (fromMaybe "about:invalid" $ HM.lookup ns namespaces') : lowerSelector' namespaces' rewrites sels
lowerSelector' ns rewrites (Psuedoclass name args:sels)
    | Just value <- name `HM.lookup` rewrites = value args ++ lowerSelector' ns rewrites sels
lowerSelector' ns rewrites (sel:sels) = sel : lowerSelector' ns rewrites sels
lowerSelector' _ _ [] = []

--------
---- constructors
--------
psuedoClassesFilter :: StyleSheet s => s -> LowerPsuedoClasses s
psuedoClassesFilter s = LowerPsuedoClasses s HM.empty ["before", "after"] HM.empty

addPsuedoEl :: Text -> LowerPsuedoClasses s -> LowerPsuedoClasses s
addPsuedoEl ps self = self { psuedoEls = psuedoEls self ++ Txt.words ps }

addRewrite :: Text -> Text -> LowerPsuedoClasses s -> LowerPsuedoClasses s
addRewrite name sel self = addRewrite' name (tokenize sel) self
addRewrite' :: Text -> [Token] -> LowerPsuedoClasses s -> LowerPsuedoClasses s
addRewrite' name sel self =
    addTest' name (\args -> [Psuedoclass "where" $ spliceArgs sel args]) self
  where
    spliceArgs [] [] = []
    spliceArgs (Ident "_":toks) (arg:args) = arg : spliceArgs toks args
    spliceArgs (tok:toks) args = tok : spliceArgs toks args
    spliceArgs _ _ = [Ident "\tfail"]

addContains :: Text -> [Int] -> LowerPsuedoClasses s -> LowerPsuedoClasses s
addContains name path self =
    addRewrite' name (L.intercalate [Comma] $ buildSelector path [Colon, Ident "root"]) self
  where
    buildSelector (p:ps) prefix =
        let prefix' = prefix ++ [Delim '>', Colon, Function "nth-child", num p, RightParen]
        in prefix' : buildSelector ps prefix'
    buildSelector [] _ = []
    num x = Number (Txt.pack $ show x) $ NVInteger (toInteger x)

addTest :: Text -> Maybe Text -> Text -> PropertyFunc -> LowerPsuedoClasses s -> LowerPsuedoClasses s
addTest name ns attr test self = addTest' name (noArg [Property ns attr $ Callback test]) self
    where
        noArg sel [] = sel
        noArg _ _ = [Psuedoclass " fail" []]
addTest' :: Text -> ([Token] -> [SimpleSelector]) -> LowerPsuedoClasses s -> LowerPsuedoClasses s
addTest' name sel self = self {rewriteRules = insert name sel $ rewriteRules self }

addNamespace :: Text -> Text -> LowerPsuedoClasses s -> LowerPsuedoClasses s
addNamespace ns uri self = self { namespaces = insert ns uri $ namespaces self }

htmlPsuedoFilter :: StyleSheet s => s -> LowerPsuedoClasses s
htmlPsuedoFilter s = psuedoClassesFilter s &
    addRewrite "link" "[href]" &
    addRewrite "any-link" "[href]" &
    addRewrite "blank" "[value=''], :not([value])" &
    addRewrite "checked" "[checked]" &
    addRewrite "dir" "[dir=_], [dir=_] *" &
    addRewrite "disabled" "[disabled]" &
    addRewrite "enabled" ":not([disabled])" &
    addRewrite "first-child" ":nth-child(1)" &
    addRewrite "first-of-type" ":nth-of-type(1)" &
    addRewrite "indeterminate" "[indeterminate]" &
    addRewrite "lang" "[lang|=_], [lang|=_] *" &
    -- Not sure if I ever really want to support these, but might as well list them.
    -- Requires more data to be fed to the core CSS engine.
    addRewrite "last-child" ":nth-last-child(1)" &
    addRewrite "last-of-type" ":nth-last-of-type(1)" &
    addRewrite "only-child" ":nth-child(1):nth-last-child(1)" &
    addRewrite "only-of-type" ":nth-of-type(1):nth-last-of-type(1)" &
    -- No issue with remainder.
    addRewrite "optional" ":not([required])" &
    addRewrite "placeholder-shown" "[value=''][placeholder], [placeholder]:not([value])" &
    addRewrite "readonly" "[readonly], [disabled]" &
    addRewrite "read-write" ":not([readonly]):not([disabled])" &
    addRewrite "required" "[required]" &
    addRewrite "scope" ":root" &
    addRewrite "root" "html"
