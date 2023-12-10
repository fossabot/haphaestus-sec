{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Data.HashMap.Strict
import qualified Data.HashMap.Lazy as L
import Data.Maybe (fromJust)
import Network.URI
import Data.Scientific (toRealFloat)

import Data.CSS.Syntax.Tokens
import Data.CSS.Syntax.StyleSheet (parse, StyleSheet(..), TrivialStyleSheet(..), scanAtRule, scanValue)
import Data.CSS.Syntax.Selector

import Data.CSS.Syntax.AtLayer

import Data.CSS.Style.Common
import Data.CSS.Style.Selector.Index
import Data.CSS.Style.Selector.Interpret
import Data.CSS.Style
import Data.CSS.StyleTree

import Data.CSS.Preprocessor.Conditions
import Data.CSS.Preprocessor.Conditions.Expr (Datum(..), Op(..), parse', eval)
import qualified Data.CSS.Preprocessor.Text as Txt

import qualified Data.CSS.Preprocessor.Text.CounterStyle as Ctr

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Canary" $ do
        it "Test framework works" $ do
            True `shouldBe` True
    describe "Parsing" $ do
        it "Can scan @rules" $ do
            scanAtRule [Ident "utf-8", Semicolon, Ident "a"] `shouldBe` ([Ident "utf-8", Semicolon], [Ident "a"])
            scanAtRule [Ident "before", LeftCurlyBracket, Ident "inside", RightCurlyBracket, Ident "after"] `shouldBe` (
                [Ident "before", LeftCurlyBracket, Ident "inside", RightCurlyBracket],
                [Ident "after"])
        it "Ignores @rules" $ do
            parse emptyStyle "@encoding 'utf-8';" `shouldBe` emptyStyle
            parse emptyStyle "  @encoding 'utf-8';" `shouldBe` emptyStyle
            parse emptyStyle "@encoding 'utf-8';  " `shouldBe` emptyStyle
            parse emptyStyle "@media print { a:link {color: green;} }" `shouldBe` emptyStyle
            parse emptyStyle "  @media print { a:link {color: green;} }" `shouldBe` emptyStyle
            parse emptyStyle "@media print { a:link {color: green;} }  " `shouldBe` emptyStyle

            parse emptyStyle "@encoding 'utf-8'; a {color:green}" `shouldBe` linkStyle
            parse emptyStyle "a {color:green}@encoding 'utf-8';" `shouldBe` linkStyle
            parse emptyStyle "@media print{a{color:black;}}a {color:green}" `shouldBe` linkStyle
            parse emptyStyle "a {color:green} @media print {a{color:black;}}" `shouldBe` linkStyle
        it "Parses style rules" $ do
            -- Syntax examples from "Head First HTML & CSS with XHTML"
            parse emptyStyle "bedroom { drapes: blue; carpet: wool shag; }" `shouldBe` TrivialStyleSheet [
                StyleRule (Element [Tag "bedroom"]) [
                    ("drapes", [Ident "blue"]),
                    ("carpet", [Ident "wool", Ident "shag"])
                ] ""]
            parse emptyStyle "  bathroom{tile :1in white;drapes :pink}" `shouldBe` TrivialStyleSheet [
                StyleRule (Element [Tag "bathroom"]) [
                    ("tile", [Dimension "1" (NVInteger 1) "in", Ident "white"]),
                    ("drapes", [Ident "pink"])
                ] ""]
        it "Parses selectors" $ do
            parse emptyStyle ".class {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Class "class"]) [] ""
                ]
            parse emptyStyle "*.class {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Class "class"]) [] ""
                ]
            parse emptyStyle "#id {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Id "id"]) [] ""
                ]
            parse emptyStyle "[attr] {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Property Nothing "attr" Exists]) [] ""
                ]
            parse emptyStyle "a , b {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Tag "b"]) [] "",
                    StyleRule (Element [Tag "a"]) [] ""
                ]
            parse emptyStyle "a b {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Descendant (Element [Tag "a"]) [Tag "b"]) [] ""
                ]
            parse emptyStyle "a > b {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Child (Element [Tag "a"]) [Tag "b"]) [] ""
                ]
            parse emptyStyle "a ~ b {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Sibling (Element [Tag "a"]) [Tag "b"]) [] ""
                ]
            parse emptyStyle "a + b {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Adjacent (Element [Tag "a"]) [Tag "b"]) [] ""
                ]
            parse emptyStyle "a::before {}"
                `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Tag "a"]) [] "before"
                ]
            parse emptyStyle "a:before {}"
                `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Tag "a", Psuedoclass "before" []]) [] ""
                ]
    describe "Style Index" $ do
        it "Retrieves appropriate styles" $ do
            let index = addStyleRule styleIndex [0] $ styleRule' sampleRule
            let element = ElementNode {
                name = "a",
                namespace = "",
                parent = Nothing,
                previous = Nothing,
                attributes = [
                    Attribute "class" "" "external",
                    Attribute "href" "" "https://adrian.geek.nz/",
                    Attribute "id" "" "mysite"
                ]
            }
            let element2 = ElementNode {
                name = "b",
                namespace = "",
                parent = Just element,
                previous = Just element, -- Invalid tree, oh well.
                attributes = []
            }
            rulesForElement index element `shouldBe` [sampleRule]
            rulesForElement index element2 `shouldBe` []

            let rule1 = StyleRule (Element [Class "external"]) [("color", [Ident "green"])] ""
            let index1 = addStyleRule styleIndex [0] $ styleRule' rule1
            rulesForElement index1 element `shouldBe` [rule1]
            rulesForElement index1 element2 `shouldBe` []

            let rule2 = StyleRule (Element [Id "mysite"]) [("color", [Ident "green"])] ""
            let index2 = addStyleRule styleIndex [0] $ styleRule' rule2
            rulesForElement index2 element `shouldBe` [rule2]
            rulesForElement index2 element2 `shouldBe` []

            let rule3 = StyleRule (Element [Property Nothing "href" $ Prefix "https://"]) [("color", [Ident "green"])] ""
            let index3 = addStyleRule styleIndex [0] $ styleRule' rule3
            rulesForElement index3 element `shouldBe` [rule3]
            rulesForElement index3 element2 `shouldBe` []
    describe "Selector Compiler" $ do
        it "Correctly evaluates selectors" $ do
            let parentEl = ElementNode {
                name = "a",
                namespace = "",
                parent = Nothing,
                previous = Nothing,
                attributes = [
                    Attribute "class" "" "external secure link",
                    Attribute "href" "" "https://adrian.geek.nz/index.html",
                    Attribute "id" "" "mysite",
                    Attribute "lang" "" "en-US"
                ]
            }
            let sibling = ElementNode {
                name = "img",
                namespace = "",
                parent = Just parentEl,
                previous = Nothing,
                attributes = []
            }
            let child = ElementNode {
                name = "b",
                namespace = "",
                parent = Just parentEl,
                previous = Just sibling,
                attributes = []
            }

            let selector1 = compile (Element [Tag "a"])
            selector1 parentEl `shouldBe` True
            selector1 sibling `shouldBe` False
            selector1 child `shouldBe` False

            let selector2 = compile (Element [Class "external"])
            selector2 parentEl `shouldBe` True
            selector2 sibling `shouldBe` False
            selector2 child `shouldBe` False

            let selector3 = compile (Element [Id "mysite"])
            selector3 parentEl `shouldBe` True
            selector3 sibling `shouldBe` False
            selector3 child `shouldBe` False

            let selector4 = compile (Element [Property Nothing "lang" Exists])
            selector4 parentEl `shouldBe` True
            selector4 sibling `shouldBe` False
            selector4 child `shouldBe` False

            let selector5 = compile (Element [Property Nothing "class" $ Include "secure"])
            selector5 parentEl `shouldBe` True
            selector5 sibling `shouldBe` False
            selector5 child `shouldBe` False

            let selector6 = compile (Element [Property Nothing "href" $ Prefix "https://"])
            selector6 parentEl `shouldBe` True
            selector6 sibling `shouldBe` False
            selector6 child `shouldBe` False

            let selector7 = compile (Element [Property Nothing "href" $ Suffix ".html"])
            selector7 parentEl `shouldBe` True
            selector7 sibling `shouldBe` False
            selector7 child `shouldBe` False

            let selector8 = compile (Element [Property Nothing "href" $ Substring ".geek.nz"])
            selector8 parentEl `shouldBe` True
            selector8 sibling `shouldBe` False
            selector8 child `shouldBe` False

            let selector9 = compile (Element [Property Nothing "lang" $ Dash "en"])
            selector9 parentEl `shouldBe` True
            selector9 sibling `shouldBe` False
            selector9 child `shouldBe` False

            let selectorA = compile (Element [Property Nothing "lang" $ Dash "en-US"])
            selectorA parentEl `shouldBe` True
            selectorA sibling `shouldBe` False
            selectorA child `shouldBe` False

            let selectorB = compile (Element [Property Nothing "lang" $ Dash "en-UK"])
            selectorB parentEl `shouldBe` False
            selectorB sibling `shouldBe` False
            selectorB child `shouldBe` False

            -- TODO These could be tested better.
            let selectorC = compile $ Child (Element [Tag "a"]) [Tag "b"]
            selectorC parentEl `shouldBe` False
            selectorC sibling `shouldBe` False
            selectorC child `shouldBe` True

            let selectorD = compile $ Descendant (Element [Tag "a"]) [Tag "b"]
            selectorD parentEl `shouldBe` False
            selectorD sibling `shouldBe` False
            selectorD child `shouldBe` True

            let selectorE = compile $ Sibling (Element [Tag "img"]) [Tag "b"]
            selectorE parentEl `shouldBe` False
            selectorE sibling `shouldBe` False
            selectorE child `shouldBe` True

            let selectorF = compile $ Adjacent (Element [Tag "img"]) [Tag "b"]
            selectorF parentEl `shouldBe` False
            selectorF sibling `shouldBe` False
            selectorF child `shouldBe` True

    describe "Style resolution" $ do
        it "respects selector specificity" $ do
            let el = ElementNode {
                name = "a",
                namespace = "",
                parent = Nothing,
                previous = Nothing,
                attributes = [Attribute "class" "" "link"]
            }
            let rules = parse queryable "a.link {color: green} a {color: red}"
            let VarParser _ (TrivialPropertyParser style) = cascade rules el [] temp::(VarParser TrivialPropertyParser)
            style ! "color" `shouldBe` [Ident "green"]
        it "respects syntax order" $ do
            let el = ElementNode {
                name = "a",
                namespace = "",
                parent = Nothing,
                previous = Nothing,
                attributes = [Attribute "class" "" "link"]
            }
            let rules = parse queryable "a {color: red; color: green}"
            let VarParser _ (TrivialPropertyParser style) = cascade rules el [] temp::(VarParser TrivialPropertyParser)
            style ! "color" `shouldBe` [Ident "green"]

            let rules2 = parse queryable "a {color: red} a {color: green}"
            let VarParser _ (TrivialPropertyParser style2) = cascade rules2 el [] temp::(VarParser TrivialPropertyParser)
            style2 ! "color" `shouldBe` [Ident "green"]
        it "respects stylesheet precedence" $ do
            let el = ElementNode {
                name = "a",
                namespace = "",
                parent = Nothing,
                previous = Nothing,
                attributes = [Attribute "class" "" "link"]
            }
            let rules = parse (queryable {priority = [1]}) "a {color: green}"
            let rules2 = parse (rules {priority = [2]}) "a {color: red}" :: QueryableStyleSheet (VarParser TrivialPropertyParser)
            let VarParser _ (TrivialPropertyParser style) = cascade rules2 el [] temp::(VarParser TrivialPropertyParser)
            style ! "color" `shouldBe` [Ident "green"]

            let el' = ElementNode {
                name = "a",
                namespace = "",
                parent = Nothing,
                previous = Nothing,
                attributes = [Attribute "class" "" "link"]
            }
            let rules' = parse (queryable {priority = [1]}) "a {color: red}"
            let rules2' = parse (rules' {priority = [2]}) "a {color: green !important}" :: QueryableStyleSheet (VarParser TrivialPropertyParser)
            let VarParser _ (TrivialPropertyParser style') = cascade rules2' el' [] temp::(VarParser TrivialPropertyParser)
            style' ! "color" `shouldBe` [Ident "green"]
        it "respects overrides" $ do
            let el = ElementNode {
                name = "a",
                namespace = "",
                parent = Nothing,
                previous = Nothing,
                attributes = [Attribute "class" "" "link"]
            }
            let rules = parse queryable "a {color: red;}"
            let VarParser _ (TrivialPropertyParser style) = cascade rules el [("color", [Ident "green"])] temp::(VarParser TrivialPropertyParser)
            style ! "color" `shouldBe` [Ident "green"]
    describe "Parser freezes" $ do
        it "does not regress" $ do
            parse emptyStyle "output: {content: 'Output'; pitch: high}"
                `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Tag "output"]) [] ""
                ] -- Turned out to just be incorrect parsing
            parse emptyStyle "input, output {content: attr(value)}"
                `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Tag "output"]) [
                        ("content", [Function "attr", Ident "value", RightParen])
                    ] "",
                    StyleRule (Element [Tag "input"]) [
                        ("content", [Function "attr", Ident "value", RightParen])
                    ] ""
                ]
        it "paren balancing" $ do
            scanValue [RightParen] `shouldBe` ([], [RightParen])
            scanValue [LeftParen] `shouldBe` ([LeftParen], [])
            scanValue [Function "fn", LeftParen] `shouldBe` ([Function "fn", LeftParen], [])
            scanValue [Function "fn", Ident "arg", LeftParen] `shouldBe`
                ([Function "fn", Ident "arg", LeftParen], [])

    describe "CSS Variables" $ do
        it "are captured" $ do
            let parser = temp :: VarParser TrivialPropertyParser
            vars parser `shouldBe` []
            let parser1 = setVars [("--var", [Ident "value"])] parser
            vars parser1 `shouldBe` [("--var", [Ident "value"])]
            let parser2 = fromJust $ longhand parser parser1 "property" [Function "var", Ident "--var", RightParen]
            vars parser2 `shouldBe` [("--var", [Ident "value"])]
            let VarParser _ (TrivialPropertyParser style) = parser2
            style ! "property" `shouldBe` [Ident "value"]

            let el = ElementNode {
                name = "a",
                namespace = "",
                parent = Nothing,
                previous = Nothing,
                attributes = []
            }
            let rules = parse queryable "a {--var: value}"
            let VarParser v _ = cascade rules el [] temp
            v `shouldBe` [("--var", [Ident "value"])]
        it "applies within element" $ do
            let el = ElementNode {
                name = "a",
                namespace = "",
                parent = Nothing,
                previous = Nothing,
                attributes = []
            }
            let rules = parse queryable "a {--link: #f00; color: var(--link)}"
            let VarParser vars (TrivialPropertyParser style) = cascade rules el [] temp
            style ! "color" `shouldBe` [Hash HId "f00"]
            style ! "--link" `shouldBe` [Hash HId "f00"]
            vars `shouldBe` [("--link", [Hash HId "f00"])]
        it "inherits" $ do
            let parent = ElementNode {
                name = "a",
                namespace = "",
                parent = Nothing,
                previous = Nothing,
                attributes = []
            }
            let el = ElementNode {
                name = "b",
                namespace = "",
                parent = Just parent,
                previous = Nothing,
                attributes = []
            }
            let rules = parse queryable "a {--link: #f00} b {color: var(--link)}"
            let VarParser vars (TrivialPropertyParser style) = cascade rules el [] $ cascade rules parent [] temp
            vars `shouldBe` [("--link", [Hash HId "f00"])]
            style ! "color" `shouldBe` [Hash HId "f00"]
    describe "Conditional @rules" $ do
        it "handles normal rules" $ do
            let TrivialStyleSheet styles = resolve' $ parse conditional "a {color: green}"
            styles `shouldBe` [StyleRule (Element [Tag "a"]) [("color", [Ident "green"])] ""]

            let TrivialStyleSheet styles = resolve' $ parse conditional "@rule; a {color: green}"
            styles `shouldBe` [StyleRule (Element [Tag "a"]) [("color", [Ident "green"])] ""]

            let TrivialStyleSheet styles = resolve' $ parse conditional "a {color: green} @rule;"
            styles `shouldBe` [StyleRule (Element [Tag "a"]) [("color", [Ident "green"])] ""]

            let TrivialStyleSheet styles = resolve' $ parse conditional "a {color: green} @font {}"
            styles `shouldBe` [StyleRule (Element [Tag "a"]) [("color", [Ident "green"])] ""]

            let TrivialStyleSheet styles = resolve' $ parse conditional "@font {} a {color: green}"
            styles `shouldBe` [StyleRule (Element [Tag "a"]) [("color", [Ident "green"])] ""]
        it "handles @document" $ do
            let TrivialStyleSheet styles = resolve' $ parse conditional "@document url(about:blank) { a {color: green} }"
            styles `shouldBe` [StyleRule (Element []) [] "", StyleRule (Element [Tag "a"]) [("color", [Ident "green"])] ""]
            let TrivialStyleSheet styles = resolve' $ parse conditional "@document url(about:credits) { a {color: red} }"
            styles `shouldBe` []
            let TrivialStyleSheet styles = resolve' $ parse conditional "@document url-prefix('about:') { a {color: green} }"
            styles `shouldBe` [StyleRule (Element []) [] "", StyleRule (Element [Tag "a"]) [("color", [Ident "green"])] ""]
            let TrivialStyleSheet styles = resolve' $ parse conditional "@document url-prefix('https:') { a {color: red} }"
            styles `shouldBe` []
            let TrivialStyleSheet styles = resolve' $ parse conditional "@document media-document('test') { a {color: green} }"
            styles `shouldBe` [StyleRule (Element []) [] "", StyleRule (Element [Tag "a"]) [("color", [Ident "green"])] ""]
            let TrivialStyleSheet styles = resolve' $ parse conditional "@document media-document('other') { a {color: red} }"
            styles `shouldBe` []
        it "handles @media" $ do
            let TrivialStyleSheet styles = resolve' $ parse conditional "@media test { a {color: green} }"
            styles `shouldBe` [StyleRule (Element []) [] "", StyleRule (Element [Tag "a"]) [("color", [Ident "green"])] ""]
            let TrivialStyleSheet styles = resolve' $ parse conditional "@media screen { a {color: red} }"
            styles `shouldBe` []

            let TrivialStyleSheet styles = resolve' $ parse conditional "@media test or screen { a {color: green} }"
            styles `shouldBe` [StyleRule (Element []) [] "", StyleRule (Element [Tag "a"]) [("color", [Ident "green"])] ""]
            let TrivialStyleSheet styles = resolve' $ parse conditional "@media test or test {a {color: green} }"
            styles `shouldBe` [StyleRule (Element []) [] "", StyleRule (Element [Tag "a"]) [("color", [Ident "green"])] ""]
            let TrivialStyleSheet styles = resolve' $ parse conditional "@media screen or screen {a {color: red} }"
            styles `shouldBe` []
            let TrivialStyleSheet styles = resolve' $ parse conditional "@media screen or test {a {color: green} }"
            styles `shouldBe` [StyleRule (Element []) [] "", StyleRule (Element [Tag "a"]) [("color", [Ident "green"])] ""]

            let TrivialStyleSheet styles = resolve' $ parse conditional "@media test and screen { a {color: red} }"
            styles `shouldBe` []
            let TrivialStyleSheet styles = resolve' $ parse conditional "@media test and test { a {color: green} }"
            styles `shouldBe` [StyleRule (Element []) [] "", StyleRule (Element [Tag "a"]) [("color", [Ident "green"])] ""]
            let TrivialStyleSheet styles = resolve' $ parse conditional "@media screen and screen { a {color: red} }"
            styles `shouldBe` []
            let TrivialStyleSheet styles = resolve' $ parse conditional "@media screen and test {a {color: red} }"
            styles `shouldBe` []

            let TrivialStyleSheet styles = resolve' $ parse conditional "@media 2 < 3 { a {color: green} }"
            styles `shouldBe` [StyleRule (Element []) [] "", StyleRule (Element [Tag "a"]) [("color", [Ident "green"])] ""]
            let TrivialStyleSheet styles = resolve' $ parse conditional "@media 2 < 2 { a {color: red} }"
            styles `shouldBe` []
            let TrivialStyleSheet styles = resolve' $ parse conditional "@media 2 < 1 { a {color: red} }"
            styles `shouldBe` []
        it "handles @import" $ do
            let styles = parse conditional "@import url(about:style.css);"
            extractImports' styles `shouldBe` [URI "about:" Nothing "style.css" "" ""]
            let styles = parse conditional "@import 'about:style.css';"
            extractImports' styles `shouldBe` [URI "about:" Nothing "style.css" "" ""]

            let styles = parse conditional "@import url(about:style.css) test;"
            extractImports' styles `shouldBe` [URI "about:" Nothing "style.css" "" ""]
            let styles = parse conditional "@import url(about:style.css) screen;"
            extractImports' styles `shouldBe` []
        -- TODO @supports is harder to test

    describe "CSS Counters" $ do
        it "Propagates other properties" $ do
            let textStyle = temp :: Txt.TextStyle TrivialPropertyParser
            let textStyle1 = fromJust $ longhand temp textStyle "foo" [Ident "bar"]
            style (Txt.resolve $ StyleTree textStyle1 []) `shouldBe`
                TrivialPropertyParser (fromList [("foo", [Ident "bar"])])

            let textStyle2 = fromJust $ longhand temp textStyle1 "counter-reset" [Ident "heading"]
            style (Txt.resolve $ StyleTree textStyle2 []) `shouldBe`
                TrivialPropertyParser (fromList [("foo", [Ident "bar"])])

            let textStyle3 = fromJust $ longhand temp textStyle2 "counter-set" [Ident "heading"]
            style (Txt.resolve $ StyleTree textStyle3 []) `shouldBe`
                TrivialPropertyParser (fromList [("foo", [Ident "bar"])])

            let textStyle4 = fromJust $ longhand temp textStyle3 "counter-increment" [Ident "heading"]
            style (Txt.resolve $ StyleTree textStyle4 []) `shouldBe`
                TrivialPropertyParser (fromList [("foo", [Ident "bar"])])

            let textStyle5 = fromJust $ longhand temp textStyle4 "white-space" [Ident "normal"]
            style (Txt.resolve $ StyleTree textStyle5 []) `shouldBe`
                TrivialPropertyParser (fromList [("foo", [Ident "bar"]), ("white-space", [Ident "normal"])])

            let textStyle6 = fromJust $ longhand temp textStyle4 "white-space" [Ident "pre"]
            style (Txt.resolve $ StyleTree textStyle6 []) `shouldBe`
                TrivialPropertyParser (fromList [("foo", [Ident "bar"]), ("white-space", [Ident "nowrap"])])

            let textStyle7 = fromJust $ longhand temp textStyle4 "white-space" [Ident "nowrap"]
            style (Txt.resolve $ StyleTree textStyle7 []) `shouldBe`
                TrivialPropertyParser (fromList [("foo", [Ident "bar"]), ("white-space", [Ident "nowrap"])])

            let textStyle8 = fromJust $ longhand temp textStyle4 "white-space" [Ident "pre-wrap"]
            style (Txt.resolve $ StyleTree textStyle8 []) `shouldBe`
                TrivialPropertyParser (fromList [("foo", [Ident "bar"]), ("white-space", [Ident "normal"])])

            let textStyle9 = fromJust $ longhand temp textStyle4 "white-space" [Ident "pre-line"]
            style (Txt.resolve $ StyleTree textStyle9 []) `shouldBe`
                TrivialPropertyParser (fromList [("foo", [Ident "bar"]), ("white-space", [Ident "normal"])])

        it "Inserts counters" $ do
            let textStyle = temp :: Txt.TextStyle TrivialPropertyParser
            shorthand textStyle "content" [Function "counter", Ident "-rhaps-ol", RightParen] `shouldBe`
                [("content", [Function "counter", Ident "-rhaps-ol", RightParen])]
            shorthand textStyle "content" [Function "counters", Ident "-rhaps-ol", Comma, String ".", RightParen] `shouldBe`
                [("content", [Function "counters", Ident "-rhaps-ol", Comma, String ".", RightParen])]

            let textStyle1 = fromJust $ longhand temp textStyle "content" [Function "counter", Ident "-rhaps-ol", RightParen]
            style (Txt.resolve $ StyleTree textStyle1 []) `shouldBe` TrivialPropertyParser (fromList [("content", [])])

            let textStyle2 = fromJust $ longhand temp textStyle1 "counter-reset" [Ident "-rhaps-ol"]
            style (Txt.resolve $ StyleTree textStyle2 []) `shouldBe` TrivialPropertyParser (fromList [("content", [String "0"])])

            let textStyle3 = fromJust $ longhand temp textStyle1 "counter-set" [Ident "-rhaps-ol"]
            style (Txt.resolve $ StyleTree textStyle3 []) `shouldBe` TrivialPropertyParser (fromList [("content", [String "0"])])

            let textStyle4 = fromJust $ longhand temp textStyle1 "counter-increment" [Ident "-rhaps-ol"]
            style (Txt.resolve $ StyleTree textStyle4 []) `shouldBe` TrivialPropertyParser (fromList [("content", [String "1"])])
    describe "@layer" $ do
        it "Deduplicates names" $ do
            let init = Tree L.empty
            let tree2 = registerLayer ["LeagueOfGentlemenAdventurers", "The Stranger"] init
            let tree3 = registerLayer ["JusticeUnion", "TomTomorrow"] tree2
            let tree4 = registerLayer ["HomeTeam", "DocRocket"] tree3
            let tree5 = registerLayer ["JusticeUnion", "TheOgre"] tree4

            layerPath ["JusticeUnion", "TheOgre"] tree5 `shouldBe` [2, 2]
            layerPath ["HomeTeam"] tree5 `shouldBe` [3]
            layerPath ["LeagueOfGentlemenAdventurers"] tree5 `shouldBe` [1]
            uniqueName ["HomeTeam"] tree5 `shouldBe` ["HomeTeam", "1"]

    describe "@counter-style" $ do
        -- NOTE: Thes are mostly based on examples given in the spec
        -- https://w3c.github.io/csswg-drafts/css-counter-styles-3/#counter-style-system
        it "Handles system: cyclic" $ do
            let counter = Ctr.defaultCounter {
                Ctr.system = Ctr.Cyclic, Ctr.symbols = ["!", "@", "#"]
              }
            Ctr.counterRenderMarker counter 2 `shouldBe` "@. "
            Ctr.counterRenderMarker counter 4 `shouldBe` "!. "
        it "Handles system: fixed" $ do
            let counter = Ctr.defaultCounter {
                Ctr.system = Ctr.Fixed 1, Ctr.symbols = ["◰", "◳", "◲", "◱"],
                Ctr.suffix = ": "
              }
            Ctr.counterRenderMarker counter 1 `shouldBe` "◰: "
            Ctr.counterRenderMarker counter 2 `shouldBe` "◳: "
            Ctr.counterRenderMarker counter 3 `shouldBe` "◲: "
            Ctr.counterRenderMarker counter 4 `shouldBe` "◱: "
            Ctr.counterRenderMarker counter 5 `shouldBe` "5: "
            Ctr.counterRenderMarker counter 6 `shouldBe` "6: "
        it "Handles system: symbolic" $ do
            let counter = Ctr.defaultCounter {
                Ctr.system = Ctr.Symbolic, Ctr.symbols = ["*", "⁑", "†", "‡"],
                Ctr.suffix = " "
            }
            Ctr.counterRenderMarker counter 1 `shouldBe` "* "
            Ctr.counterRenderMarker counter 2 `shouldBe` "⁑ "
            Ctr.counterRenderMarker counter 3 `shouldBe` "† "
            Ctr.counterRenderMarker counter 4 `shouldBe` "‡ "
            Ctr.counterRenderMarker counter 5 `shouldBe` "** "
            Ctr.counterRenderMarker counter 6 `shouldBe` "⁑⁑ "
        it "Handles system: alphabetic" $ do
            let counter = Ctr.defaultCounter {
                Ctr.system = Ctr.Alphabetic, Ctr.symbols = ["◦", "•"],
                Ctr.suffix = " "
            }
            Ctr.counterRenderMarker counter 1 `shouldBe` "◦ "
            Ctr.counterRenderMarker counter 2 `shouldBe` "• "
            Ctr.counterRenderMarker counter 3 `shouldBe` "◦◦ "
            Ctr.counterRenderMarker counter 4 `shouldBe` "◦• "
            Ctr.counterRenderMarker counter 5 `shouldBe` "•◦ "
            Ctr.counterRenderMarker counter 6 `shouldBe` "•• "
            Ctr.counterRenderMarker counter 7 `shouldBe` "◦◦◦ "
        it "Handles system: numeric" $ do
            let counter = Ctr.defaultCounter {
                Ctr.system = Ctr.Numeric, Ctr.symbols = ["0", "1", "2"]
            }
            Ctr.counterRenderMarker counter 1 `shouldBe` "1. "
            Ctr.counterRenderMarker counter 2 `shouldBe` "2. "
            Ctr.counterRenderMarker counter 3 `shouldBe` "10. "
            Ctr.counterRenderMarker counter 4 `shouldBe` "11. "
            Ctr.counterRenderMarker counter 5 `shouldBe` "12. "
            Ctr.counterRenderMarker counter 6 `shouldBe` "20. "
        it "Handles system: additive" $ do
            let counter = Ctr.defaultCounter {
                Ctr.system = Ctr.Additive, Ctr.suffix = " ",
                Ctr.additiveSymbols = [(6, "⚅"), (5, "⚄"), (4, "⚃"),
                                        (3, "⚂"), (2, "⚁"), (1, "⚀")]
            }
            Ctr.counterRenderMarker counter 1 `shouldBe` "⚀ "
            Ctr.counterRenderMarker counter 2 `shouldBe` "⚁ "
            Ctr.counterRenderMarker counter 3 `shouldBe` "⚂ "
            Ctr.counterRenderMarker counter 11 `shouldBe` "⚅⚄ "
            Ctr.counterRenderMarker counter 12 `shouldBe` "⚅⚅ "
            Ctr.counterRenderMarker counter 13 `shouldBe` "⚅⚅⚀ "
        it "Handles Chinese-specific numbering systems" $ do
            let counterRender = Ctr.counterRender Ctr.simpChineseInformal
            counterRender 1 `shouldBe` "一"
            counterRender 2 `shouldBe` "二"
            counterRender 3 `shouldBe` "三"
            counterRender 4 `shouldBe` "四"
            counterRender 5 `shouldBe` "五"
            counterRender 6 `shouldBe` "六"
            counterRender 7 `shouldBe` "七"
            counterRender 8 `shouldBe` "八"
            counterRender 9 `shouldBe` "九"
            counterRender 10 `shouldBe` "十"
            counterRender 11 `shouldBe` "十一"
            counterRender 12 `shouldBe` "十二"
            counterRender 13 `shouldBe` "十三"
            counterRender 14 `shouldBe` "十四"
            counterRender 15 `shouldBe` "十五"
            counterRender 16 `shouldBe` "十六"
            counterRender 17 `shouldBe` "十七"
            counterRender 18 `shouldBe` "十八"
            counterRender 19 `shouldBe` "十九"
            counterRender 20 `shouldBe` "二十"
            counterRender 21 `shouldBe` "二十一"
            counterRender 22 `shouldBe` "二十二"
            counterRender 23 `shouldBe` "二十三"
            counterRender 24 `shouldBe` "二十四"
            counterRender 25 `shouldBe` "二十五"
            counterRender 26 `shouldBe` "二十六"
            counterRender 27 `shouldBe` "二十七"
            counterRender 28 `shouldBe` "二十八"
            counterRender 29 `shouldBe` "二十九"
            counterRender 30 `shouldBe` "三十"
            counterRender 31 `shouldBe` "三十一"
            counterRender 32 `shouldBe` "三十二"
            counterRender 33 `shouldBe` "三十三"
            counterRender 34 `shouldBe` "三十四"
            counterRender 35 `shouldBe` "三十五"
            counterRender 36 `shouldBe` "三十六"
            counterRender 37 `shouldBe` "三十七"
            counterRender 38 `shouldBe` "三十八"
            counterRender 39 `shouldBe` "三十九"
            counterRender 40 `shouldBe` "四十"
            counterRender 41 `shouldBe` "四十一"
            counterRender 42 `shouldBe` "四十二"
            counterRender 43 `shouldBe` "四十三"
            counterRender 44 `shouldBe` "四十四"
            counterRender 45 `shouldBe` "四十五"
            counterRender 46 `shouldBe` "四十六"
            counterRender 47 `shouldBe` "四十七"
            counterRender 48 `shouldBe` "四十八"
            counterRender 49 `shouldBe` "四十九"
            counterRender 50 `shouldBe` "五十"
            counterRender 51 `shouldBe` "五十一"
            counterRender 52 `shouldBe` "五十二"
            counterRender 53 `shouldBe` "五十三"
            counterRender 54 `shouldBe` "五十四"
            counterRender 55 `shouldBe` "五十五"
            counterRender 56 `shouldBe` "五十六"
            counterRender 57 `shouldBe` "五十七"
            counterRender 58 `shouldBe` "五十八"
            counterRender 59 `shouldBe` "五十九"
            counterRender 60 `shouldBe` "六十"
            counterRender 61 `shouldBe` "六十一"
            counterRender 62 `shouldBe` "六十二"
            counterRender 63 `shouldBe` "六十三"
            counterRender 64 `shouldBe` "六十四"
            counterRender 65 `shouldBe` "六十五"
            counterRender 66 `shouldBe` "六十六"
            counterRender 67 `shouldBe` "六十七"
            counterRender 68 `shouldBe` "六十八"
            counterRender 69 `shouldBe` "六十九"
            counterRender 70 `shouldBe` "七十"
            counterRender 71 `shouldBe` "七十一"
            counterRender 72 `shouldBe` "七十二"
            counterRender 73 `shouldBe` "七十三"
            counterRender 74 `shouldBe` "七十四"
            counterRender 75 `shouldBe` "七十五"
            counterRender 76 `shouldBe` "七十六"
            counterRender 77 `shouldBe` "七十七"
            counterRender 78 `shouldBe` "七十八"
            counterRender 79 `shouldBe` "七十九"
            counterRender 80 `shouldBe` "八十"
            counterRender 81 `shouldBe` "八十一"
            counterRender 82 `shouldBe` "八十二"
            counterRender 83 `shouldBe` "八十三"
            counterRender 84 `shouldBe` "八十四"
            counterRender 85 `shouldBe` "八十五"
            counterRender 86 `shouldBe` "八十六"
            counterRender 87 `shouldBe` "八十七"
            counterRender 88 `shouldBe` "八十八"
            counterRender 89 `shouldBe` "八十九"
            counterRender 90 `shouldBe` "九十"
            counterRender 91 `shouldBe` "九十一"
            counterRender 92 `shouldBe` "九十二"
            counterRender 93 `shouldBe` "九十三"
            counterRender 94 `shouldBe` "九十四"
            counterRender 95 `shouldBe` "九十五"
            counterRender 96 `shouldBe` "九十六"
            counterRender 97 `shouldBe` "九十七"
            counterRender 98 `shouldBe` "九十八"
            counterRender 99 `shouldBe` "九十九"
            counterRender 100 `shouldBe` "一百"
            counterRender 101 `shouldBe` "一百零一"
            counterRender 102 `shouldBe` "一百零二"
            counterRender 103 `shouldBe` "一百零三"
            counterRender 104 `shouldBe` "一百零四"
            counterRender 105 `shouldBe` "一百零五"
            counterRender 106 `shouldBe` "一百零六"
            counterRender 107 `shouldBe` "一百零七"
            counterRender 108 `shouldBe` "一百零八"
            counterRender 109 `shouldBe` "一百零九"
            counterRender 110 `shouldBe` "一百一十"
            counterRender 111 `shouldBe` "一百一十一"
            counterRender 112 `shouldBe` "一百一十二"
            counterRender 113 `shouldBe` "一百一十三"
            counterRender 114 `shouldBe` "一百一十四"
            counterRender 115 `shouldBe` "一百一十五"
            counterRender 116 `shouldBe` "一百一十六"
            counterRender 117 `shouldBe` "一百一十七"
            counterRender 118 `shouldBe` "一百一十八"
            counterRender 119 `shouldBe` "一百一十九"
            counterRender 120 `shouldBe` "一百二十"
        it "Handles ethiopian numbering system" $ do
            Ctr.counterRender Ctr.ethiopic 1 `shouldBe` "፩"
            Ctr.counterRender Ctr.ethiopic 100 `shouldBe` "፻"
            Ctr.counterRender Ctr.ethiopic 78010092 `shouldBe` "፸፰፻፩፼፺፪"
            Ctr.counterRender Ctr.ethiopic 780100000092 `shouldBe` "፸፰፻፩፼፼፺፪"

styleIndex :: StyleIndex
styleIndex = new
queryable :: QueryableStyleSheet (VarParser TrivialPropertyParser)
queryable = queryableStyleSheet
emptyStyle :: TrivialStyleSheet
emptyStyle = TrivialStyleSheet []
conditional :: ConditionalStyles TrivialPropertyParser
conditional = conditionalStyles (fromJust $ parseURI "about:blank") "test"
linkStyle :: TrivialStyleSheet
linkStyle = TrivialStyleSheet [sampleRule]
sampleRule :: StyleRule
sampleRule = StyleRule (Element [Tag "a"]) [("color", [Ident "green"])] ""
resolve' = resolve (\var -> B (var == "test")) evalToken emptyStyle
    where
        evalToken (Number _ (NVInteger x)) = N $ fromInteger x
        evalToken (Number _ (NVNumber x)) = N $ toRealFloat x
        evalToken _ = B False
extractImports' = extractImports (\var -> B (var == "test")) evalToken
    where
        evalToken (Number _ (NVInteger x)) = N $ fromInteger x
        evalToken (Number _ (NVNumber x)) = N $ toRealFloat x
        evalToken _ = B False
