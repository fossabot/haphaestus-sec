{-# LANGUAGE OverloadedStrings #-}
module Stylist(cssPriorityAgent, cssPriorityUser, cssPriorityAuthor,
    PropertyParser(..), TrivialPropertyParser(..),
    StyleSheet(..), TrivialStyleSheet(..), Props,
    Element(..), Attribute(..),
    elementPath, compileAttrTest, matched, attrTest, hasWord, hasLang,
    parseUnorderedShorthand, parseUnorderedShorthand', parseOperands) where

import Data.Text (Text, unpack)
import Data.CSS.Syntax.Tokens (Token(..))
import Data.List

import Stylist.Parse (StyleSheet(..), TrivialStyleSheet(..), scanBlock)
import Stylist.Parse.Selector

-- | Set the priority for a CSS stylesheet being parsed.
cssPriorityAgent, cssPriorityUser, cssPriorityAuthor :: StyleSheet s => s -> s
cssPriorityAgent = setPriority 1
cssPriorityUser = setPriority 2
cssPriorityAuthor = setPriority 3

-- | Defines how to parse CSS properties into an output "style" format.
class PropertyParser a where
    -- | Default styles.
    temp :: a
    -- | Creates a style inherited from a parent style.
    inherit :: a -> a
    inherit = id

    priority :: a -> [Text]
    priority _ = []

    -- | Expand a shorthand property into longhand properties.
    shorthand :: a -> Text -> [Token] -> [(Text, [Token])]
    shorthand self key value | Just _ <- longhand self self key value = [(key, value)]
        | otherwise = []
    -- | Mutates self to store the given CSS property, if it's syntax is valid.
    -- longhand parent self name value
    longhand :: a -> a -> Text -> [Token] -> Maybe a

    -- | Retrieve stored variables, optional.
    getVars :: a -> Props
    getVars _ = []
    -- | Save variable values, optional.
    setVars :: Props -> a -> a
    setVars _ = id

    -- | Mutates self to store the given pseudoelement styles,
    -- passing a callback so you can alter the parent &
    -- (for interactive pseudoclasses) base styles.
    pseudoEl :: a -> Text -> (a -> Maybe a -> a) -> a
    pseudoEl self _ _ = self

-- | "key: value;" entries to be parsed into an output type.
type Props = [(Text, [Token])]

-- | Gathers properties as a key'd list.
-- Works well with `lookup`.
data TrivialPropertyParser = TrivialPropertyParser [(String, [Token])] deriving (Show, Eq)
instance PropertyParser TrivialPropertyParser where
    temp = TrivialPropertyParser []
    longhand _ (TrivialPropertyParser self) key value =
        Just $ TrivialPropertyParser ((unpack key, value):self)

-- | An inversely-linked tree of elements, to apply CSS selectors to.
data Element = ElementNode {
    -- | The element's parent in the tree.
    parent :: Maybe Element,
    -- | The element's previous sibling in the tree.
    previous :: Maybe Element,
    -- | The element's name.
    name :: Text,
    -- | The element's namespace.
    namespace :: Text,
    -- | The element's attributes, in sorted order.
    attributes :: [Attribute]
}
-- | A key-value attribute.
data Attribute = Attribute Text Text String deriving (Eq, Ord)

-- | Computes the child indices to traverse to reach the given element.
elementPath :: Element -> [Int]
elementPath = elementPath' []
-- | Variant of `elementPath` with a prefix path.
elementPath' path ElementNode { parent = Just parent', previous = prev } =
    elementPath' (succ (countSib prev) : path) parent'
elementPath' path ElementNode { parent = Nothing, previous = prev } =
    (succ (countSib prev) : path)
-- | How many previous children does this element have?
countSib (Just (ElementNode { previous = prev })) = succ $ countSib prev
countSib Nothing = 0

-- | Converts a property text into a callback testing against a string.
compileAttrTest :: PropertyTest -> String -> Bool
compileAttrTest Exists = matched
compileAttrTest (Equals val) = (== (unpack val))
compileAttrTest (Suffix val) = isSuffixOf $ unpack val
compileAttrTest (Prefix val) = isPrefixOf $ unpack val
compileAttrTest (Substring val) = isInfixOf $ unpack val
compileAttrTest (Include val) = hasWord $ unpack val
compileAttrTest (Dash val) = hasLang $ unpack val
compileAttrTest (Callback (PropertyFunc cb)) = cb

-- | returns True regardless of value.
matched :: t -> Bool
matched _ = True
-- | Tests the given word is in the whitespace-seperated value.
hasWord :: String -> String -> Bool
hasWord expected value = expected `elem` words value
-- | Tests whether the attribute holds the expected value or a sub-locale.
hasLang :: [Char] -> [Char] -> Bool
hasLang expected value = expected == value || isPrefixOf (expected ++ "-") value

-- | Test whether the element matches a parsed property test, for the given attribute.
attrTest :: Maybe Text -> Text -> PropertyTest -> Element -> Bool
attrTest namespace name test ElementNode { attributes = attrs } = any predicate attrs
    where
        predicate attr@(Attribute ns' _ _) | Just ns <- namespace = ns == ns' && predicate' attr
            | otherwise = predicate' attr
        predicate' (Attribute _ name' value') = name == name' && compileAttrTest test value'

-- | Utility for parsing shorthand attributes which don't care in which order the
-- subproperties are specified.
-- Each property must parse only a single function or token.
parseUnorderedShorthand :: PropertyParser a =>
        a -> [Text] -> [Token] -> [(Text, [Token])]
parseUnorderedShorthand self properties toks
    | Just _ <- lookup "" ret = [] -- Error recovery!
    | otherwise = ret
  where
    ret = parseUnorderedShorthand' self properties $ parseOperands toks
-- | Variant of `parseUnorderedShorthand` taking pre-split list.
parseUnorderedShorthand' :: PropertyParser a =>
        a -> [Text] -> [[Token]] -> [(Text, [Token])]
parseUnorderedShorthand' self properties (arg:args) = inner properties []
  where
    inner (prop:props) props'
        | entry@(_:_) <- shorthand self prop arg =
            entry ++ parseUnorderedShorthand' self (props' ++ props) args
        | otherwise = inner props (prop:props')
    inner [] _ = [("", [])] -- Error caught & handled by public API.
parseUnorderedShorthand' self (prop:props) [] = -- Shorthands have long effects!
    (prop, [Ident "initial"]):parseUnorderedShorthand' self props []
parseUnorderedShorthand' _ [] [] = []

-- | Splits a token list so each function is it's own list.
-- Other tokens are split into their own singletons.
parseOperands :: [Token] -> [[Token]]
parseOperands (Function name:toks) = let (args, toks') = scanBlock toks
    in (Function name:args):parseOperands toks'
parseOperands (tok:toks) = [tok]:parseOperands toks
parseOperands [] = []
