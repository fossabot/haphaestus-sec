{-# LANGUAGE OverloadedStrings #-}
module SSML(styleToSSML, postorder) where

import Text.XML
import qualified Data.Text as Txt
import Data.Map
import Data.Maybe (isNothing, fromMaybe, fromJust)
import qualified Data.Map as M
import Data.CSS.StyleTree

import Data.CSS.Syntax.Tokens
import Data.Scientific (toRealFloat)
import Data.List (elemIndex)

import SpeechStyle

styleToSSML :: StyleTree SpeechStyle -> Element
styleToSSML = Element "{http://www.w3.org/2001/10/synthesis}speak" M.empty .
    collapseBreaks' . floatBreaks' . styleToNodes
styleToNodes :: StyleTree SpeechStyle -> [Node]
styleToNodes = Prelude.map style . postorder styleToSSML'

styleToSSML' SpeechStyle { speak = False } _ = []
styleToSSML' self@SpeechStyle {content = ""} children = el "prosody" [
        ("xml:lang", lang self),
        ("rhapsode:pseudo", pseudoEl' self),
        ("volume", volume self),
        ("rate", rate self),
        ("pitch", pitch2txt <$> pitch self),
        ("range", pitch2txt <$> range self)
    ] $ el "prosody" [
        ("volume", unit2txt <$> volumeAdjust self),
        ("rate", unit2txt <$> rateAdjust self),
        ("pitch", unit2txt <$> (pitchAdjust =<< pitch self)),
        ("range", unit2txt <$> (pitchAdjust =<< range self))
    ] $ el "emphasis" [("level", stress self)] $
    el "say-as" [("interpret-as", speakAs self)] $
    el "tts:style" [
        ("field", (\_ -> "punctuation") <$> speakAs self),
        ("mode", (\b -> if b then "all" else "none") <$> punctuation self)
    ] $ buildVoices (reverse $ voices self) $
    buildBox self children
styleToSSML' style childs = styleToSSML' style {content = ""} (
        pseudo "before" ++ [NodeContent $ content style] ++ pseudo "after")
    where
        pseudo n = [child | child@(NodeElement (Element _ attrs _)) <- childs,
            M.lookup "rhapsode:pseudo" attrs == Just n]

buildVoices (Voice name:voices) children =
    el "voice" [("name", Just name)] $ buildVoices voices children
buildVoices (VoicePattern age gender variant:voices) children = el "voice" [
        ("age", Txt.pack <$> show <$> age),
        ("gender", Just gender),
        ("variant", Txt.pack <$> show <$> variant)
    ] $ buildVoices voices children
buildVoices [] children = children

buildBox self childs = concat [
        el "mark" [("name", marker self)] [],
        breakEl $ pauseBefore self,
        audioEl $ cueBefore self,
        breakEl $ restBefore self,
        childs,
        breakEl $ restAfter self,
        audioEl $ cueAfter self,
        breakEl $ pauseAfter self
    ]
breakEl self = el "break" [("strength", strength self), ("time", unit2txt <$> time self)] []
audioEl NoCue = []
audioEl self = el "prosody" [("volume", unit2txt <$> cueVolume self)] $
    el "audio" [("src", Just $ src self)] []

-- support
el :: Name -> [(Name, Maybe Txt.Text)] -> [Node] -> [Node]
el n attrs children | all (isNothing . snd) attrs = children
    | otherwise = [NodeElement $ Element {
        elementName = n,
        elementAttributes = M.fromList [(k, v) | (k, Just v) <- attrs],
        elementNodes = children
    }]

relativeUnit n | n < 0 = Txt.pack (show n)
    | otherwise = Txt.pack ('+':show n)
unit2txt (Unit' unit n) = relativeUnit n `Txt.append` unit

pitch2txt (Pitch kw _) = kw
pitch2txt (Absolute (Unit' unit n)) = Txt.pack (show n) `Txt.append` unit
pitch2txt (Relative n) = unit2txt n

-- <break> collapse
floatBreaks :: Element -> [Node]
floatBreaks el@(Element _ _ childs)
    | break@(NodeElement (Element "break" _ _)):nodes <- floatBreaks' childs =
        break : floatBreaks el{elementNodes = nodes}
    | break@(NodeElement (Element "break" _ _)):nodes <- reverse $ floatBreaks' childs =
        floatBreaks el{elementNodes = reverse nodes} ++ [break]
    | otherwise = [NodeElement el]
floatBreaks' (NodeElement m@(Element "mark" _ _):NodeElement b@(Element "break" _ _):nodes) =
    NodeElement b : floatBreaks' (NodeElement m:nodes)
floatBreaks' (NodeElement el:nodes) = floatBreaks el ++ floatBreaks' nodes
floatBreaks' (node:nodes) = node : floatBreaks' nodes
floatBreaks' [] = []

collapseBreaks :: Element -> Element
collapseBreaks (Element name attrs elChildren) =
    Element name attrs $ collapseBreaks' elChildren
collapseBreaks' :: [Node] -> [Node]
collapseBreaks' (
        NodeElement a@(Element "break" _ _):
        NodeElement b@(Element "break" _ _):
        nodes
    ) = NodeElement (maxBreak a b) : collapseBreaks' nodes
collapseBreaks' (NodeElement el:nodes) = NodeElement (collapseBreaks el) : collapseBreaks' nodes
collapseBreaks' (node:nodes) = node:collapseBreaks' nodes
collapseBreaks' [] = []

volumes = ["x-weak", "weak", "medium", "strong", "x-strong"]
maxBreak x@(Element _ a _) y@(Element _ b _)
    | Just a' <- "strength" `M.lookup` a, Just b' <- "strength" `M.lookup` b, a' /= b' =
        if fromJust (elemIndex a' volumes) > fromJust (elemIndex b' volumes) then x else y
    | Just a' <- "time" `M.lookup` a, Just b' <- "time" `M.lookup` b, a' /= b' =
        if toMS (tokenize a') > toMS (tokenize b') then x else y
    | otherwise = x
toMS [Dimension _ n "s"] = cssFloat n * 1000
toMS [Dimension _ n "ms"] = cssFloat n
toMS _ = 0 -- Should never happen.
cssFloat (NVInteger i) = fromInteger i
cssFloat (NVNumber n) = toRealFloat n
