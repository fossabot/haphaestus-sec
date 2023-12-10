{-# LANGUAGE OverloadedStrings #-}
module SpeechStyle(SpeechStyle(..),
    Unit'(..), Pitch(..), pitchAdjust, Voice(..), Pause(..), Cue(..)) where

import Data.CSS.Syntax.Tokens
import Data.CSS.Style
import qualified Data.Text as Txt
import Data.Text (Text, unpack, pack, append)
import Data.Scientific (toRealFloat)
import Data.Maybe (isJust, catMaybes, fromMaybe)

import Text.Read (readMaybe) -- to parse <progress> into a more international textual representation.
import Network.MIME.Info as MIME

data Unit' = Unit' Text Float
data SpeechStyle = SpeechStyle {
    volume :: Maybe Text,
    volumeAdjust :: Maybe Unit',
    rate :: Maybe Text,
    rateAdjust :: Maybe Unit',
    pitch :: Maybe Pitch,
    range :: Maybe Pitch,

    speak :: Bool,
    speakAs :: Maybe Text,
    punctuation :: Maybe Bool,

    voices :: [Voice],
    stress :: Maybe Text,

    pauseBefore :: Pause,
    pauseAfter :: Pause,
    restBefore :: Pause,
    restAfter :: Pause,

    cueBefore :: Cue,
    cueAfter :: Cue,
    marker :: Maybe Text,

    content :: Text,
    lang :: Maybe Text,
    pseudoEl' :: Maybe Text
}

volumes = Txt.words "silent x-soft soft medium loud x-loud"
stresses = Txt.words "strong moderate none reduced"

instance PropertyParser SpeechStyle where
    temp = SpeechStyle {
        volume = Nothing,
        volumeAdjust = Nothing,
        rate = Nothing,
        rateAdjust = Nothing,
        pitch = Nothing,
        range = Nothing,

        speak = True,
        speakAs = Nothing,
        punctuation = Nothing,

        voices = [],
        stress = Nothing,

        pauseBefore = Pause Nothing Nothing,
        pauseAfter = Pause Nothing Nothing,
        restBefore = Pause Nothing Nothing,
        restAfter = Pause Nothing Nothing,
        cueBefore = NoCue,
        cueAfter = NoCue,
        marker = Nothing,

        content = "",
        lang = Nothing,
        pseudoEl' = Nothing
    }
    inherit _ = temp -- Text synthesizers handle inheritance.

    longhand _ self "voice-volume" [Ident "initial"] = Just self {volume = Just "medium"}
    longhand _ self "voice-volume" [Ident kw, Dimension _ n "dB"]
        | kw `elem` Prelude.tail volumes = Just self {volume = Just kw, volumeAdjust = Just $ cssUnit n "dB"}
    longhand _ self "voice-volume" [Ident kw] | kw `elem` volumes = Just self {volume = Just kw}

    longhand _ self "voice-rate" [kw, Percentage _ n] =
        longhand self self { rateAdjust = Just $ cssUnit n "%" } "voice-rate" [kw]
    longhand _ self "voice-rate" [Ident kw]
        | kw `elem` ["x-slow", "slow", "medium", "fast", "x-fast"] = Just self {rate = Just kw}
        | kw `elem` ["initial", "normal"] = Just self {rate = Just "default"}

    longhand _ self "voice-pitch" toks = (\v -> self { pitch = Just v }) <$> parsePitch toks
    longhand _ self "voice-range" toks = (\v -> self { range = Just v }) <$> parsePitch toks

    longhand _ self "speak" [Ident "never"] = Just self { speak = False }
    longhand _ self "speak" [Ident kw] | kw `elem` ["always", "initial"] = Just self { speak = True }

    longhand _ self "speak-as" [Ident kw] | kw `elem` ["normal", "initial"] = Just self { speakAs = Nothing }
    longhand _ self "speak-as" [Ident "spell-out"] = Just self { speakAs = Just "characters" }
    longhand _ self "speak-as" [Ident "digits"] = Just self { speakAs = Just "tts:digits" }
    longhand _ self "speak-as" [Ident "literal-punctuation"] = Just self { speakAs = Nothing, punctuation = Just True }
    longhand _ self "speak-as" [Ident "no-punctuation"] = Just self { speakAs = Nothing, punctuation = Just False }
    longhand _ self "speak-as" [tok, Ident kw] | kw `elem` ["literal-punctuation", "no-punctuation"] =
        longhand self self { punctuation = Just (kw == "literal-punctuation") } "speak-as" [tok]

    longhand _ self "voice-family" [Ident preserve] = Just self
    longhand _ self "voice-family" toks | Prelude.all isJust val = Just self { voices = catMaybes val }
        where val = Prelude.map parseVoice $ split' Comma toks

    longhand _ self "voice-stress" [Ident kw] | kw `elem` stresses = Just self { stress = Just kw }

    longhand _ self "pause-before" toks = (\v -> self { pauseBefore = v }) <$> parsePause toks
    longhand _ self "pause-after" toks = (\v -> self { pauseBefore = v }) <$> parsePause toks
    longhand _ self "rest-before" toks = (\v -> self { restBefore = v }) <$> parsePause toks
    longhand _ self "rest-after" toks = (\v -> self { restAfter = v }) <$> parsePause toks

    longhand _ self "cue-before" toks = (\v -> self { cueBefore = v }) <$> parseCue toks
    longhand _ self "cue-after" toks = (\v -> self { cueAfter = v }) <$> parseCue toks
    longhand _ self "-rhaps-marker" toks = (\v -> self { marker = Just v }) <$> parseStrings toks

    longhand _ self "content" [Ident "initial"] = Just self {content = ""}
    longhand _ self "content" toks = (\v -> self {content = v}) <$> parseStrings toks
    longhand _ self "::" [Ident pseudo] = Just self {pseudoEl' = Just pseudo} -- To make sure content doesn't override :before & :after
    longhand _ self "-rhaps-lang" [String v] = Just self {lang = Just v}
    longhand _ _ _ _ = Nothing

    shorthand _ "pause" [a, b] | isPause [a], isPause [b] = [("pause-before", [a]), ("pause-after", [b])]
    shorthand _ "pause" toks | isPause toks = [("pause-before", toks), ("pause-after", toks)]
    shorthand _ "rest" [a, b] | isPause [a], isPause [b] = [("rest-before", [a]), ("rest-after", [b])]
    shorthand _ "rest" toks | isPause toks = [("rest-before", toks), ("rest-after", toks)]

    shorthand _ "cue" (a:b:c) | isCue [a, b], isCue c = [("cue-before", [a, b]), ("cue-after", c)]
    shorthand _ "cue" (a:b) | isCue [a], isCue b = [("cue-before", [a]), ("cue-after", b)]
    shorthand _ "cue" toks | isCue toks = [("cue-before", toks), ("cue-after", toks)]

    shorthand _ "-rhaps-marker" [Ident m] = [
            ("-rhaps-marker", [String m, Function "counter", Ident m, RightParen]),
            ("counter-increment", [Ident m])
        ]

    shorthand self key value | isJust $ longhand self self key value = [(key, value)]
        | otherwise = []

-- parsers
data Pitch = Pitch Text (Maybe Unit') | Absolute Unit' | Relative Unit'
pitchAdjust (Pitch _ adjust) = adjust
pitchAdjust _ = Nothing

pitches = ["x-low", "low", "medium", "high", "x-high"]
parsePitch [Ident "initial"] = Just $ Pitch "medium" Nothing
parsePitch [kw, Percentage n' n] = parsePitch [kw, Dimension n' n "%"]
parsePitch [Ident kw, Dimension _ n unit]
    | kw `elem` pitches && unit `elem` ["hz", "khz", "st", "%"] =
        Just $ Pitch kw $ Just $ cssUnit n unit
parsePitch [Ident kw] | kw `elem` pitches = Just $ Pitch kw Nothing
parsePitch [Dimension _ n unit, Ident "absolute"]
    | unit `elem` ["hz", "khz"], cssFloat n > 0 = Just $ Absolute $ cssUnit n unit
parsePitch [Ident "absolute", Dimension _ n unit]
    | unit `elem` ["hz", "khz"], cssFloat n > 0 = Just $ Absolute $ cssUnit n unit
parsePitch [Dimension _ n unit]
    | unit `elem` ["hz", "khz"] = Just $ Relative $ cssUnit n unit
parsePitch _ = Nothing

data Voice = Voice Text | VoicePattern (Maybe Integer) Text (Maybe Integer)
genders = ["male", "female", "neutral"]
parseVoice (Comma:toks) = parseVoice toks
parseVoice (Ident "child":toks) = parseVoice (Number "6" (NVInteger 6):toks)
parseVoice (Ident "young":toks) = parseVoice (Number "24" (NVInteger 24):toks)
parseVoice (Ident "old":toks) = parseVoice (Number "75" (NVInteger 75):toks)
parseVoice [Ident kw, Number _ (NVInteger v)]
    | v >= 1 && kw `elem` genders = Just $ VoicePattern Nothing kw $ Just v
parseVoice [Ident kw] | kw `elem` genders = Just $ VoicePattern Nothing kw Nothing
    | otherwise = Just $ Voice kw
parseVoice [Number _ (NVInteger age), Ident kw, Number _ (NVInteger v)]
    | age >= 0 && kw `elem` genders && v >= 1 = Just $ VoicePattern (Just age) kw (Just v)
parseVoice [Number _ (NVInteger age), Ident kw]
    | age >= 0 && kw `elem` genders = Just $ VoicePattern (Just age) kw Nothing
parseVoice _ = Nothing

data Pause = Pause { strength :: Maybe Text, time :: Maybe Unit' }
pauses = Txt.words "x-weak weak medium strong x-strong"
parsePause [Ident "none"] = Nothing
parsePause [Ident kw] | kw `elem` pauses = Just Pause { strength = Just kw, time = Nothing }
parsePause [Dimension _ n unit] | unit `elem` ["s", "ms"] =
    Just $ Pause Nothing $ Just $ cssUnit n unit
parsePause _ = Nothing
isPause = isJust . parsePause

data Cue = Cue {src :: Text, cueVolume :: Maybe Unit'} | NoCue
parseCue [Url source] = Just $ Cue source Nothing
parseCue [Url source, Dimension _ n "dB"] = Just $ Cue source $ Just $ cssUnit n "dB"
parseCue [Ident "none"] = Just NoCue
parseCue _ = Nothing
isCue = isJust . parseCue

parseStrings (String txt:toks) = append txt <$> parseStrings toks
parseStrings (Function "-rhaps-percentage":String num:String denom:RightParen:toks) =
    append (pack $ show frac) <$> parseStrings toks
  where
    frac :: Int
    frac = round (readNum num / readNum denom * 100)
    readNum :: Text -> Float
    readNum = fromMaybe (0.0) . readMaybe . unpack
parseStrings (Function "-rhaps-filetype":String mime:RightParen:toks) =
    append (pack $ MIME.name $ mimeInfo $ unpack mime) <$> parseStrings toks
parseStrings [] = Just ""
parseStrings _ = Nothing

-- ParsingUtils
cssUnit n "khz" = Unit' "hz" (cssFloat n*1000)
cssUnit n unit = Unit' unit $ cssFloat n
cssFloat :: NumericValue -> Float
cssFloat (NVInteger i) = fromInteger i
cssFloat (NVNumber n) = toRealFloat n

split' :: Eq a => a -> [a] -> [[a]]
split' _ [] = []
split' sep list = h:split' sep t where (h,t) = Prelude.break (==sep) list
