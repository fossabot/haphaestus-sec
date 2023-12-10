{-# LANGUAGE OverloadedStrings #-}
-- | Lowers certain CSS properties to plain text.
module Data.CSS.Preprocessor.Text(
    TextStyle, resolve, resolveWithCounterStyles, CounterStore'(..)) where

import Data.CSS.Syntax.Tokens (Token(..), NumericValue(..))
import Stylist (parseUnorderedShorthand, PropertyParser(..))
import Stylist.Parse (scanBlock)
import Data.CSS.StyleTree
import qualified Data.Text as Txt
import Data.Text (Text)
import Data.CSS.Preprocessor.Text.CounterStyle
        (parseCounter, counterRender, CounterStore'(..), decimalCounter,
        defaultCounterStore, CounterStore, CounterStyle(..), speakAs')

import Data.Maybe (fromMaybe, isJust)
import qualified Data.HashMap.Lazy as M
import Data.Function ((&))

import Data.Char (isSpace)

type Counters = [(Text, Int)]
-- | `PropertyParser` decorator that parses & lowers certain CSS properties to plain text.
data TextStyle p = TextStyle {
    inner :: p,
    counterProps :: [(Text, [Token])],

    counterReset :: Counters,
    counterIncrement :: Counters,
    counterSet :: Counters,

    whiteSpaceCollapse :: Bool,
    newlineCollapse :: Bool,

    isListItem :: Bool,
    listStyleImage :: [Token],
    listStyleType :: [Token],
    listPosInside :: Bool,
    markerIsRight :: Maybe Bool,
    isRTL :: Bool,

    beforePseudo :: Maybe (TextStyle p),
    afterPseudo  :: Maybe (TextStyle p),
    markerPseudo :: Maybe (TextStyle p)
}

instance PropertyParser p => PropertyParser (TextStyle p) where
    temp = TextStyle {
            inner = temp,
            counterProps = [],
            counterReset = [],
            counterIncrement = [],
            counterSet = [],
            whiteSpaceCollapse = True,
            newlineCollapse = True,
            isListItem = False,
            listStyleImage = [],
            listStyleType = [Ident "disc"],
            listPosInside = False,
            markerIsRight = Nothing,
            isRTL = False,
            beforePseudo = Nothing,
            afterPseudo  = Nothing,
            markerPseudo = Nothing
        }
    inherit parent = TextStyle {
            inner = inherit $ inner parent,
            counterProps = [],
            counterReset = [],
            counterIncrement = [],
            counterSet = [],
            whiteSpaceCollapse = whiteSpaceCollapse parent,
            newlineCollapse = newlineCollapse parent,
            isListItem = False,
            listStyleImage = listStyleImage parent,
            listStyleType = listStyleType parent,
            listPosInside = listPosInside parent,
            markerIsRight = if isJust $ markerIsRight parent
                then Just $ isRTL parent else Nothing,
            isRTL = isRTL parent,
            beforePseudo = Nothing,
            afterPseudo  = Nothing,
            markerPseudo = Nothing
        }
    priority self = priority $ inner self

    shorthand _ key value
        | key `elem` ["counter-reset", "counter-increment", "counter-set"],
            Just _ <- parseCounters 0 value = [(key, value)]
    shorthand self "white-space" [Ident val]
        | val `elem` ["normal", "pre", "pre-wrap", "pre-line"] = [("white-space", [Ident val])]
        | otherwise = shorthand (inner self) "white-space" [Ident val]
    shorthand self "list-style" toks = parseUnorderedShorthand self subprops toks
      where subprops = ["list-style-image", "list-style-type", "list-style-position"]
    shorthand TextStyle { inner = s } k v
        | Just _ <- longhand s s k $ removeCounters v = [(k, v)]
        | otherwise = shorthand s k v

    longhand _ self "counter-reset" value = (\v -> self {counterReset = v}) <$> parseCounters 0 value
    longhand _ self "counter-increment" value = (\v -> self {counterIncrement = v}) <$> parseCounters 1 value
    longhand _ self "counter-set" value = (\v -> self {counterSet = v}) <$> parseCounters 0 value

    longhand p self "white-space" [Ident "initial"] = setWhiteSpace p self True True "normal"
    longhand p self "white-space" [Ident "normal"] = setWhiteSpace p self True True "normal"
    longhand p self "white-space" [Ident "pre"] = setWhiteSpace p self False False "nowrap"
    longhand p self "white-space" [Ident "nowrap"] = setWhiteSpace p self True True "nowrap"
    longhand p self "white-space" [Ident "pre-wrap"] = setWhiteSpace p self False False "normal"
    longhand p self "white-space" [Ident "pre-line"] = setWhiteSpace p self True False "normal"

    longhand p self@TextStyle {inner=self'} "display" [Ident "list-item"] =
        Just self {
            isListItem = True,
            inner = fromMaybe self' $
                longhand (inner p) self' "display" [Ident "block"]
        }
    longhand TextStyle {inner=p'} self@TextStyle {inner = self'} "display" value
        | Just ret <- longhand p' self' "display" value = Just self {
            isListItem = False,
            inner = ret
          }
        | otherwise = Nothing

    longhand _ self "list-style-image" [Ident kw] | kw `elem` ["initial", "none"]
        = Just self { listStyleImage = [] }
    longhand TextStyle { inner = p' } self@TextStyle { inner = self' }
            "list-style-image" value
        | Just _ <- longhand p' self' "background-image" value = Just self {
            listStyleImage = value -- This is a valid image according to caller.
          }
        | otherwise = Nothing
    longhand _ self "list-style-type" [Ident "initial"] =
        Just self { listStyleType = [Ident "disc"] }
    longhand _ self "list-style-type" toks | Just _ <- parseCounter M.empty toks =
        Just self { listStyleType = toks }
    longhand _ self "list-style-position" [Ident "inside"] =
        Just self { listPosInside = True }
    longhand _ self "list-style-position" [Ident "outside"] =
        Just self { listPosInside = False }
    longhand _ self "list-style-position" [Ident "initial"] =
        Just self { listPosInside = False }

    longhand _ self "marker-side" [Ident "match-self"] =
        Just self { markerIsRight = Nothing }
    longhand parent self "marker-side" [Ident "match-parent"] =
        Just self { markerIsRight = Just $ isRTL parent }
    longhand _ self "marker-side" [Ident "initial"] =
        Just self { markerIsRight = Nothing }
    longhand _ self k@"direction" v@[Ident "ltr"] = Just self {
        isRTL = False, counterProps = insertList k v $ counterProps self }
    longhand _ self k@"direction" v@[Ident "rtl"] = Just self {
        isRTL = True, counterProps = insertList k v $ counterProps self }
    longhand _ self k@"direction" v@[Ident "initial"] = Just self {
        isRTL = False, counterProps = insertList k v $ counterProps self }

    -- Capture `content` properties & anything else using counter(s) functions.
    -- This is important in Rhapsode for the sake of navigational markers.
    -- Ignoring invalid properties.
    longhand TextStyle { inner = p' } TextStyle { inner = self' } key value
        | Nothing <- longhand p' self' key $ removeCounters value = Nothing
    longhand _ self "content" [Ident "normal"] =
        Just self {
            counterProps =
                [(k, val) | (k, val) <- counterProps self, k /= "content"]
          }
    longhand parent self key value
        | key == "content" || Function "counter" `elem` value || Function "counters" `elem` value =
            Just $ self { counterProps = insertList key value $ counterProps self }
        | otherwise = (\v -> self {inner = v}) <$>
            longhand (inner parent ) (inner self) key value

    pseudoEl self "before" calc = self { beforePseudo = Just $ calc self Nothing }
    pseudoEl self "after" calc  = self { afterPseudo  = Just $ calc self Nothing }
    pseudoEl self "marker" calc = self { markerPseudo = Just $ calc self Nothing }
    pseudoEl self sel calc = self {
        inner = pseudoEl (inner self) sel calc'
      } where
        calc' parent (Just base) =
            inner $ calc temp { inner = parent } $ Just temp { inner = base }
        calc' parent Nothing = inner $ calc temp { inner = parent } Nothing

insertList :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
insertList key value list | Nothing <- lookup key list = (key, value) : list
    | otherwise = [(k, if k == key then value else v) | (k, v) <- list]

removeCounters :: [Token] -> [Token]
removeCounters (Function "counter":Ident _:RightParen:toks) = String "" : removeCounters toks
removeCounters (Function "counter":Ident _:Comma:toks)
    | Just (_, RightParen:toks') <- parseCounter0 toks = String "" : removeCounters toks'
removeCounters (Function "counters":Ident _:Comma:String _:RightParen:toks) =
    String "" : removeCounters toks
removeCounters (Function "counters":Ident _:Comma:String _:Comma:toks)
    | Just (_, RightParen:toks') <- parseCounter0 toks = String "" : removeCounters toks'
removeCounters (tok:toks) = tok : removeCounters toks
removeCounters [] = []

parseCounter0 :: [Token] -> Maybe (CounterStyle, [Token])
parseCounter0 = parseCounter M.empty

setWhiteSpace :: PropertyParser p => TextStyle p -> TextStyle p -> Bool -> Bool -> Text -> Maybe (TextStyle p)
setWhiteSpace parent self collapse noNewlines lowered = Just $ self {
        inner = inner self `fromMaybe` longhand (inner parent) (inner self) "white-space" [Ident lowered],
        whiteSpaceCollapse = collapse,
        newlineCollapse = noNewlines
    }
parseCounters :: Int -> [Token] -> Maybe [(Text, Int)]
parseCounters _ [Ident "none"] = Just []
parseCounters _ [Ident "initial"] = Just []
parseCounters _ [] = Just []
parseCounters x (Ident counter : Number _ (NVInteger count') : toks) =
    (:) (counter, fromIntegral count') <$> parseCounters x toks
parseCounters x (Ident counter : toks) = (:) (counter, x) <$> parseCounters x toks
parseCounters _ _ = Nothing

-- | Returns inner `PropertyParser` with text properties applied.
resolve :: PropertyParser p => StyleTree (TextStyle p) -> StyleTree p
resolve = resolveWithCounterStyles defaultCounterStore
resolveWithCounterStyles :: PropertyParser p =>
    CounterStore' -> StyleTree (TextStyle p) -> StyleTree p
resolveWithCounterStyles (CounterStore counters) =
    resolve' . collapseWS . applyCounters counters . insertPseudos counters
resolve' :: PropertyParser p => StyleTree (TextStyle p) -> StyleTree p
resolve' = treeMap $ \TextStyle {inner = inner', counterProps = props} -> foldl resolveProp inner' props
resolveProp :: PropertyParser p => p -> (Text, [Token]) -> p
resolveProp sty (key, value) = sty `fromMaybe` longhand temp sty key value

--------
---- Lists & pseudo-elements
--------

insertPseudos :: PropertyParser p =>
    CounterStore -> StyleTree (TextStyle p) -> StyleTree (TextStyle p)
insertPseudos s (StyleTree self@TextStyle { afterPseudo = Just child } childs) =
    insertPseudos s $
        StyleTree self { afterPseudo = Nothing } (childs ++ [t Nothing child])
insertPseudos s (StyleTree self@TextStyle { beforePseudo = Just child } childs) =
    insertPseudos s $ StyleTree self { beforePseudo = Nothing }
        (t Nothing child:childs)

insertPseudos s (StyleTree self@TextStyle { markerPseudo = Nothing } childs) =
    insertPseudos s $ StyleTree self { markerPseudo = Just temp } childs
insertPseudos s self@(StyleTree TextStyle { markerPseudo = Just child} _)
    | Just text <- lookup "content" $ counterProps child =
        addBullet self s Nothing text
insertPseudos s self@(StyleTree TextStyle { listStyleImage = bullet@(_:_) } _) =
    addBullet self s Nothing (bullet ++ [String " "])
insertPseudos s self@(StyleTree TextStyle { listStyleType = bullet@(_:_) } _)
    | Just (cstyle, _) <- parseCounter s bullet =
        addBullet self s (Just cstyle) $ text cstyle
  where
    text counter = String (prefix counter):
        Function "counter":Ident "list-item":Comma:bullet ++
        [String $ suffix counter]

insertPseudos store (StyleTree self childs) =
    StyleTree self $ map (insertPseudos store) childs

addBullet :: PropertyParser p => StyleTree (TextStyle p) ->
    CounterStore -> Maybe CounterStyle -> [Token] -> StyleTree (TextStyle p)
addBullet (StyleTree self@TextStyle {
        counterIncrement = counters, isListItem = True
    } childs) store cstyle txt | Nothing <- lookup "list-item" counters =
        addBullet (StyleTree self {
            counterIncrement = ("list-item", 1):counterIncrement self
        } childs) store cstyle txt
addBullet (StyleTree self@TextStyle {
        isListItem = True, listPosInside = True, markerPseudo = Just child
    } childs) store cstyle txt = insertPseudos store $
        StyleTree self { isListItem = False } (t cstyle child {
            counterProps = insertList "content" txt $ counterProps child
        } : childs)
addBullet (StyleTree s@TextStyle {markerIsRight=Nothing} childs) store cstyle txt
    = addBullet (StyleTree s { markerIsRight = Just $ isRTL s } childs)
        store cstyle txt
addBullet (StyleTree self@TextStyle { markerIsRight = Just False,
        isListItem = True, listPosInside = False, markerPseudo = Just child
    } childs) store cstyle txt = insertPseudos store $
        StyleTree self {
            isListItem = False,
            -- Flex lays out children horizontally at min size.
            counterProps=insertList "display" [Ident "flex"] $ counterProps child
        } [
            t cstyle child {
                counterProps = insertList "content" txt $ counterProps child
            },
            -- Generate a new layout box for the bullet to sit outside of.
            StyleTree temp childs
        ]
addBullet (StyleTree self@TextStyle { markerIsRight = Just True,
        isListItem = True, listPosInside = False, markerPseudo = Just child
    } childs) store cstyle txt = insertPseudos store $
        StyleTree self {
            isListItem = False,
            counterProps=insertList "display" [Ident "flex"] $ counterProps child
        } [
            StyleTree temp childs,
            t cstyle child {
                counterProps = insertList "content" txt $ counterProps child
            }
        ]
addBullet (StyleTree self childs) store _ _ =
    insertPseudos store $ StyleTree self {
        isListItem = False,
        listStyleImage = [], listStyleType = []
    } childs

t :: Maybe CounterStyle -> TextStyle p -> StyleTree (TextStyle p)
t (Just cstyle) self = StyleTree self {
    counterProps = insertList "speak-as" [Ident $ speakAs' cstyle] $
        counterProps self } []
t Nothing self = StyleTree self []

--------
---- Counters
--------
type Context = M.HashMap Text [([Integer], Int)]

inheritCounters :: Context -> Context -> Context
inheritCounters counterSource valueSource = M.unionWith cb valueSource counterSource -- indexed by name & el-path
    where cb val source = [counter | counter@(path, _) <- val,
                            path `elem` [p | (p, _) <- source]]

instantiateCounter :: Context -> Path -> Text -> Int -> Context
instantiateCounter counters path name val =
        M.insertWith appendCounter name [(path, val)] counters
    where
        appendCounter new (old@((_:oldPath), _):olds)
            | oldPath == tail path = new ++ olds
            | otherwise =  new ++ (old:olds)
        appendCounter new [] = new
        appendCounter new (_:olds) = new ++ olds
instantiateCounters :: Path -> Counters -> Context -> Context
instantiateCounters path instruct counters = foldl cb counters instruct
    where cb counters' (name, value) = instantiateCounter counters' path name value

incrementCounter :: Context -> Path -> Text -> Int -> Context
incrementCounter counters path name val =
        M.insertWith addCounter name [(path, val)] counters
    where
        addCounter ((_, new):_) ((path', old):rest) = (path', new + old):rest
        addCounter [] old = old
        addCounter new [] = new
incrementCounters :: Path -> Counters -> Context -> Context
incrementCounters path instruct counters = foldl cb counters instruct
    where cb counters' (name, value) = incrementCounter counters' path name value

setCounter :: Context -> Path -> Text -> Int -> Context
setCounter counters path name val = M.insertWith setCounter' name [(path, val)] counters
    where
        setCounter' ((_, val'):_) ((path', _):rest) = (path', val'):rest
        setCounter' [] old = old
        setCounter' new [] = new
setCounters :: Path -> Counters -> Context -> Context
setCounters path instruct counters = foldl cb counters instruct
    where cb counters' (name, value) = setCounter counters' path name value


renderCounters :: CounterStore -> Context -> [Token] -> [Token]
renderCounters store counters (Function "counter":Ident name:RightParen:toks)
    | Just ((_, count):_) <- name `M.lookup` counters =
        String (counterRender decimalCounter count) : renderCounters store counters toks
    | otherwise = renderCounters store counters toks
renderCounters store counters (Function "counter":Ident name:Comma:toks)
    | Just ((_, count):_) <- name `M.lookup` counters,
            Just (cstyle, RightParen:toks') <- parseCounter store toks =
        String (counterRender cstyle count) : renderCounters store counters toks'
    | otherwise = renderCounters store counters $ skipBlock toks
renderCounters store counters (Function "counters":Ident name:Comma:String sep:RightParen:toks)
    | Just counter <- name `M.lookup` counters = String (Txt.intercalate sep [
        counterRender decimalCounter count | (_, count) <- reverse counter
    ]) : renderCounters store counters toks
    | otherwise = renderCounters store counters toks
renderCounters store counters (Function "counters":Ident name:Comma:String sep:Comma:toks)
    | Just counter <- name `M.lookup` counters,
            Just (cstyle, RightParen:toks') <- parseCounter store toks =
        String (Txt.intercalate sep [
            counterRender cstyle count | (_, count) <- reverse counter
        ]) : renderCounters store counters toks'
    | otherwise = renderCounters store counters toks
renderCounters store counters (tok:toks) = tok : renderCounters store counters toks
renderCounters _ _ [] = []

skipBlock :: [Token] -> [Token]
skipBlock = snd . scanBlock

applyCounters :: CounterStore -> StyleTree (TextStyle p) -> StyleTree (TextStyle p)
applyCounters counters = treeOrder (applyCounters0 counters) M.empty
applyCounters0 :: CounterStore -> Context -> Context -> Path -> TextStyle p ->
        (Context, TextStyle p)
applyCounters0 store counterSource valueSource path node =
    let counters = inheritCounters counterSource valueSource &
            instantiateCounters path (counterReset node) &
            incrementCounters path (counterIncrement node) &
            setCounters path (counterSet node)
    in (counters, node {
        counterProps = [(k, renderCounters store counters v) | (k, v) <- counterProps node]
    })

--------
---- white-space
--------
content :: TextStyle p -> [Token]
content = fromMaybe [] . lookup "content" . counterProps
setContent :: [Token] -> TextStyle p -> TextStyle p
setContent value self = self {
        counterProps = [(k, if k == "content" then value else v) | (k, v) <- counterProps self]
    }

collapseWS :: StyleTree (TextStyle p) -> StyleTree (TextStyle p)
collapseWS = treeOrder collapseWS0 True
collapseWS0 :: Bool -> Bool -> Path -> TextStyle p -> (Bool, TextStyle p)
collapseWS0 _ _ _ node@(TextStyle {
    whiteSpaceCollapse = False, newlineCollapse = False }) = (False, node)
collapseWS0 _ inSpace _ node@(TextStyle {
        whiteSpaceCollapse = wsCollapse,
        newlineCollapse = nlCollapse
    }) = (trailingSpace, setContent content' node)
  where (trailingSpace, content') =
            collapseWSToks inSpace wsCollapse nlCollapse $ content node

collapseWSToks :: Bool -> Bool -> Bool -> [Token] -> (Bool, [Token])
collapseWSToks stripStart wsCollapse nlCollapse (String txt:toks) =
    let (trailingSpace, str') =
            collapseWSStr stripStart wsCollapse nlCollapse $ Txt.unpack txt
        (trailingSpace', toks') =
            collapseWSToks trailingSpace wsCollapse nlCollapse toks
    in (trailingSpace', String (Txt.pack str'):toks')
collapseWSToks _ wsCollapse nlCollapse (tok:toks) =
    let (trailingSpace, toks') = collapseWSToks False wsCollapse nlCollapse toks
    in (trailingSpace, tok:toks')
collapseWSToks trailingWS _ _ [] = (trailingWS, [])

collapseWSStr, collapseWSStr' :: Bool -> Bool -> Bool -> String -> (Bool, String)
collapseWSStr _ wsCollapse False str@('\n':_) =
    collapseWSStr' True wsCollapse False str
collapseWSStr True True nlCollapse (ch:str) | isSpace ch =
    collapseWSStr True True nlCollapse str
collapseWSStr False True nlCollapse str@(ch:_) | isSpace ch =
    collapseWSStr' True True nlCollapse str
collapseWSStr _ wsCollapse nlCollapse str =
    collapseWSStr' False wsCollapse nlCollapse str
collapseWSStr' a b c (d:ds) =
    let (trailing, ds') = collapseWSStr a b c ds in (trailing, d:ds')
collapseWSStr' a _ _ [] = (a, [])
