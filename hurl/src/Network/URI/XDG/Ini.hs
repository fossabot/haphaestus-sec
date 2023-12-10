module Network.URI.XDG.Ini(INI, parseIni, iniLookup, iniLookupLocalized) where

import Data.Char (isSpace, toLower)
import Data.List (dropWhile, dropWhileEnd)

type INI = [(String, [(String, String)])]

parseIni :: String -> INI
parseIni source = parseIni' $ filter (not . isComment) $ map strip $ lines source

strip cs = dropWhile isSpace $ dropWhileEnd isSpace $ map toLower cs
strip2 (a, b) = (strip a, strip b)

isComment ('#':_) = True
isComment "" = True
isComment _ = False

parseIni' (('[':cs):lines) | ']':header <- reverse cs =
    let (keys, rest) = parseKeys lines in (strip $ reverse header, keys) : parseIni' rest
parseIni' _ = []

parseKeys :: [String] -> ([(String, String)], [String])
parseKeys lines@(('[':_):_) = ([], lines)
parseKeys (line:lines) =
    let (keys, rest) = parseKeys lines in (strip2 (parseKey line) : keys, rest)
parseKeys [] = ([], [])

parseKey ('=':as) = ([], as)
parseKey (a:as) = let (x, y) = parseKey as in (a:x, y)
parseKey [] = ([], [])

---

iniLookup :: String -> String -> INI -> Maybe String
iniLookup group key ini = lookup group ini >>= lookup key

iniLookupLocalized :: [String] -> String -> String -> INI -> Maybe String
iniLookupLocalized (locale:locales) group key ini
    | Just ret <- iniLookup group (key ++ "[" ++ locale' ++ "]") ini = Just ret
    | otherwise = iniLookupLocalized locales group key ini
    where locale' = map dash2under locale
iniLookupLocalized [] group key ini = iniLookup group key ini

dash2under '-' = '_'
dash2under c = c
