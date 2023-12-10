{-# LANGUAGE OverloadedStrings #-}
module Network.URI.Cache(shouldCacheHTTP, cacheHTTP, readCacheHTTP, cleanCacheHTTP,
    writeHSTS, readHSTS, appendHSTS, appendHSTSFromHeader, removeHSTS, testHSTS) where
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header

-- For escaping filepaths, since I already have this dependency
import Network.URI (escapeURIString, isUnescapedInURIComponent, URI, uriToString)
import Data.Time.Clock
import Data.Time.Format

import Data.ByteString as Strict
import Data.ByteString.Char8 as C
import Data.ByteString.Lazy as Lazy
import System.IO as IO
import System.FilePath
import System.Directory
import qualified Data.Text as Txt

import Data.Maybe
import Data.Char (isSpace, isDigit, toLower)
import Data.List as L
import Control.Monad (forM, void, when)
import Text.Read (readMaybe)

stripBS = C.dropWhile isSpace -- FIXME Upgrade bytestring dependency for a real strip function.

httpCacheDirective :: Response b -> Strict.ByteString -> Maybe Strict.ByteString
httpCacheDirective response key | Just header <- lookup hCacheControl $ responseHeaders response =
        let directives = Prelude.map stripBS $ C.split ',' header
        in if key `Prelude.elem` directives
            then Just ""
            else listToMaybe $ mapMaybe (C.stripPrefix $ C.snoc key '=') directives
    | otherwise = Nothing

shouldCacheHTTP :: Response b -> Bool
-- IETF RFC7234 Section 3
shouldCacheHTTP response = -- Assume GET
    statusCode (responseStatus response) `Prelude.elem` [200, 201, 404] && -- Supported response code
        isNothing (httpCacheDirective response "no-store") -- Honor no-store
        -- This is a private cache, don't check for Cache-Control: private
        -- Also, I'll cache anything for supported response codes, regardless of explicit expiry times.

uriToString' uri = uriToString id uri ""
parseHTTPTime :: String -> Maybe UTCTime
parseHTTPTime str | ',' `L.elem` str = parseTimeM True defaultTimeLocale rfc822DateFormat str
parseHTTPTime str = parseTimeM True defaultTimeLocale "%_d %b %Y %H:%M:%S %Z" str
secondsFromNow i = do
    now <- getCurrentTime
    -- This ugliness required because regex depends on outdated version of time.
    return $ addUTCTime (fromRational $ toRational $ secondsToDiffTime i) now

computeExpires :: Response a -> IO UTCTime
computeExpires resp
  | Just header <- lookup hExpires $ responseHeaders resp,
        Just time <- parseHTTPTime $ C.unpack header = return time
  | Just pragma <- httpCacheDirective resp "max-age",
        Just seconds <- readMaybe $ C.unpack pragma = secondsFromNow seconds
  | otherwise = secondsFromNow (60*60*24) -- One day

cacheHTTP :: URI -> Response Lazy.ByteString -> IO ()
cacheHTTP uri resp | shouldCacheHTTP resp = do
    expires <- computeExpires resp
    let headers = responseHeaders resp
    writeKV (uriToString' uri) (
        [("expires", show expires)] ++ getHeader "content-type" "mime" ++
            getHeader "ETag" "etag" ++ getHeader "Last-Modified" "modified",
        responseBody resp)
  where
    getHeader header key | Just value <- lookup header $ responseHeaders resp = [(key, C.unpack value)]
        | otherwise = []
cacheHTTP _ _ = return ()

readCacheHTTP :: URI -> IO (Maybe (Txt.Text, Lazy.ByteString), Maybe ResponseHeaders)
readCacheHTTP uri = do
    cached <- readKV $ uriToString' uri
    case cached of
        Just (headers, body) | Just expiry <- readMaybe =<< lookup "expires" headers -> do
            let mime = fromMaybe "application/octet-stream" $ lookup "mime" headers
            now <- getCurrentTime

            -- Headers for a validation request & whether should be sent.
            let headers' = if expiry <= now then Nothing else Just (
                    [("If-Modified-Since", C.pack val) | ("modified", val) <- headers,
                        isJust $ parseHTTPTime val] ++
                    [("If-None-Match", C.pack val) | ("etag", val) <- headers])
            -- Cache entry has expired, delete.
            when (isJust headers') $ deleteKV $ uriToString' uri

            return (Just (Txt.pack mime, body), headers')

        _ -> return (Nothing, Just [])

cleanCacheHTTP = void $ do
    now <- getCurrentTime
    let tombstone = now

    dir <- getXdgDirectory XdgCache "nz.geek.adrian.hurl"
    dirExists <- doesDirectoryExist (dir </> "http")
    files <- if dirExists then listDirectory (dir </> "http") else return []
    forM files $ \file -> do
        exists <- doesFileExist file
        when exists $ IO.withFile file ReadMode $ \h -> do
            (headers, _) <- parseHeaders h
            let hasHeader h = isJust $ lookup h headers
                validatable = hasHeader "modified" || hasHeader "etag"
                expires = fromMaybe tombstone (readMaybe =<< lookup "expires" headers)
            when (now >= expires && not validatable) $ removeFile file

------
--- Key-value storage
------

readKV :: String -> IO (Maybe ([(String, String)], Lazy.ByteString))
writeKV :: String -> ([(String, String)], Lazy.ByteString) -> IO ()
deleteKV :: String -> IO ()
openKV :: String -> IO.IOMode -> (Handle -> IO r) -> IO (Maybe r)
pathKV :: String -> IO FilePath

pathKV key = do
    dir <- getXdgDirectory XdgCache "nz.geek.adrian.hurl"
    createDirectoryIfMissing True (dir </> "http")
    return (dir </> "http" </> escapeURIString isUnescapedInURIComponent key)

openKV key mode act = do
    path <- pathKV key
    exists <- doesFileExist path
    if exists then Just <$> IO.withFile path mode act else return Nothing

readKV key = openKV key ReadMode parseHeaders

parseHeaders h = do
    isEnd <- IO.hIsEOF h
    if isEnd then return ([], "") else do
        line <- IO.hGetLine h
        case L.break isSpace $ strip' line of
            ("", "") -> do
                body <- Lazy.hGetContents h
                return ([], body)
            (key, value) -> do
                (headers, body) <- parseHeaders h
                return ((key, strip' value):headers, body)
strip' = L.dropWhile isSpace . L.dropWhileEnd isSpace

writeKV key (headers, body) = void $ openKV key WriteMode $ \h -> do
    forM headers $ \(key, value) -> do
        IO.hPutStrLn h (key++' ':value)
    IO.hPutStrLn h ""
    Lazy.hPut h body

deleteKV key = pathKV key >>= removeFile

--------
---- HSTS Support
--------
readHSTS :: IO [(String, Bool, UTCTime)]
readHSTS = do
    (headers, _) <- fromMaybe ([], "") <$> readKV ".HSTS"
    -- Remove expired & duplicate entries on startup via `nubHSTS`
    now <- getCurrentTime
    let db = nubHSTS now (L.reverse $ mapMaybe parseRecord headers) []
    writeHSTS $ seq (L.length db) db -- Ensure the file is fully read before being written.
    return db
  where
    parseRecord ('*':domain, value) | Just expires <- readMaybe value = Just (domain, True, expires)
    parseRecord (domain, value) | Just expires <- readMaybe value = Just (domain, False, expires)
    parseRecord _ = Nothing
appendHSTS :: (String, Bool, UTCTime) -> IO ()
appendHSTS = void . openKV ".HSTS" AppendMode . flip appendHSTS'
appendHSTS' h (domain, True, expires) = IO.hPutStrLn h ('*':domain ++ ' ':show expires)
appendHSTS' h (domain, False, expires) = IO.hPutStrLn h (domain ++ ' ':show expires)
writeHSTS :: [(String, Bool, UTCTime)] -> IO ()
writeHSTS domains = void . openKV ".HSTS" WriteMode $ \h -> forM domains (appendHSTS' h)

-- Directly disregards IETF RFC6797 section 12.1
-- I prefer not to give up on designing a proper consent UI.
removeHSTS :: [(String, Bool, UTCTime)] -> String -> IO [(String, Bool, UTCTime)]
removeHSTS db badDomain = do
    now <- getCurrentTime -- Clear out expired records while we're at it...
    let ret = nubHSTS now db [badDomain]
    writeHSTS ret
    return ret

nubHSTS now (x@(domain, _, expires):db) filter
    | domain `L.elem` filter = nubHSTS now db (domain:filter)
    -- Filter out expired entries while we're at it.
    | now >= expires = nubHSTS now db (domain:filter)
    | otherwise = x:nubHSTS now db (domain:filter)
nubHSTS _ [] _ = []

appendHSTSFromHeader :: String -> Strict.ByteString -> IO (Maybe (String, Bool, UTCTime))
appendHSTSFromHeader domain header =
    let dirs = parseDirectives $ C.split ';' header
    in if validateHSTS dirs then do
        expiry <- secondsFromNow $ fromMaybe 0 (readMaybe =<< lookup "max-age" dirs)
        -- FIXME: Is it right I'm ignoring if this has a value.
        let subdomains = isJust $ lookup "includesubdomains" dirs
        appendHSTS (domain, subdomains, expiry)
        return $ Just (domain, subdomains, expiry)
    else return Nothing

parseDirectives (dir:dirs) = case L.break (== '=') $ C.unpack dir of
    (key, '=':'"':quoted) | Just (value, dirs') <- parseString quoted dirs
        -> (lowercase $ strip key, value):parseDirectives dirs'
    (_, '=':'"':_) -> [("", "")] -- Represents error...
    (key, '=':value) -> (lowercase $ strip key, strip value):parseDirectives dirs
    (key, _) -> (lowercase $ strip key, ""):parseDirectives dirs
  where
    parseString ('\\':c:str) tail = appendC c $ parseString str tail
    parseString ("\"") tail = Just ("", tail)
    parseString ('"':_) _ = Nothing -- Disallow trailing text
    parseString (c:str) tail = appendC c $ parseString str tail
    -- Handle the naive split-by-semicolon above.
    parseString "" (extra:tail) = appendC ';' $ parseString (C.unpack extra) tail
    parseString "" [] = Nothing
    appendC c (Just (str, tail)) = Just (c:str, tail)
    appendC _ Nothing = Nothing

    strip = L.dropWhile isSpace . L.dropWhileEnd isSpace
    lowercase = L.map toLower
parseDirectives [] = []

validateHSTS directives
    | Just _ <- lookup "" directives = False -- indicates empty key or malformed string
    | Nothing <- lookup "max-age" directives = False -- mandatory field
    | Just val <- lookup "max-age" directives, L.any (not . isDigit) val = False -- invalid value
    | otherwise = validateHSTS' directives -- check no duplicate keys
validateHSTS' ((dir, _):dirs) | Just _ <- lookup dir dirs = False
    | otherwise = validateHSTS' dirs
validateHSTS' [] = True

testHSTS :: UTCTime -> String -> [(String, Bool, UTCTime)] -> Bool
testHSTS now key ((_, _, expires):db) | now > expires = testHSTS now key db
testHSTS _ key ((domain, _, _):db) | key == domain = True
testHSTS _ key ((domain, True, _):db) | ('.':domain) `L.isSuffixOf` key = True
testHSTS now key (_:db) = testHSTS now key db
testHSTS _ _ [] = False
