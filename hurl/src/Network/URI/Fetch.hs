{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Retrieves documents for a URL, supporting multiple URL schemes that can be
-- disabled at build-time for reduced dependencies.
module Network.URI.Fetch(
    Session(locale, aboutPages, redirectCount, cachingEnabled, validateCertificates, credentials),
    newSession,
    fetchURL, fetchURL', fetchURLs, submitURL, submitURL', mimeERR, htmlERR,
    dispatchByMIME, appsForMIME, Application(..), dispatchByApp,
    saveDownload, downloadToURI,
    -- logging API
    LogRecord(..), enableLogging, retrieveLog, writeLog) where

import Network.URI.Types

import qualified Data.Text as Txt
import           Data.Text (Text)
import qualified Data.Text.Encoding as Txt
import           Network.URI
import qualified Data.ByteString as Strict
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Builder as Builder
import           Network.URI.Charset
import           Control.Exception
import           System.IO.Error (isEOFError)
import           Control.Concurrent.Async (forConcurrently)

import qualified Data.Maybe as M

-- for about: URIs & port parsing, all standard lib
import Data.Maybe (fromMaybe, listToMaybe, isJust)
import Data.Either (isLeft)
import Text.Read (readMaybe)
-- for executable extensions, all standard lib
import Data.Char (isSpace)
import System.Exit (ExitCode(..))

-- for saveDownload
import System.Directory
import System.FilePath

-- for logging
import Control.Concurrent.MVar
import Data.Time.Clock
import System.IO
import Control.Monad
import Data.List as L

#ifdef WITH_HTTP_URI
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.MultipartFormData as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import           Network.HTTP.Types
import           Network.PublicSuffixList.Lookup (effectiveTLDPlusOne)

import           Data.List (intercalate)
import           Control.Concurrent (forkIO)

import           Network.URI.Cache
import           Network.URI.CookiesDB
#endif

#if WITH_HTTP_URI || WITH_RAW_CONNECTIONS
import qualified Network.Connection as Conn
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS
import           Data.Default.Class (def)
#endif

#ifdef WITH_DATA_URI
import qualified Data.ByteString.Base64.URL.Lazy as B64
#endif

import Network.URI.Locale
import Network.URI.Messages

#ifdef WITH_XDG
import Network.URI.XDG
#endif

#ifdef WITH_PLUGIN_REWRITES
import Network.URI.PlugIns.Rewriters
#endif
#ifdef WITH_PLUGIN_EXEC
import System.Process
#endif

-- | Data shared accross multiple URI requests.
data Session = Session {
#ifdef WITH_HTTP_URI
    managerHTTP :: HTTP.Manager,
    managerHTTPNoValidate :: HTTP.Manager,
    globalCookieJar :: MVar HTTP.CookieJar,
    cookiesPath :: FilePath,
    retroactiveCookies :: Maybe (MVar HTTP.CookieJar),
    hstsDomains :: MVar [(String, Bool, UTCTime)],
#endif
#ifdef WITH_RAW_CONNECTIONS
    connCtxt :: Conn.ConnectionContext,
#endif
#ifdef WITH_XDG
    apps :: XDGConfig,
#endif
#ifdef WITH_PLUGIN_REWRITES
    rewriter :: Rewriter,
#endif
    -- | The languages (RFC2616-encoded) to which responses should be localized.
    locale :: [String],
    -- | Callback function for localizing error messages, or throwing exceptions
    trans' :: Errors -> String,
    -- | Additional files to serve from about: URIs.
    aboutPages :: [(FilePath, ByteString)],
    -- | Log of timestamped/profiled URL requests
    requestLog :: Maybe (MVar [LogRecord]),
    -- | How many redirects to follow for Gemini or HTTP(S) requests
    redirectCount :: Int,
    -- | Whether to cache network responses, avoiding sending requests
    cachingEnabled :: Bool,
    -- | App-specific config subdirectory to check
    appName :: String,
    -- | Whether to validate the server is who they say they are on secured protocols.
    validateCertificates :: Bool,
    -- | Bytestrings or files containing the client certificate to use for logging into the server.
    credentials :: Maybe (Either (FilePath, FilePath) (C8.ByteString, C8.ByteString)),
    credentials' :: MVar (Maybe (Either (FilePath, FilePath) (C8.ByteString, C8.ByteString)))
}

data LogRecord = LogRecord {
    url :: URI,
    accept :: [String],
    redirected :: URI,
    mimetype :: String,
    response :: Either Text ByteString,
    begin :: UTCTime,
    end :: UTCTime
  }

-- | Initializes a default Session object to support HTTPS & Accept-Language
-- if HTTP is enabled.
newSession :: IO Session
newSession = newSession' ""

-- | Variant of `newSession` which loads plugins for the named app.
newSession' :: String -> IO Session
newSession' appname = do
    (ietfLocale, unixLocale) <- rfc2616Locale
    credentialsMVar <- newMVar Nothing
#ifdef WITH_HTTP_URI
    {- let httpsSettings = (TLS.defaultParamsClient "example.com" "https") {
        TLS.clientSupported = def { TLS.supportedCiphers = TLS.ciphersuite_default },
        TLS.clientHooks = def {
            TLS.onCertificateRequest = deliverCredentials credentialsMVar
        }
    }
    let httpsSettingsNoValidate = httpsSettings {
        TLS.clientShared = def {
            TLS.sharedValidationCache = TLS.ValidationCache
                (\_ _ _ -> return TLS.ValidationCachePass)
                (\_ _ _ -> return ())
        }
    } -} -- FIXME: Be nice to support clientside certs... Those are far too strict!
    managerHTTP' <- HTTP.newManager $ TLS.mkManagerSettings
        (Conn.TLSSettingsSimple False False False) Nothing
    managerHTTPnovalidate' <- HTTP.newManager $ TLS.mkManagerSettings
        (Conn.TLSSettingsSimple True False False) Nothing

    cookiesDir <- getXdgDirectory XdgData "nz.geek.adrian.hurl.cookies2"
    let cookiesPath' = cookiesDir </> appname
    cookies' <- readCookies cookiesPath'
    now <- getCurrentTime
    cookieJar <- newMVar $ HTTP.evictExpiredCookies cookies' now
    cookieJar' <- newMVar $ HTTP.createCookieJar []

    hstsDomains' <- newMVar =<< readHSTS
#endif
#ifdef WITH_RAW_CONNECTIONS
    connCtxt <- Conn.initConnectionContext
#endif
#ifdef WITH_XDG
    apps' <- loadXDGConfig unixLocale
#endif
#ifdef WITH_PLUGIN_REWRITES
    rewriters <- parseRewriters appname
#endif

    return Session {
#ifdef WITH_HTTP_URI
        managerHTTP = managerHTTP',
        managerHTTPNoValidate = managerHTTPnovalidate',
        globalCookieJar = cookieJar,
        cookiesPath = cookiesPath',
        retroactiveCookies = Just cookieJar',
        hstsDomains = hstsDomains',
#endif
#ifdef WITH_RAW_CONNECTIONS
        connCtxt = connCtxt,
#endif
#ifdef WITH_XDG
        apps = apps',
#endif
#ifdef WITH_PLUGIN_REWRITES
        rewriter = rewriters,
#endif
        locale = ietfLocale,
        trans' = trans ietfLocale,
        aboutPages = [],
        requestLog = Nothing,
        redirectCount = 5,
        cachingEnabled = True,
        validateCertificates = True,
        appName = appname,
        credentials = Nothing,
        credentials' = credentialsMVar
    }

llookup key fallback map = fallback `fromMaybe` listToMaybe [v | (k, v) <- map, k == key]
parsePort fallback (':':port) = fallback `fromMaybe` readMaybe port
parsePort fallback _ = fallback

-- | Retrieves a URL-identified resource & it's MIMEtype, possibly decoding it's text.
fetchURL :: Session -- ^ The session of which this request is a part.
    -> [String] -- ^ The expected MIMEtypes in priority order.
    -> URI -- ^ The URL to retrieve
    -> IO (String, Either Text ByteString) -- ^ The MIMEtype & possibly text-decoded response.
fetchURL sess mimes uri = do
    (_, mime, resp) <- fetchURL' sess mimes uri
    return (mime, resp)

fetchURLLogged log sess mimes uri = do
    begin' <- getCurrentTime
    res@(redirected', mimetype', response') <- fetchURL' sess mimes uri
    end' <- getCurrentTime
    modifyMVar_ log $ \log' -> return (
        LogRecord uri mimes redirected' mimetype' response' begin' end' : log')
    return res

-- | Concurrently fetch given URLs.
fetchURLs :: Session -> [String] -> [URI] -> ((URI, String, Either Text ByteString) -> IO a) -> IO [(URI, a)]
fetchURLs sess mimes uris cb = do
    let fetch = case requestLog sess of {Nothing -> fetchURL'; Just log -> fetchURLLogged log}
    let sess' = sess {
#ifdef WITH_HTTP_URI
        retroactiveCookies = Nothing
#endif
      }
    forConcurrently uris (\u -> fetch sess' mimes u >>= cb) >>= return . L.zip uris

-- | Internal MIMEtypes for error reporting
mimeERR, htmlERR :: String
mimeERR = "txt/x-error\t"
htmlERR = "html/x-error\t"

submitURL :: Session -> [String] -> URI -> Text -> String -> IO (URI, String, Either Text ByteString)
-- | See submitURL', preserved for backwards compatability.
-- This is a little more cumbersome to use, & doesn't support file uploads.
-- Was designed naively based on convenience of initial caller.
submitURL s a u m q =
    submitURL' s a u (Txt.encodeUtf8 m) "application/x-www-form-urlencoded" $
        Prelude.map parseQuery $ Txt.splitOn "&" $ Txt.pack q
  where
    parseQuery q = let (key, value) = Txt.breakOn "=" q in if Txt.null value
        then (decode key, Left "")
        else (decode key, Left $ decode $ Txt.tail value)
    decode = unEscapeString . Txt.unpack
-- | Uploads given key-value pairs to the specified URL using the specified HTTP method & encoding.
-- The key-value pairs may specify filepaths, in which case the method must be "POST"
-- and the encoding must be "multipart/form-data" for that data to get sent.
--
-- Unsupported encodings (values other than "application/x-www-form-urlencoded",
-- "text/plain", or "multipart/form-data") omits the key-value pairs from the request.
submitURL' :: Session -> [String] -> URI -> Strict.ByteString -> Strict.ByteString ->
    [(String, Either String FilePath)] -> IO (URI, String, Either Text ByteString)
#ifdef WITH_HTTP_URI
addHTTPBody mime body req = return req {
    HTTP.requestHeaders = (hContentType, mime) :
        Prelude.filter (\(x, _) -> x /= hContentType) (HTTP.requestHeaders req),
    HTTP.requestBody = HTTP.RequestBodyBS $ C8.pack body
  }
packQuery :: [(String, Either String FilePath)] -> C8.ByteString -> HTTP.Request -> IO HTTP.Request
packQuery query mime@"application/x-www-form-urlencoded" =
    addHTTPBody mime $ encodeQuery query
packQuery query mime@"text/plain" = addHTTPBody mime $
    Prelude.unlines [value | (key, Left value) <- query, not $ null value]
packQuery q "multipart/form-data" = HTTP.formDataBody $ Prelude.map encodePart q
  where
    encodePart (key, Left value) = HTTP.partBS (Txt.pack key) (C8.pack value)
    encodePart (key, Right value) =
        -- C:\fakepath\ is part of the webstandards now & I might as well use it.
        -- Ancient browsers exposed the full filepath which was a security vulnerability.
        -- Now to avoid breaking servers relying on this behaviour we send
        -- a fake Windows filepath.
        HTTP.partFileRequestBodyM (Txt.pack key) ("C:\\fakepath\\" ++ takeFileName value) $ do
            size <- fromInteger <$> withBinaryFile value ReadMode hFileSize
            body <- B.readFile value
            return $ HTTP.RequestBodyBuilder size $ Builder.lazyByteString body
packQuery _ _ = return -- Do not upload data if requested to do so in an invalid format.
submitURL' session mimes uri method "GET" query = fetchURL' session mimes uri {
    uriQuery = '?': encodeQuery query } -- Specialcase GET!
submitURL' session accept uri method encoding query | uriScheme uri `elem` ["http:", "https:"] = do
    -- HURL is very strict on when it allows cookies to be set: Only POST HTTP requests are considered consent.
    -- For the sake of most webframeworks' CSRF protection, cookies from retrieving the form are retroactively set.
    csrfCookies <- case retroactiveCookies session of {
        Just cookies -> Just <$> readMVar cookies;
        Nothing -> return Nothing
    }
    fetchHTTPCached session False accept uri (\req -> do
        ret <- packQuery query encoding req
        return ret {
            HTTP.cookieJar = firstJust csrfCookies $ HTTP.cookieJar req,
            HTTP.method = method
        }) $ \resp -> do
            let cookies = HTTP.responseCookieJar resp
            swapMVar (globalCookieJar session) cookies
            writeCookies (cookiesPath session) cookies False
#endif
submitURL' session mimes uri _method _encoding query = fetchURL' session mimes uri {
    uriQuery = '?':encodeQuery query }
encodeQuery :: [(String, Either String FilePath)] -> String
encodeQuery [("", Left query)] = query -- Mostly for the sake of Gemini...
encodeQuery query = intercalate "&" $ M.mapMaybe encodePart query
  where
    encodePart (key, Left "") = Just $ escape key
    encodePart (key, Left value) = Just (escape key ++ '=':escape value)
    encodePart _ = Nothing
    escape = escapeURIString isUnescapedInURIComponent

-- | As per `fetchURL`, but also returns the redirected URI.
fetchURL' :: Session -> [String] -> URI -> IO (URI, String, Either Text ByteString)
fetchURL' sess@Session {redirectCount = 0 } _ uri =
    return (uri, mimeERR, Left $ Txt.pack $ trans' sess ExcessiveRedirects)

#ifdef WITH_PLUGIN_REWRITES
fetchURL' session mimes uri
    | Just uri' <- applyRewriter (rewriter session) uri = fetchURL' session mimes uri'
#endif

#ifdef WITH_PLUGIN_EXEC
fetchURL' session@Session { appName = appname } mimes uri@(URI "ext:" Nothing path query _) = do
    dir <- getXdgDirectory XdgData "nz.geek.adrian.hurl"
    sysdirs <- getXdgDirectoryList XdgDataDirs
    let dirs = concat [[dir' </> appname, dir'] | dir' <- dir : sysdirs]
    programs <- findExecutablesInDirectories dirs ("bin" </> path)
    case programs of
      [] -> return (uri, mimeERR, Left $ Txt.pack $ trans' session $ ReadFailed "404")
      program:_ -> do
        let args = case query of {
            '?':rest -> split (== '&') rest;
            _ -> []
        }
        (exitcode, stdout, stderr) <- readProcessWithExitCode program args ""
        let response = if isSuccess exitcode then stdout else stderr
        let (header, body) = breakOn '\n' response
        case strip header of
            'm':'i':'m':'e':mimetype -> return (uri, strip mimetype, Left $ Txt.pack body)
            'u':'r':'l':header' | Just uri' <- parseURIReference $ strip header' ->
                fetchURL' (session {redirectCount = redirectCount session - 1}) mimes $
                    relativeTo uri' uri
            _ | isSuccess exitcode -> return (uri, "text/html", Left $ Txt.pack response)
            _ -> return (uri, mimeERR, Left $ Txt.pack response)
  where
    split p s = case dropWhile p s of
        "" -> []
        s' -> let (w, s'') = break p s' in w : split p s''
    strip = dropWhile isSpace . dropWhileEnd isSpace
    isSuccess ExitSuccess = True
    isSuccess _ = False
#endif

fetchURL' session mimes uri@(URI {uriScheme = "about:", uriPath = ""}) =
    fetchURL' session mimes $ uri {uriPath = "version"}
fetchURL' Session {aboutPages = pages} _ url@URI {uriScheme = "about:", uriPath = path} =
    return (url,
        Txt.unpack $ Txt.strip $ convertCharset "utf-8" $ B.toStrict $
            llookup (path ++ ".mime") "text/html" pages,
        Right $ llookup path "" pages)

#ifdef WITH_HTTP_URI
fetchURL' session accept uri | uriScheme uri `elem` ["http:", "https:"] =
    fetchHTTPCached session (cachingEnabled session) accept uri return saveCookies
  where
    saveCookies resp
        | Just cookies <- retroactiveCookies session =
            void $swapMVar cookies $HTTP.responseCookieJar resp
        | otherwise = return ()
#endif

#ifdef WITH_GEMINI_URI
fetchURL' sess@Session { connCtxt = ctxt } mimes uri@URI {
        uriScheme = "gemini:", uriAuthority = Just (URIAuth _ host port)
    } = do
        let params = TLS.defaultParamsClient host "gmni"
        swapMVar (credentials' sess) (credentials sess)
        conn <- Conn.connectTo ctxt Conn.ConnectionParams {
            Conn.connectionHostname = host,
            Conn.connectionPort = parsePort 1965 port,
            -- FIXME Implement certificate validation that actually common geminispace certs...
            Conn.connectionUseSecure = Just $ Conn.TLSSettings params {
                TLS.clientSupported = def { TLS.supportedCiphers = TLS.ciphersuite_default },
                TLS.clientShared = def {
                    TLS.sharedValidationCache = TLS.ValidationCache
                        (\_ _ _ -> return TLS.ValidationCachePass)
                        (\_ _ _ -> return ())
                },
                TLS.clientHooks = def {
                    TLS.onCertificateRequest = deliverCredentials $ credentials' sess
                }
            },
            Conn.connectionUseSocks = Nothing
        }
        Conn.connectionPut conn $ C8.pack $ uriToString id uri "\r\n"
        header <- Conn.connectionGetLine 1027 conn
        case parseHeader $ C8.unpack header of
            ('2', _, mime) -> do
                body <- B.fromChunks <$> connectionGetChunks conn
                let mime' = L.map (Txt.unpack . Txt.strip) $ Txt.splitOn ";" mime
                return $ resolveCharset' uri mime' body
            ('3', _, redirect) | Just redirect' <- parseURIReference $ Txt.unpack redirect ->
                fetchURL' sess {
                    redirectCount = redirectCount sess - 1
                } mimes $ relativeTo redirect' uri
            (x, y, err) -> return (uri, htmlERR, Left $ Txt.pack $
                trans' sess $ GeminiError x y $ Txt.unpack $
                    Txt.replace "<" "&lt;" $ Txt.replace "&" "&amp;" err)
    where
        parseHeader :: String -> (Char, Char, Text)
        parseHeader (major:minor:meta) = (major, minor, Txt.strip $ Txt.pack meta)
        parseHeader header = ('4', '1', Txt.pack $ trans' sess $ MalformedResponse header)
        handleIOErr :: IOError -> IO Strict.ByteString
        handleIOErr _ = return ""
        connectionGetChunks conn = do
            chunk <- Conn.connectionGetChunk conn `catch` handleIOErr
            if Strict.null chunk then return [] else (chunk:) <$> connectionGetChunks conn
#endif

#ifdef WITH_FILE_URI
fetchURL' sess (defaultMIME:_) uri@URI {uriScheme = "file:"} = do
    response <- B.readFile $ uriPath uri
    return (uri, defaultMIME, Right response)
  `catch` \e -> do
    return (uri, mimeERR,
        Left $ Txt.pack $ trans' sess $ ReadFailed $ displayException (e :: IOException))
#endif

#ifdef WITH_DATA_URI
fetchURL' _ (defaultMIME:_) uri@URI {uriScheme = "data:"} =
    let request = uriPath uri ++ uriQuery uri ++ uriFragment uri
    in case breakOn ',' $ unEscapeString request of
        ("", response) -> return (uri, defaultMIME, Left $ Txt.pack response)
        (mime', response) | '4':'6':'e':'s':'a':'b':';':mime <- reverse mime' ->
            return $ case B64.decode $ B.fromStrict $ C8.pack response of
                Left str -> (uri, mimeERR, Left $ Txt.pack $ unEscapeString str)
                Right bytes -> (uri, reverse mime, Right bytes)
        (mime, response) -> return (uri, mime, Left $ Txt.pack response)
#endif

#ifdef WITH_XDG
fetchURL' sess@Session { apps = a } _ uri@(URI {uriScheme = s}) = do
        app <- dispatchURIByMIME a uri ("x-scheme-handler/" ++ init s)
        return (uri, htmlERR, Left $ Txt.pack $ trans' sess $ app)
#else
fetchURL' sess _ URI {uriScheme = scheme} =
    return (uri, mimeERR, Left $ Txt.pack $ trans' sess $ UnsupportedScheme scheme)
#endif

dispatchByMIME :: Session -> String -> URI -> IO (Maybe String)
#if WITH_XDG
dispatchByMIME sess@Session { apps = a } mime uri = do
    err <- dispatchURIByMIME a uri mime
    return $ case err of
        UnsupportedMIME _ -> Nothing
        _ -> Just $ trans' sess err
#else
dispatchByMIME _ _ _ = return Nothing
#endif

appsForMIME :: Session -> String -> IO [Application]
#if WITH_XDG
appsForMIME Session { apps = a, locale = l } = queryHandlers' a l
#else
appsForMIME _ _ = []
#endif

dispatchByApp :: Session -> Application -> String -> URI -> IO Bool
#if WITH_XDG
dispatchByApp session@Session { locale = l } Application { appId = app} mime uri = do
    try1 <- launchApp' l uri app -- First try handing off the URL, feedreaders need this!
    case try1 of
        Left app -> return True
        Right False -> return False
        Right True -> do
            -- Download as temp file to open locally, the app requires it...
            temp <- canonicalizePath =<< getTemporaryDirectory
            resp <- fetchURL' session [mime] uri
            uri' <- saveDownload (URI "file:" Nothing "" "" "") temp resp
            isLeft <$> launchApp' l uri' app
#else
dispatchByApp _ _ _ _ = return False
#endif

#ifdef WITH_HTTP_URI
fetchHTTPCached session@Session { trans' = t} shouldCache
        accept@(defaultMIME:_) rawUri cbReq cbResp = do
    now <- getCurrentTime
    hsts <- readMVar $ hstsDomains session
    uri <- case (uriScheme rawUri, uriAuthority rawUri) of {
        (_, Just (URIAuth _ domain _)) | not $ validateCertificates session -> (do
            modifyMVar_ (hstsDomains session) $ flip removeHSTS domain
            return rawUri);
        ("http:", Just (URIAuth _ domain _))
            | testHSTS now domain hsts -> return rawUri { uriScheme = "https:" };
        _ -> return rawUri
    }
    let manager = (if validateCertificates session
        then managerHTTP else managerHTTPNoValidate) session
    swapMVar (credentials' session) (credentials session)

    cached <- if shouldCache then readCacheHTTP uri else return (Nothing, Nothing)

    response <- case cached of
        (Just (mime, body), Nothing) -> return $ Right (mime, body)
        (cached, cachingHeaders) -> do
            request <- HTTP.requestFromURI uri
            cookieJar <- readMVar $ globalCookieJar session
            request'<- cbReq request {
                HTTP.cookieJar = Just $ cookieJar,
                HTTP.requestHeaders = [
                    ("Accept", C8.pack $ intercalate ", " accept),
                    ("Accept-Language", C8.pack $ intercalate ", " $ locale session)
                ] ++ fromMaybe [] cachingHeaders,
                HTTP.redirectCount = 0
            }
            response <- HTTP.httpLbs request' manager
            cbResp response
            case (
                uriScheme uri,
                uriAuthority uri,
                "strict-transport-security" `lookup` HTTP.responseHeaders response
              ) of
                ("https:", Just (URIAuth _ domain _), Just header)
                  | Just domain' <- effectiveTLDPlusOne $ Txt.pack domain -> do
                    record <- appendHSTSFromHeader (Txt.unpack domain') header
                    case record of
                        Just x -> modifyMVar_ (hstsDomains session) (return . (x:))
                        _ -> return ()
                _ -> return ()
            case (
                    HTTP.responseStatus response,
                    HTTP.responseBody response,
                    [val | ("content-type", val) <- HTTP.responseHeaders response]
              ) of
                (Status 304 _, _, _) | Just cached'@(_, body) <- cached -> do
                    cacheHTTP uri $ response { HTTP.responseBody = body }
                    return $ Right cached'
                -- Manually handle redirects so the caller & HTTP cache gets the correct URI.
                (Status code _, _, _) | code > 300 && code < 400,
                        Just location <- lookup "location" $ HTTP.responseHeaders response,
                        Just uri' <- parseURIReference $ C8.unpack location ->
                    return $ Left $ relativeTo uri' uri
                (Status code msg, "", _) -> return $ Right (Txt.pack htmlERR,
                    B.fromStrict $ C8.pack $
                        trans' session $ HTTPStatus code $ C8.unpack msg)
                (_, body, (mimetype:_)) -> do
                    cacheHTTP uri response
                    forkIO cleanCacheHTTP -- Try to keep diskspace down...

                    let mime = Txt.toLower $ convertCharset "utf-8" mimetype
                    return $ Right (mime, body)
                (_, response, []) -> return $ Right (Txt.pack defaultMIME, response)

    case response of
        Left redirect ->
            let session' = session { redirectCount = redirectCount session - 1 }
            in fetchURL' session' accept redirect
        Right (mime, body) ->
            let mime' = L.map (Txt.unpack . Txt.strip) $ Txt.splitOn ";" mime
            in return $ resolveCharset' uri mime' body
  `catch` \e -> do return (rawUri, htmlERR, Left $ Txt.pack $ transHttp t e)
fetchHTTPCached session _ [] uri _ _ =
    return (uri, htmlERR, Left $ Txt.pack $ trans' session $ UnsupportedMIME "")
#endif

#if WITH_HTTP_URI || WITH_GEMINI_URI
deliverCredentials credentialsMVar _ = do
    credentials' <- readMVar credentialsMVar -- workaround for HTTP-Client-TLS
    case credentials' of
        Just (Left (public, private)) -> right <$> TLS.credentialLoadX509 public private
        Just (Right (public, private)) ->
            return $ right $ TLS.credentialLoadX509FromMemory public private
        Nothing -> return Nothing
  where
    right (Left _) = Nothing
    right (Right x) = Just x
#endif

-- Downloads utilities
-- | write download to a file in the given directory.
saveDownload :: URI -> FilePath -> (URI, String, Either Text ByteString) -> IO URI
saveDownload baseURI dir (URI {uriPath = path}, mime, resp) = do
    dest <- unusedFilename (dir </> takeFileName' path)
    case resp of
        Left txt -> writeFile dest $ Txt.unpack txt
        Right bytes -> B.writeFile dest bytes
    -- TODO set user.mime file attribute.
    return $ baseURI {uriPath = dest}
  where
    takeFileName' s = case takeFileName s of { "" -> "index";  f -> f}

unusedFilename path = do
        exists <- doesFileExist path
        if exists then go 0 else return path
    where
        go n = do
            let path' = path ++ show n
            exists <- doesFileExist path'
            if exists then go (n+1) else return path'

-- | Convert a download into a data: URI
downloadToURI :: (URI, String, Either Text ByteString) -> URI
downloadToURI (_, mime, Left txt) = nullURI {
        uriScheme = "data:",
        uriPath = mime ++ "," ++ escapeURIString isReserved (Txt.unpack txt)
    }
downloadToURI (_, mime, Right bytes) = nullURI {
        uriScheme = "data:",
        uriPath = mime ++ ";base64," ++ C8.unpack (B.toStrict $ B64.encode bytes)
    }

-- Logging API
enableLogging :: Session -> IO Session
enableLogging session = do
    log <- newMVar []
    return session { requestLog = Just log }

retrieveLog :: Session -> IO [LogRecord]
retrieveLog session@Session { requestLog = Just log } = swapMVar log []
retrieveLog _ = return []

writeLog :: Handle -> Session -> IO ()
writeLog out session = do
    writeRow ["URL", "Redirected", "Accept", "MIMEtype", "Size", "Begin", "End", "Duration"]
    log <- retrieveLog session
    forM log $ \record -> writeRow [
        show $ url record, show $ redirected record,
        show $ accept record, show $ mimetype record,
        case response record of
            Left txt -> show $ Txt.length txt
            Right bs -> show $ B.length bs,
        show $ begin record, show $ end record,
        show (end record `diffUTCTime` end record)
      ]
    return ()
  where
    writeRow = hPutStrLn out . L.intercalate "\t"

-- Utils

breakOn c (a:as) | c == a = ([], as)
    | otherwise = let (x, y) = breakOn c as in (a:x, y)
breakOn _ [] = ([], [])

firstJust a@(Just _) _ = a
firstJust Nothing b = b
