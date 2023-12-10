-- | Read & write Netscape Navigator cookies format.
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Network.URI.CookiesDB (readCookies, writeCookies) where
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Network.HTTP.Client
import           System.Directory (doesFileExist)

import           Web.Cookie (formatCookieExpires, parseCookieExpires)
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Time.Clock (nominalDay, getCurrentTime, addUTCTime, UTCTime)

readCookies :: FilePath -> IO CookieJar
readCookies filepath = do
    exists <- doesFileExist filepath
    if exists then do
        file <- B.readFile filepath
        now <- getCurrentTime
        return $ createCookieJar $ readCookies' now file
    else return $ createCookieJar []
readCookies' :: UTCTime -> B.ByteString -> [Cookie]
readCookies' now = mapMaybe (readCookie' now) . C.lines
readCookie' :: UTCTime -> B.ByteString -> Maybe Cookie
readCookie' now = readCookie now . C.split '\t'
readCookie :: UTCTime -> [B.ByteString] -> Maybe Cookie
readCookie now [domain, _, path, secure, expiration, name, value] =
    Just Cookie {
        cookie_domain = domain,
        cookie_path = path,
        cookie_secure_only = b secure,
        cookie_expiry_time = fromMaybe (addUTCTime nominalDay now) $ parseCookieExpires expiration,
        cookie_name = name,
        cookie_value = value,

        cookie_creation_time = now,
        cookie_last_access_time = now,
        cookie_persistent = True,
        cookie_host_only = False,
        cookie_http_only = False
    }
readCookie now [domain, _, path, secure, expiration, name, value, httpOnly, session] =
    Just Cookie {
        cookie_domain = domain,
        cookie_path = path,
        cookie_secure_only = b secure,
        cookie_expiry_time = fromMaybe (addUTCTime nominalDay now) $ parseCookieExpires expiration,
        cookie_name = name,
        cookie_value = value,
        cookie_http_only = b httpOnly,
        cookie_persistent = not $ b session,

        cookie_creation_time = now,
        cookie_last_access_time = now,
        cookie_host_only = False
    }
readCookie now [domain, _, path, secure, expiration, name, value,
    httpOnly, session, sameSite] = Just Cookie {
        cookie_domain = domain,
        cookie_path = path,
        cookie_secure_only = b secure,
        cookie_expiry_time = fromMaybe (addUTCTime nominalDay now) $ parseCookieExpires expiration,
        cookie_name = name,
        cookie_value = value,
        cookie_http_only = b httpOnly,
        cookie_persistent = not $ b session,
        cookie_host_only = sameSite == "STRICT",

        cookie_creation_time = now,
        cookie_last_access_time = now
    }
readCookie now [domain, _, path, secure, expiration, name, value,
    httpOnly, session, sameSite, _] = Just Cookie {
        cookie_domain = domain,
        cookie_path = path,
        cookie_secure_only = b secure,
        cookie_expiry_time = fromMaybe (addUTCTime nominalDay now) $ parseCookieExpires expiration,
        cookie_name = name,
        cookie_value = value,
        cookie_http_only = b httpOnly,
        cookie_persistent = not $ b session,
        cookie_host_only = sameSite == "STRICT",

        cookie_creation_time = now,
        cookie_last_access_time = now
    }
readCookie now [domain, _, path, secure, expiration, name, value,
    httpOnly, session, sameSite, _, creation] = Just Cookie {
        cookie_domain = domain,
        cookie_path = path,
        cookie_secure_only = b secure,
        cookie_expiry_time = fromMaybe (addUTCTime nominalDay now) $ parseCookieExpires expiration,
        cookie_name = name,
        cookie_value = value,
        cookie_http_only = b httpOnly,
        cookie_persistent = not $ b session,
        cookie_host_only = sameSite == "STRICT",
        cookie_creation_time = fromMaybe now $ parseCookieExpires creation,
        cookie_last_access_time = fromMaybe now $ parseCookieExpires creation
    }
readCookie now (domain:_:path:secure:expiration:name:value:
    httpOnly:session:sameSite:_:creation:access:_) = Just Cookie {
        cookie_domain = domain,
        cookie_path = path,
        cookie_secure_only = b secure,
        cookie_expiry_time = fromMaybe (addUTCTime nominalDay now) $ parseCookieExpires expiration,
        cookie_name = name,
        cookie_value = value,
        cookie_http_only = b httpOnly,
        cookie_persistent = not $ b session,
        cookie_host_only = sameSite == "STRICT",
        cookie_creation_time = fromMaybe now $ parseCookieExpires creation,
        cookie_last_access_time = fromMaybe now $ parseCookieExpires access
    }
readCookie _ _ = Nothing
b "TRUE" = True
b _ = False

writeCookies :: FilePath -> CookieJar -> Bool -> IO ()
writeCookies filepath cookies isSession = do
    B.writeFile filepath $ writeCookies' isSession $ destroyCookieJar cookies
writeCookies' :: Bool -> [Cookie] -> B.ByteString
writeCookies' isSession = C.unlines . map writeCookie' . filter shouldSaveCookie
    where
        shouldSaveCookie | isSession = cookie_persistent
            | otherwise = const True
writeCookie' :: Cookie -> B.ByteString
writeCookie' Cookie {..} = C.intercalate "\t" [
    cookie_domain, "TRUE", cookie_path, b' cookie_secure_only,
    formatCookieExpires cookie_expiry_time, cookie_name, cookie_value,
    b' cookie_http_only, b' $ not cookie_persistent,
    if cookie_host_only then "STRICT" else "LAX", "MEDIUM",
    formatCookieExpires cookie_creation_time,
    formatCookieExpires cookie_last_access_time]
b' True = "TRUE"
b' False = "FALSE"
