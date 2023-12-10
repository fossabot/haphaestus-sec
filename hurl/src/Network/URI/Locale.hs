-- | Internal module for retrieving languages to localize to.
-- Also provides decoupling layers between Network.URI.Messages & optional dependencies.
{-# LANGUAGE CPP #-}
module Network.URI.Locale(rfc2616Locale
#ifdef WITH_HTTP_URI
, transHttp
#endif
) where

import System.Environment (lookupEnv)
import Control.Monad (forM)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Char (toLower)

#ifdef WITH_HTTP_URI
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..))
import Control.Exception (displayException)
import Network.TLS (TLSException(..), TLSError(..), AlertDescription(..))
import Control.Exception.Base (fromException)
import Network.HTTP.Types (Status(..))

import Network.URI.Messages
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as Txt
import Text.Read (readMaybe)
#endif

--- This file is based on logic in GNOME's LibSoup & GLib.

-- | Returns the languages to which responses should be localized.
-- Retrieved from Gettext configuration & reformatted for use in the
-- HTTP Accept-Language request header.
rfc2616Locale :: IO ([String], [String])
rfc2616Locale = do
    locales <- forM ["LANGUAGE", "LC_ALL", "LC_MESSAGES", "LANG"] lookupEnv
    let posix = split ":" $ firstJust locales "en_US"
    let ietf = mapMaybe toRFC2616Lang posix
    return (explode ietf, explode posix)

toRFC2616Lang "C" = Nothing
toRFC2616Lang ('C':'.':_) = Nothing
toRFC2616Lang ('C':'@':_) = Nothing
toRFC2616Lang lang = case toRFC2616Lang' lang of
    "" -> Nothing
    lang' -> Just lang'

toRFC2616Lang' ('_':cs) = '-' : toRFC2616Lang' cs
toRFC2616Lang' ('.':_) = []
toRFC2616Lang' ('@':_) = []
toRFC2616Lang' (c:cs) = toLower c : toRFC2616Lang' cs
toRFC2616Lang' [] = []

-- Makes sure to include the raw languages, and not just localized variants.
extractLangs (locale:locales) | (lang:_) <- split "-_.@" locale = lang : extractLangs locales
extractLangs (_:locales) = extractLangs locales
extractLangs [] = []

explode locales = locales ++ [l | l <- extractLangs locales, l `notElem` locales]

firstJust (Just a:_) _ | a /= "" = a
firstJust (_:maybes) fallback = firstJust maybes fallback
firstJust [] fallback = fallback

split b (a:as) | a `elem` b = [] : split b as
        | (head':tail') <- split b as = (a:head') : tail'
        | otherwise = [a:as]
split _ [] = [[]]

--------
---- Decoupling Layer
--------
#ifdef WITH_HTTP_URI
transHttp trans' (InvalidUrlException url msg) = trans' $ InvalidUrl url msg
transHttp trans' (HttpExceptionRequest _ (TooManyRedirects _)) = trans' $ ExcessiveRedirects
transHttp trans' (HttpExceptionRequest _ ResponseTimeout) = trans' $ TimeoutResponse
transHttp trans' (HttpExceptionRequest _ ConnectionTimeout) = trans' $ TimeoutConnection
transHttp trans' (HttpExceptionRequest _ (ConnectionFailure err)) =
    trans' $ FailedConnect $ displayException err
transHttp trans' (HttpExceptionRequest _ (StatusCodeException _ code)) =
    trans' $ HTTPStatus (fromMaybe 500 $ readMaybe $ C8.unpack code) ""
transHttp trans' (HttpExceptionRequest _ OverlongHeaders) =
    trans' $ HTTPStatus 431 "Overlong Headers"
transHttp trans' (HttpExceptionRequest _ (InvalidStatusLine why)) =
    trans' $ MalformedResponse $ C8.unpack why
transHttp trans' (HttpExceptionRequest _ (InvalidHeader why)) =
    trans' $ MalformedResponse $ C8.unpack why
transHttp trans' (HttpExceptionRequest _ (InvalidRequestHeader why)) =
    trans' $ InvalidRequest $ C8.unpack why
transHttp trans' (HttpExceptionRequest _ (ProxyConnectException a b (Status code msg))) =
    trans' $ ProxyError (C8.unpack a) b code $ C8.unpack msg
-- NOTE: Minor details are unlocalized for now... Can always come back to this!
transHttp trans' (HttpExceptionRequest _ NoResponseDataReceived) =
    trans' $ MalformedResponse "Empty"
transHttp trans' (HttpExceptionRequest _ TlsNotSupported) =
    trans' $ HandshakeMisc "Unsupported"
transHttp trans' (HttpExceptionRequest _ (WrongRequestBodyStreamSize a b)) =
    trans' $ OtherException $ unlines ["Wrong request bodysize", show a, show b]
transHttp trans' (HttpExceptionRequest _ (ResponseBodyTooShort a b)) =
    trans' $ MalformedResponse ("Too short " ++ show a ++ '<' : show b)
transHttp trans' (HttpExceptionRequest _ InvalidChunkHeaders) =
    trans' $ MalformedResponse "Chunk headers"
transHttp trans' (HttpExceptionRequest _ IncompleteHeaders) =
    trans' $ MalformedResponse "Incomplete headers"
transHttp trans' (HttpExceptionRequest _ (InvalidDestinationHost why)) =
    trans' $ FailedConnect $ C8.unpack why
transHttp trans' (HttpExceptionRequest _ (HttpZlibException _)) =
    trans' $ MalformedResponse "ZLib compression"
transHttp trans' (HttpExceptionRequest _ ConnectionClosed) =
    trans' $ FailedConnect "already-closed"
transHttp trans' (HttpExceptionRequest _ (InvalidProxySettings why)) =
    trans' $ FailedConnect ("proxy (" ++ Txt.unpack why ++ ")")
transHttp trans' (HttpExceptionRequest _ (InvalidProxyEnvironmentVariable key value)) =
    trans' $ FailedConnect ("proxy (" ++ Txt.unpack key ++ '=' : Txt.unpack value ++ ")")
transHttp trans' (HttpExceptionRequest _ (InternalException e)) =
    trans' $ case fromException e of
        Just (Terminated _ why _) -> InsecureTerminated why
        Just (HandshakeFailed (Error_Misc msg)) -> HandshakeMisc msg
        Just (HandshakeFailed (Error_Protocol (_, _, CloseNotify))) -> HandshakeClosed
        Just (HandshakeFailed (Error_Protocol (_, _, HandshakeFailure))) -> HandshakeError
        Just (HandshakeFailed (Error_Protocol (_, _, BadCertificate))) -> InsecureCertificate ""
        Just (HandshakeFailed (Error_Protocol (_, _, UnsupportedCertificate))) ->
            InsecureCertificate $ trans' InsecureCertificateUnsupported
        Just (HandshakeFailed (Error_Protocol (_, _, CertificateExpired))) ->
            InsecureCertificate $ trans' InsecureCertificateExpired
        Just (HandshakeFailed (Error_Protocol (_, _, CertificateRevoked))) ->
            InsecureCertificate $ trans' InsecureCertificateRevoked
        Just (HandshakeFailed (Error_Protocol (_, _, CertificateUnknown))) ->
            InsecureCertificate $ trans' InsecureCertificateUnknown
        Just (HandshakeFailed (Error_Protocol (_, _, UnknownCa))) ->
            InsecureCertificate $ trans' InsecureCertificateUnknownCA
        Just (HandshakeFailed (Error_Protocol (why, _, _))) -> HandshakeMisc why
        Just (HandshakeFailed (Error_Certificate why)) -> InsecureCertificate why
        Just (HandshakeFailed (Error_HandshakePolicy why)) -> HandshakePolicy why
        Just (HandshakeFailed Error_EOF) -> HandshakeEOF
        Just (HandshakeFailed (Error_Packet why)) -> HandshakePacketInvalid why
        Just (HandshakeFailed (Error_Packet_unexpected a b)) -> HandshakePacketUnexpected a b
        Just (HandshakeFailed (Error_Packet_Parsing why)) -> HandshakePacketUnparsed why
        Just ConnectionNotEstablished -> InsecureUnestablished
        Nothing -> OtherException $ displayException e
#endif
