{-# LANGUAGE CPP #-}
-- | Module holding localized error messages to be presented as a response.
--
-- To localize error messages provided by HURL, provide your translations between
-- "BEGIN LOCALIZATION" & "END LOCALIZATION" in this file.
--
-- The lines are formatted:
--    trans ("LANG":_) (KEY) = "TRANSLATION"
-- with uppercase indicating the bits you fill in.
--
-- Translations between #if WITH_HTTP_URI & #endif are specific to HTTP error handling.
module Network.URI.Messages (trans, Errors(..)) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Control.Exception (Exception)

trans _ (RawXML markup) = markup
--- BEGIN LOCALIZATION
("en":_) `trans` UnsupportedScheme scheme = "Unsupported protocol " ++ scheme
("en":_) `trans` UnsupportedMIME mime = "Unsupported filetype " ++ mime
("en":_) `trans` RequiresInstall mime appsMarkup =
    "<h1>Please install a compatible app to open <code>" ++ linkType ++
        "</code> links</h1>\n" ++ appsMarkup
  where linkType = fromMaybe mime $ stripPrefix "x-scheme-handler/" mime
("en":_) `trans` OpenedWith app = "Opened in " ++ app
("en":_) `trans` ReadFailed msg = "Failed to read file: " ++ msg
("en":_) `trans` MalformedResponse why = "Invalid response! " ++ why
("en":_) `trans` ExcessiveRedirects = "Too many redirects!"
("en":_) `trans` GeminiError '1' '1' label =
    "<form><label>" ++ label ++ "<input type=password></form>" 
("en":_) `trans` GeminiError '1' _ label = "<form><label>" ++ label ++ "<input></form>"
("en":_) `trans` GeminiError '4' '1' _ = "Site unavailable!"
("en":_) `trans` GeminiError '4' '2' _ = "Program error!"
("en":_) `trans` GeminiError '4' '3' _ = "Proxy error!"
("en":_) `trans` GeminiError '4' '4' timeout =
    "Site busy! Please reload after at least " ++ timeout ++ " seconds"
("en":_) `trans` GeminiError '5' '1' _ = "Page not found! Try the <a href='/'>homepage</a>."
("en":_) `trans` GeminiError '5' '2' _ = "Page deleted! Try the <a href='/'>homepage</a>."
("en":_) `trans` GeminiError '5' '3' _ = "Contacted wrong server!"
("en":_) `trans` GeminiError '5' '9' _ = "Malformed request, my bad!"
("en":_) `trans` GeminiError '6' '1' _ = "<form><label>Authentication required" ++
    "<-argo-authenticate error='Unauthorized account!'></-argo-authenticate></form>"
("en":_) `trans` GeminiError '6' '2' _ = "<form><label>Authentication required" ++
    "<-argo-authenticate error='Invalid account!'></-argo-authenticate></form>"
("en":_) `trans` GeminiError '6' _ _ = "<form><label>Authentication required" ++
    "<-argo-authenticate></-argo-authenticate></form>"
("en":_) `trans` GeminiError _ _ error = error
("en":_) `trans` HTTPStatus 400 _ = "I sent a bad request, according to this site."
("en":_) `trans` HTTPStatus 401 _ = "Authentication required!" -- FIXME: Support HTTP Basic Auth.
("en":_) `trans` HTTPStatus 402 _ = "Payment required!"
("en":_) `trans` HTTPStatus 403 _ = "Access denied!"
("en":_) `trans` HTTPStatus 404 _ = "Page not found! Try the <a href='/'>homepage</a>."
("en":_) `trans` HTTPStatus 405 _ = "Bad webform for this destination webaddress! " ++
    "<em>Method not allowed</em>."
("en":_) `trans` HTTPStatus 406 _ = "No representation available for given criteria!"
("en":_) `trans` HTTPStatus 407 _ = "Authentication into proxyserver required!"
("en":_) `trans` HTTPStatus 408 _ = "The site took too long to connect! <em>(HTTP 408)</em>"
("en":_) `trans` HTTPStatus 409 _ = "Request is based on outdated state!"
("en":_) `trans` HTTPStatus 410 _ = "Page deleted! Try the <a href='/'>homepage</a>."
("en":_) `trans` HTTPStatus 411 _ = "I sent a bad request, according to this site." ++
    "<em>(Missing <code>Content-Length</code> header)</em>"
("en":_) `trans` HTTPStatus 412 _ = "Webpage doesn't meet our preconditions."
("en":_) `trans` HTTPStatus 413 _ = "Payload too large, please upload a smaller file!"
("en":_) `trans` HTTPStatus 414 _ = "Web address is too long for the site!"
("en":_) `trans` HTTPStatus 415 _ = "No representation available for supported filetypes!"
("en":_) `trans` HTTPStatus 416 _ = "Invalid byte-range of requested resource!"
("en":_) `trans` HTTPStatus 417 _ = "Site cannot satisfy our stated expectations!"
("en":_) `trans` HTTPStatus 418 _ = unlines [
    "<p>I'm a little teapot<br/>",
    "Short and stout<br/>",
    "Here is my handle<br/>",
    "And here is my spout.</p>>",
    "<p>When I get all steamed up<br/>",
    "Hear me shout<br/>",
    "<q>Tip me over<br/>",
    "And pour me out!</q></p>"
  ]
("en":_) `trans` HTTPStatus 421 _ = "Contacted wrong server!"
("en":_) `trans` HTTPStatus 422 _ = "Invalid <strong>WebDAV</strong> request!"
("en":_) `trans` HTTPStatus 423 _ = "<strong>WebDAV</strong> resource is locked!"
("en":_) `trans` HTTPStatus 424 _ = "Failed due to previous failure!"
("en":_) `trans` HTTPStatus 425 _ = "Site requires stronger security on our request!"
("en":_) `trans` HTTPStatus 426 _ = "Site requires newer networking-protocols!"
("en":_) `trans` HTTPStatus 428 _ = "Site requires additional protection to avoid loosing changes!"
("en":_) `trans` HTTPStatus 429 _ = "We sent this site too many requests for it to cope with!"
("en":_) `trans` HTTPStatus 431 _ = "I sent more auxiliary data than this site can cope with!"
("en":_) `trans` HTTPStatus 451 _ = "Requested page cannot legally be provided!"

("en":_) `trans` HTTPStatus 500 _ = "The site experienced an error generating this webpage. " ++
    "<em>The webmasters have probably already been automatically notified.</em>"
("en":_) `trans` HTTPStatus 501 _ =
    "Bad webform for this destination webaddress! <em>Method not implemented</em>."
("en":_) `trans` HTTPStatus 502 _ = "Proxyserver got a malformed response!"
("en":_) `trans` HTTPStatus 503 _ = "The site is not available right now!"
("en":_) `trans` HTTPStatus 504 _ = "The site took too long to respond! <em>(Behind proxy)</em>"
("en":_) `trans` HTTPStatus 505 _ = "The site does not speak the language as me! " ++
    "<em>(Unsupported HTTP version)</em>"
("en":_) `trans` HTTPStatus 506 _ = "The site is misconfigured!"
("en":_) `trans` HTTPStatus 507 _ = "Insufficient <strong>WebDAV</strong> storage!"
("en":_) `trans` HTTPStatus 508 _ = "<strong>WebDAV</strong> loop detected!"
("en":_) `trans` HTTPStatus 510 _ = "Further request extensions required!"
("en":_) `trans` HTTPStatus 511 _ = "Authentication into network required!"
("en":_) `trans` HTTPStatus _ error = error -- Fallback
("en":_) `trans` OtherException error = "Internal Exception <pre><code>" ++ error ++ "</code></pre>"
("en":_) `trans` InvalidUrl url message =
    "Invalid web address <code>" ++ url ++ "</code>: <em>" ++ message ++ "</em>"
("en":_) `trans` ProxyError msg code code' msg' = unlines [
    "<h1>Proxy failed to forward request!<h1>",
    "<p>" ++ show code ++ " " ++ msg ++ "</p>",
    "<p>" ++ show code' ++ " " ++ msg' ++ "</p>"
  ]
("en":_) `trans` InvalidRequest why =
    "Attempted to send invalid auxiliary data: <em>" ++ why ++ "</em>"
("en":_) `trans` InsecureUnestablished =
    "Attempted to send or recieve data before establishing secure connection!"
("en":_) `trans` InsecureCertificate why = unlines [
    "<h1>The site failed to prove it is who it says it is!</h1>",
    "<p>" ++ why ++ "</p>",
    "<p><a href=action:history/back>Leave Insecure Site</a> | ",
        "<a href=action:novalidate>Accept Imposter Risk &amp; Continue</a></p>"
  ]
("en":_) `trans` InsecureTerminated why = "Secure session disconnected! <em>" ++ why ++ "</em>"
trans ("en":_) InsecureCertificateUnknownCA = "The authority vouching for it is unknown to me!"
trans ("en":_) InsecureCertificateUnknown =
    "The cryptographic certificate it has sent us to prove its identity instead " ++
    "belongs to someone else!"
trans ("en":_) InsecureCertificateRevoked =
    "The cryptographic certificate it has sent us to prove its identity has been revoked!"
trans ("en":_) InsecureCertificateExpired =
    "The cryptographic certificate it has sent us to prove its identity has expired!"
trans ("en":_) InsecureCertificateUnsupported =
    "It has sent us a cryptographic certificate to prove its identity I failed to make sense of."
("en":_) `trans` HandshakePacketUnparsed why = "Invalid security packet: <em>" ++ why ++ "</em>"
("en":_) `trans` HandshakePacketUnexpected a b = unlines [
    "<p>Invalid security packet: <em>" ++ a ++ "</em></p>",
    "<p>" ++ b ++ "</p>"
  ]
("en":_) `trans` HandshakePacketInvalid why = "Invalid security packet: <em>" ++ why ++ "</em>"
trans ("en":_) HandshakeEOF = "Secure session disconnected!"
("en":_) `trans` HandshakePolicy why = "Invalid handshake policy: <em>" ++ why ++ "</em>"
("en":_) `trans` HandshakeMisc why =
    "Failed to establish secure connection! <em>" ++ why ++ "</em>"
trans ("en":_) HandshakeError = "Failed to negotiate security parameters!"
trans ("en":_) HandshakeClosed = "Secure session disconnected!"
("en":_) `trans` FailedConnect why = "Failed to open connection to the site: <em>" ++ why ++ "</em>"
trans ("en":_) TimeoutConnection = "The site took too long to connect!"
trans ("en":_) TimeoutResponse = "The site took too long to respond!"
--- END LOCALIZATION

trans (_:locales) err = trans locales err
trans [] err = show err

data Errors = UnsupportedScheme String | UnsupportedMIME String | RequiresInstall String String
    | OpenedWith String | ReadFailed String | RawXML String | MalformedResponse String
    | ExcessiveRedirects | HTTPStatus Int String | GeminiError Char Char String
    | OtherException String | InvalidUrl String String | ProxyError String Int Int String
    | InvalidRequest String
    | InsecureUnestablished | InsecureCertificate String | InsecureTerminated String
    | InsecureCertificateUnknownCA | InsecureCertificateUnknown | InsecureCertificateRevoked
    | InsecureCertificateExpired | InsecureCertificateUnsupported
    | HandshakePacketUnparsed String | HandshakePacketUnexpected String String
    | HandshakePacketInvalid String
    | HandshakeEOF | HandshakePolicy String | HandshakeMisc String | HandshakeError | HandshakeClosed
    | FailedConnect String | TimeoutConnection | TimeoutResponse deriving (Show)

instance Exception Errors
