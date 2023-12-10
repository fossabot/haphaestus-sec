{-# LANGUAGE OverloadedStrings #-}

-- | Handles server-specified text decoding.
module Network.URI.Charset(resolveCharset, resolveCharset', convertCharset, charsets) where
import           Data.Text (Text)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           Data.Text.Encoding
import           Debug.Trace (trace)
import           Data.List (intercalate)

-- | If the MIMEtype specifies a charset parameter, apply it.
resolveCharset :: [String] -- ^ The MIMEtype, split by ';'
    -> ByteString -- ^ The bytes received from the server
    -> (String, Either Text ByteString) -- ^ The MIMEtype (minus parameters) & possibly decoded text, to be returned from protocol handlers.
resolveCharset (mime:('c':'h':'a':'r':'s':'e':'t':'=':charset):params) response =
    (parameterizedMIME mime params, Left $ convertCharset charset $ B.toStrict response)
resolveCharset (mime:param:params) response =
    resolveCharset (parameterizedMIME mime [param]:params) response
resolveCharset [mime] response = (mime, Right $ response)
-- NOTE I can't localize this error string because resolveCharset doesn't know the locale.
--      I don't think this is worth fixing, because hitting this indicates the server is badly misbehaving.
resolveCharset [] response = ("text/x-error\t", Left "Filetype unspecified")

parameterizedMIME mime params = mime ++ ";" ++ intercalate ";" params

-- | As per `resolveCharset`, but also returns given URI (or other type).
resolveCharset' :: a -> [String] -> ByteString -> (a, String, Either Text ByteString)
resolveCharset' a mimes resp = let (mime, resp') = resolveCharset mimes resp in (a, mime, resp')

-- | Decodes bytes according to a charset identified by it's IANA-assigned name(s).
convertCharset "iso-8859-1" = decodeLatin1
convertCharset "latin1" = decodeLatin1
convertCharset "us-ascii" = decodeUtf8With replaceChar
convertCharset "utf-8" = decodeUtf8With replaceChar
convertCharset "utf-16be" = decodeUtf16BEWith replaceChar
convertCharset "utf-16le" = decodeUtf16LEWith replaceChar
convertCharset "utf-16" = decodeUtf16LEWith replaceChar
convertCharset "utf-32be" = decodeUtf32BEWith replaceChar
convertCharset "utf-32le" = decodeUtf32LEWith replaceChar
convertCharset "utf-32" = decodeUtf32LEWith replaceChar
convertCharset charset = -- FIXME Is this the best fallback for unsupported charsets?
    trace ("Unsupported text encoding" ++ charset) $ decodeUtf8With replaceChar

replaceChar _ _ = Just '�'

-- | Lists all charsets supported by convertCharset
charsets :: [Text]
charsets = ["iso-8859-1", "latin1", "us-ascii", "utf-8", "utf-16be", "utf-16le", "utf-16", "utf-32be", "utf-32le", "utf-32"]
