{-# LANGUAGE CPP #-}
module Network.MIME.Info(mimeInfo, MIME, Application(..)) where

#ifdef WITH_XDG
import Network.URI.XDG.MimeInfo (readMimeInfo)
#endif
import Network.URI.Locale (rfc2616Locale)
import Network.URI.Types (Application(..))

import qualified Data.Map as M
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)
import System.IO.Unsafe (unsafePerformIO)
import Data.Char (toLower)

type MIME = Application

{-# NOINLINE mimeInfo #-}
mimeInfo :: String -> MIME
mimeInfo = unsafePerformIO $ do
    (locales, _) <- rfc2616Locale
    cache <- newMVar M.empty :: IO (MVar (M.Map String MIME))
    return $ \mime -> unsafePerformIO $ do
        readMVar cache >>= inner mime locales cache
  where
    inner mime _ _ cache | Just val <- mime `M.lookup` cache = return val
    inner mime locales cache' cache = do
        ret <- readMimeInfo locales mime
        modifyMVar_ cache' $ return . M.insert mime ret
        return ret

#ifndef WITH_XDG
readMimeInfo _ mime = return Application {
        name = mime,
        icon = URI "about:" Nothing "invalid" "" "",
        description = "",
        appId = mime
    }
#endif
