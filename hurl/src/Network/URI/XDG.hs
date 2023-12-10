{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.URI.XDG(XDGConfig, loadXDGConfig, dispatchURIByMIME, queryHandlers', launchApp') where

import Network.URI (URI(..))
import Network.URI.Types
import Network.URI.Messages (Errors(..))
import Network.URI.XDG.DesktopEntry
import Network.URI.XDG.MimeApps
import Data.List (stripPrefix)
import Data.Maybe (catMaybes)

import qualified Text.XML as XML
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as Txt
import Network.URI.XDG.AppStream
import Network.URI.XDG.AppStreamOutput
import Control.Monad (forM)
import Network.URI

data XDGConfig = XDGConfig {
    components :: M.Map Text Component,
    componentsByMIME :: M.Map Text [Component],
    iconCache :: IconCache,
    handlers :: HandlersConfig,
    locales :: [String]
}

loadXDGConfig :: [String] -> IO XDGConfig
loadXDGConfig locales = do
    handlers <- loadHandlers
    components <- loadDatabase locales
    icons <- scanIconCache
    return $ XDGConfig components (buildMIMEIndex components) icons handlers locales

dispatchURIByMIME :: XDGConfig -> URI -> String -> IO Errors
dispatchURIByMIME config uri mime = do
    app <- queryHandlers (handlers config) mime `mapFirstM` launchApp (locales config) uri
    case app of
        Just app -> return $ OpenedWith app
        Nothing -> reportUnsupported config mime uri

reportUnsupported :: XDGConfig -> String -> URI -> IO Errors
reportUnsupported XDGConfig { components = comps } "x-scheme-handler/appstream" URI {
        uriAuthority = Just (URIAuth { uriRegName = ident })
    } | Just el <- xmlForID comps $ Txt.pack ident = return $ RawXML $ serializeXML el
    | otherwise = return $ UnsupportedScheme "appstream:" -- Could also do a 404...
reportUnsupported XDGConfig { iconCache = icondirs, componentsByMIME = index } mime _  = do
    let apps = appsForMIME icondirs index $ Txt.pack mime
    apps' <- forM apps $ \app -> do
        icons' <- testLocalIcons $ icons app
        return $ app {icons = icons'}
    return $ RequiresInstall mime $ outputApps apps'

mapFirstM :: [a] -> (a -> IO (Maybe b)) -> IO (Maybe b)
mapFirstM (x:xs) cb = do
    item <- cb x
    case item of
        Just _ -> return item
        Nothing -> mapFirstM xs cb
mapFirstM [] _ = return Nothing

queryHandlers' :: XDGConfig -> [String] -> String -> IO [Application]
queryHandlers' XDGConfig { handlers = config } locales mime =
    catMaybes <$> mapM (desktop2app locales) (queryHandlers config mime)
