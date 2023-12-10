module Network.URI.XDG.MimeApps(HandlersConfig, loadHandlers, queryHandlers, split, fromMaybe') where

import System.Environment (lookupEnv)
import Control.Monad (forM)
import Control.Exception (catch)
import System.FilePath
import Data.List (nub, (\\))
import System.Directory (getHomeDirectory)

import Network.URI.XDG.Ini

type HandlersConfig = [INI]

loadHandlers :: IO HandlersConfig
loadHandlers = do
    desktop <- lookupEnv "XDG_CURRENT_DESKTOP"
    dir0 <- mimeAppsDirs "XDG_CONFIG" ".config" "/etc/xdg"
    dir1 <- mimeAppsDirs "XDG_DATA" ".local/share" "/usr/local/share/:/usr/share/"
    let filepaths = mimeAppsFiles (dir0 ++ map (</> "applications") dir1) desktop
    files <- forM filepaths tryReadFile
    return $ map parseIni files

tryReadFile path = readFile path `catch` handler
  where
    handler :: IOError -> IO String
    handler e = return ""

mimeAppsDirs envPrefix defaultHome defaultDirs = do
    home <- lookupEnv (envPrefix ++ "_HOME")
    dirs <- lookupEnv (envPrefix ++ "_DIRS")
    cwd <- getHomeDirectory
    let home' = fromMaybe' (cwd </> defaultHome) home
    let dirs' = fromMaybe' defaultDirs dirs
    return (home' : filter (/= "") (split ':' dirs'))

mimeAppsFiles (dir:dirs) (Just desktop) = (dir </> desktop ++ "-mimeapps.list") :
    (dir </> "mimeapps.list") : (mimeAppsFiles dirs $ Just desktop)
mimeAppsFiles (dir:dirs) Nothing = (dir </> "mimeapps.list") : mimeAppsFiles dirs Nothing
mimeAppsFiles [] _ = []

---

queryHandlers :: HandlersConfig -> String -> [String]
-- TODO Expand MIMEtypes in reference to the local MIMEtypes database.
queryHandlers config mime = nub (
        queryHandlers' "default applications" config mime ++
        (queryHandlers' "added associations" config mime \\
        queryHandlers' "removed associations" config mime)
    )

queryHandlers' group (config:configs) mime =
    queryHandlers'' group config mime ++ queryHandlers' group configs mime
queryHandlers' group [] mime = []
queryHandlers'' group config mime
    | Just apps <- iniLookup group mime config = filter (/= "") $ split ';' apps
    | otherwise = []

---

fromMaybe' a (Just "") = a
fromMaybe' _ (Just a) = a
fromMaybe' a Nothing = a

split b (a:as) | a == b = [] : split b as
        | (head':tail') <- split b as = (a:head') : tail'
        | otherwise = [a:as]
split _ [] = [[]]
