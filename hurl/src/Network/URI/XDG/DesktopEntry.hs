module Network.URI.XDG.DesktopEntry(launchApp, launchApp', desktop2app) where

import Data.Maybe (fromMaybe, catMaybes, isJust)
import Data.List (isInfixOf)
import Control.Exception (catch)
import System.Environment (lookupEnv)
import Control.Monad (forM)
import System.Directory (doesFileExist)
import System.FilePath

import Network.URI
import System.Process (spawnCommand)

import Network.URI.XDG.Ini
import Network.URI.XDG.MimeApps (split, fromMaybe')
import Network.URI.Types (Application(..))

launchApp' :: [String] -- ^ The locale to use
             -> URI -- ^ The URI to have it open.
             -> String -- ^ The .desktop ID
             -> IO (Either String Bool) -- ^ The localized name of the application or whether it expects a local path.
launchApp' locales uri desktopID = do
    app <- readDesktopID desktopID
    let grp = "desktop entry"
    let name = fromMaybe desktopID $ iniLookupLocalized locales grp "name" app
    case (iniLookup grp "type" app, iniLookup grp "exec" app) of
        (Just "Application", Just exec) | uriScheme uri /= "file:" && (isInfixOf "%f" exec || isInfixOf "%F" exec) ->
            return $ Right True
        (Just "Application", Just exec) ->
            catch (execApp uri exec name app) execFailed
        _ -> return $ Right False

launchApp :: [String] -- ^ The locale to use
             -> URI -- ^ The URI to have it open.
             -> String -- ^ The .desktop ID
             -> IO (Maybe String) -- ^ The localized name of the application
launchApp a b c = leftToMaybe <$> launchApp' a b c

readDesktopID desktopID = do
    dirs <- lookupEnv "XDG_DATA_DIRS"
    let dirs' = split ':' $ fromMaybe' "/usr/local/share/:/usr/share/" dirs
    filepaths <- forM (filter (/= "") dirs') $ \dir -> do
        exists <- doesFileExist (dir </> "applications" </> desktopID)
        if exists then
            return $ Just (dir </> "applications" </> desktopID)
        else
            return Nothing -- TODO? Handle cases where - = subdirectory path?
    case catMaybes filepaths of
        (filepath:_) -> do
            source <- readFile filepath
            let metadata = (" ", ["filename", filepath]) -- Used by %k macro
            return (parseIni source)
        [] -> return []

-- Capitals usually means supports multiple arguments,
-- but HURL doesn't support making use of that.
macros uri@URI {uriScheme="file:", uriPath=f} ('%':'f':cmd) x = esc f ++ macros uri cmd x
macros uri@URI {uriScheme="file:", uriPath=f} ('%':'F':cmd) x = esc f ++ macros uri cmd x
macros uri ('%':'u':cmd) x = esc uri ++ macros uri cmd x
macros uri ('%':'U':cmd) x = esc uri ++ macros uri cmd x
macros uri ('%':'i':cmd) (app, name)
    | Just icon <- iniLookup "desktop entry" "icon" app =
        "--icon " ++ esc icon ++ macros uri cmd (app, name)
    | otherwise = macros uri cmd (app, name)
macros uri ('%':'c':cmd) (app, name) = esc name ++ macros uri cmd (app, name)
macros uri ('%':'k':cmd) (app, name)
    | Just file <- iniLookup " " "filename" app = esc file ++ macros uri cmd (app, name)
    | otherwise = macros uri cmd (app, name)
macros uri ('%':'%':cmd) x = '%' : macros uri cmd x
macros uri (c:cmd) x = c : macros uri cmd x
macros _ [] _ = []

esc txt = '\'' : esc' (show txt)
esc' ('\'':cs) = '\\' : '\'' : esc' cs
esc' (c:cs) = c : esc' cs
esc' [] = "'"

execApp :: URI -> String -> String -> INI -> IO (Either String a)
execApp uri exec name app = do
    spawnCommand $ macros uri exec (app, name)
    return $ Left name

execFailed :: IOError -> IO (Either a Bool)
execFailed _ = return $ Right False

desktop2app :: [String] -> String -> IO (Maybe Application)
desktop2app locales desktopId = do
    app <- readDesktopID desktopId
    let grp = "desktop entry"
    let localized key = iniLookupLocalized locales grp key app
    let isApp = iniLookup grp "type" app == Just "Application" && isJust (iniLookup grp "exec" app)
    return $ if isApp then Just $ Application {
        name = fromMaybe desktopId $ localized "name",
        description = fromMaybe "" $ localized "comment",
        icon = case localized "icon" of
            Just icon -> URI "xdg-icon:" Nothing icon "" ""
            Nothing -> URI "about:" Nothing "blank" "" "",
        appId = desktopId
    } else Nothing

--- Utils

leftToMaybe (Left a) = Just a
leftToMaybe (Right _) = Nothing
