module Graphics.Text.Font.Choose.Config where

import Graphics.Text.Font.Choose.Strings
import Graphics.Text.Font.Choose.FontSet
import Graphics.Text.Font.Choose.CharSet
import Graphics.Text.Font.Choose.Pattern
import Graphics.Text.Font.Choose.ObjectSet

import Foreign.ForeignPtr
import Foreign.Ptr (Ptr, nullPtr, FunPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes, free)
import Foreign.Storable (Storable(..))
import Foreign.C.String (CString, peekCString, withCString)
import System.IO.Unsafe (unsafePerformIO)
import Data.Set (empty) -- For testing segfault source.

import Control.Exception (bracket)
import Graphics.Text.Font.Choose.Result (Word8, throwNull, throwFalse, throwPtr)

-- | System configuration regarding available fonts.
type Config = ForeignPtr Config'
data Config'
type Config_ = Ptr Config'

-- | Creates an empty configuration.
configCreate :: IO Config
configCreate = newForeignPtr fcConfigDestroy =<< throwNull <$> fcConfigCreate
foreign import ccall "FcConfigCreate" fcConfigCreate :: IO Config_
ptr2config = newForeignPtr fcConfigDestroy
foreign import ccall "&FcConfigDestroy" fcConfigDestroy :: FunPtr (Config_ -> IO ())

-- | Sets the current default configuration to config.
-- Implicitly calls `configBuildFonts` if necessary.
configSetCurrent :: Config -> IO ()
configSetCurrent config = throwFalse =<< (withForeignPtr config $ fcConfigSetCurrent)
foreign import ccall "FcConfigSetCurrent" fcConfigSetCurrent :: Config_ -> IO Bool

-- | Returns the current default configuration.
configGetCurrent :: IO Config
configGetCurrent = (throwNull <$> fcConfigReference nullPtr) >>=
    newForeignPtr fcConfigDestroy
foreign import ccall "FcConfigReference" fcConfigReference :: Config_ -> IO Config_

-- | Checks all of the files related to config and returns whether any of them
-- has been modified since the configuration was created.
configUptoDate :: Config -> IO Bool
configUptoDate config = withForeignPtr config $ fcConfigUptoDate
foreign import ccall "FcConfigUptoDate" fcConfigUptoDate :: Config_ -> IO Bool

-- | Return the current user's home directory, if it is available,
-- and if using it is enabled, and NULL otherwise. (See also `configEnableHome`).
configHome :: IO (Maybe String)
configHome = do
    ret <- fcConfigHome
    if ret == nullPtr then return Nothing else Just <$> peekCString ret
foreign import ccall "FcConfigHome" fcConfigHome :: IO CString

-- | If enable is `True`, then Fontconfig will use various files which are
-- specified relative to the user's home directory (using the ~ notation in
-- the configuration). When enable is `False`, then all use of the home
-- directory in these contexts will be disabled. The previous setting of
-- the value is returned.
foreign import ccall "FcConfigEnableHome" configEnableHome :: Bool -> IO Bool

-- | Returns the list of font directories specified in the configuration files
-- for config. Does not include any subdirectories.
configBuildFonts :: Config -> IO ()
configBuildFonts config = throwFalse =<< (withForeignPtr config $ fcConfigBuildFonts)
-- | Variant of `configBuildFonts` operating on the current configuration.
configBuildFonts' :: IO ()
configBuildFonts' = throwFalse =<< fcConfigBuildFonts nullPtr
foreign import ccall "FcConfigBuildFonts" fcConfigBuildFonts :: Config_ -> IO Bool

-- | Returns the list of font directories specified in the configuration files
-- for config. Does not include any subdirectories.
configGetConfigDirs :: Config -> IO StrList
configGetConfigDirs = configStrsFunc fcConfigGetConfigDirs
-- | Variant of `configGetConfigDirs` which operates on the current configuration.
configGetConfigDirs' :: IO StrList
configGetConfigDirs' = thawStrList =<< fcConfigGetConfigDirs nullPtr
foreign import ccall "FcConfigGetConfigDirs" fcConfigGetConfigDirs :: Config_ -> IO StrList_

-- | Returns the list of font directories in config.
-- This includes the configured font directories along with any directories
-- below those in the filesystem.
configGetFontDirs :: Config -> IO StrList
configGetFontDirs = configStrsFunc fcConfigGetFontDirs
-- | Variant of `configGetFontDirs` which operates on the current config.
configGetFontDirs' :: IO StrList
configGetFontDirs' = thawStrList_ $ fcConfigGetFontDirs nullPtr
foreign import ccall "FcConfigGetFontDirs" fcConfigGetFontDirs :: Config_ -> IO StrList_

-- | Returns the list of known configuration files used to generate config.
configGetConfigFiles :: Config -> IO StrList
configGetConfigFiles = configStrsFunc fcConfigGetConfigFiles
-- | Variant of `configGetConfigFiles` which operates upon current configuration.
configGetConfigFiles' :: IO StrList
configGetConfigFiles' = thawStrList_ $ fcConfigGetConfigFiles nullPtr
foreign import ccall "FcConfigGetConfigFiles" fcConfigGetConfigFiles :: Config_ -> IO StrList_

-- | Returns a string list containing all of the directories that fontconfig
-- will search when attempting to load a cache file for a font directory.
configGetCacheDirs :: Config -> IO StrList
configGetCacheDirs = configStrsFunc fcConfigGetCacheDirs
-- | Variant of `configGetCacheDirs` which operates upon current configuration.
configGetCacheDirs' :: IO StrList
configGetCacheDirs' = thawStrList_ $ fcConfigGetCacheDirs nullPtr
foreign import ccall "FcConfigGetCacheDirs" fcConfigGetCacheDirs :: Config_ -> IO StrList_

-- | Whether to operate upon system or application fontlists.
data SetName = SetSystem | SetApplication deriving (Enum, Eq, Show, Read)
-- | Returns one of the two sets of fonts from the configuration as specified
-- by set. This font set is owned by the library and must not be modified or
-- freed. If config is NULL, the current configuration is used.
-- This function isn't MT-safe.
configGetFonts :: Config -> SetName -> IO FontSet
configGetFonts config set = do
    ret <- withForeignPtr config $ flip fcConfigGetFonts $ fromEnum set
    thawFontSet $ throwNull ret
-- | Variant of `configGetFonts` which operates upon current configuration.
configGetFonts' :: SetName -> IO FontSet
configGetFonts' set = do
    ret <- fcConfigGetFonts nullPtr $ fromEnum set
    thawFontSet $ throwNull ret
foreign import ccall "FcConfigGetFonts" fcConfigGetFonts :: Config_ -> Int -> IO FontSet_

-- | Returns the interval between automatic checks of the configuration
-- (in seconds) specified in config. The configuration is checked during
-- a call to FcFontList when this interval has passed since the last check.
-- An interval setting of zero disables automatic checks.
configGetRescanInterval :: Config -> IO Int
configGetRescanInterval = flip withForeignPtr $ fcConfigGetRescanInterval
-- | Variant of `configGetRescanInterval` which operates upon current configuration.
configGetRescanInterval' :: IO Int
configGetRescanInterval' = fcConfigGetRescanInterval nullPtr
foreign import ccall "FcConfigGetRescanInterval" fcConfigGetRescanInterval ::
    Config_ -> IO Int

-- | Sets the rescan interval.
-- An interval setting of zero disables automatic checks.
configSetRescanInterval :: Config -> Int -> IO ()
configSetRescanInterval config val =
    throwFalse =<< (withForeignPtr config $ flip fcConfigSetRescanInterval val)
-- | Variant of `configSetRescanInterval` which operates upon current configuration.
configSetRescanInterval' :: Int -> IO ()
configSetRescanInterval' v = throwFalse =<< fcConfigSetRescanInterval nullPtr v
foreign import ccall "FcConfigSetRescanInterval" fcConfigSetRescanInterval ::
    Config_ -> Int -> IO Bool

-- | Adds an application-specific font to the configuration.
configAppFontAddFile :: Config -> String -> IO ()
configAppFontAddFile config file = throwFalse =<<
    (withForeignPtr config $ \config' -> withCString file $ \file' ->
        fcConfigAppFontAddFile config' file')
-- | Variant of `configAppFontAddFile` which operates upon current configuration.
configAppFontAddFile' :: String -> IO ()
configAppFontAddFile' file =
    throwFalse =<< (withCString file $ fcConfigAppFontAddFile nullPtr)
foreign import ccall "FcConfigAppFontAddFile" fcConfigAppFontAddFile ::
    Config_ -> CString -> IO Bool

-- | Scans the specified directory for fonts,
-- adding each one found to the application-specific set of fonts.
configAppFontAddDir :: Config -> String -> IO ()
configAppFontAddDir config file = throwFalse =<<
    (withForeignPtr config $ \config' -> withCString file $ fcConfigAppFontAddDir config')
-- | Variant of `configAppFontAddDir` which operates upon current configuration.
configAppFontAddDir' :: String -> IO ()
configAppFontAddDir' v = throwFalse =<< (withCString v $ fcConfigAppFontAddDir nullPtr)
foreign import ccall "FcConfigAppFontAddDir" fcConfigAppFontAddDir ::
    Config_ -> CString -> IO Bool

-- | Clears the set of application-specific fonts.
configAppFontClear :: Config -> IO ()
configAppFontClear config = throwFalse =<< withForeignPtr config fcConfigAppFontClear
-- | Variant of `configAppFontClear` which operates upon current configuration.
configAppFontClear' :: IO ()
configAppFontClear' = throwFalse =<< fcConfigAppFontClear nullPtr
foreign import ccall "FcConfigAppFontClear" fcConfigAppFontClear :: Config_ -> IO Bool

-- | What purpose does the given pattern serve?
data MatchKind = MatchPattern | MatchFont | MatchScan deriving Enum
-- | Performs the sequence of pattern modification operations,
-- if kind is `MatchPattern`, then those tagged as pattern operations are applied,
-- else if kind is `MatchFont`, those tagged as font operations are applied and
-- p_pat is used for <test> elements with target=pattern.
configSubstituteWithPat :: Config -> Pattern -> Pattern -> MatchKind -> Pattern
configSubstituteWithPat config p p_pat kind = unsafePerformIO $
    withForeignPtr config $ \config' -> withPattern p $ \p' ->
        withPattern p_pat $ \p_pat' -> do
            ok <- fcConfigSubstituteWithPat config' p' p_pat' $ fromEnum kind
            throwFalse ok
            thawPattern p'
-- | Variant of `configSubstituteWithPat` which operates upon current configuration.
configSubstituteWithPat' :: Pattern -> Pattern -> MatchKind -> Pattern
configSubstituteWithPat' p p_pat kind = unsafePerformIO $
    withPattern p $ \p' -> withPattern p_pat $ \p_pat' -> do
        ok <- fcConfigSubstituteWithPat nullPtr p' p_pat' $ fromEnum kind
        throwFalse ok
        thawPattern p'
foreign import ccall "FcConfigSubstituteWithPat" fcConfigSubstituteWithPat ::
    Config_ -> Pattern_ -> Pattern_ -> Int -> IO Bool

-- | Calls FcConfigSubstituteWithPat without setting p_pat.
configSubstitute :: Config -> Pattern -> MatchKind -> Pattern
configSubstitute config p kind = unsafePerformIO $
    withForeignPtr config $ \config' -> withPattern p $ \p' -> do
        ok <- fcConfigSubstitute config' p' $ fromEnum kind
        throwFalse ok
        thawPattern p'
-- | Variant `configSubstitute` which operates upon current configuration.
configSubstitute' :: Pattern -> MatchKind -> Pattern
configSubstitute' p kind = unsafePerformIO $ withPattern p $ \p' -> do
    ok <- fcConfigSubstitute nullPtr p' $ fromEnum kind
    throwFalse ok
    thawPattern p'
foreign import ccall "FcConfigSubstitute" fcConfigSubstitute ::
    Config_ -> Pattern_ -> Int -> IO Bool

-- | Finds the font in sets most closely matching pattern and returns
-- the result of `fontRenderPrepare` for that font and the provided pattern.
-- This function should be called only after `configSubstitute` and
-- `defaultSubstitute` have been called for p;
-- otherwise the results will not be correct.
fontMatch :: Config -> Pattern -> Maybe Pattern
fontMatch config pattern = unsafePerformIO $ withForeignPtr config $ \config' ->
    withPattern pattern $ \pattern' -> alloca $ \res' -> do
        ret <- fcFontMatch config' pattern' res'
        throwPtr res' $ thawPattern_ $ pure ret
-- | Variant of `fontMatch` which operates upon current configuration.
fontMatch' :: Pattern -> Maybe Pattern
fontMatch' pattern = unsafePerformIO $ withPattern pattern $ \pattern' -> alloca $ \res' -> do
    ret <- fcFontMatch nullPtr pattern' res'
    throwPtr res' $ thawPattern_ $ pure ret
foreign import ccall "FcFontMatch" fcFontMatch ::
    Config_ -> Pattern_ -> Ptr Word8 -> IO Pattern_

-- | Returns the list of fonts sorted by closeness to p. If trim is `True`,
-- elements in the list which don't include Unicode coverage not provided by
-- earlier elements in the list are elided. The union of Unicode coverage of
-- all of the fonts is returned in `snd`. This function should be called only
-- after `configSubstitute` and `defaultSubstitute` have been called for p;
-- otherwise the results will not be correct.
fontSort :: Config -> Pattern -> Bool -> Maybe (FontSet, CharSet)
fontSort config pattern trim = unsafePerformIO $ withForeignPtr config $ \config' ->
    withPattern pattern $ \pattern' -> alloca $ \csp' -> alloca $ \res' -> do
            ret <- fcFontSort config' pattern' trim csp' res'
            throwPtr res' $ do
                x <- thawFontSet_ $ pure ret
                y <- thawCharSet' csp'
                return (x, y)
-- | Variant of `fontSort` which operates upon current configuration.
fontSort' :: Pattern -> Bool -> Maybe (FontSet, CharSet)
fontSort' pattern trim = unsafePerformIO $ withPattern pattern $ \pattern' ->
    alloca $ \csp' -> alloca $ \res' -> do
        ret <- fcFontSort nullPtr pattern' trim csp' res'
        throwPtr res' $ do
            x <- thawFontSet_ $ pure ret
            y <- thawCharSet' csp'
            return (x, y)
foreign import ccall "FcFontSort" fcFontSort ::
    Config_ -> Pattern_ -> Bool -> Ptr CharSet_ -> Ptr Word8 -> IO FontSet_

-- | Creates a new pattern consisting of elements of font not appearing in pat,
-- elements of pat not appearing in font and the best matching value from pat
-- for elements appearing in both. The result is passed to
--`configSubstituteWithPat` with kind `matchFont` and then returned.
fontRenderPrepare :: Config -> Pattern -> Pattern -> Pattern
fontRenderPrepare config pat font = unsafePerformIO $ withForeignPtr config $ \config' ->
    withPattern pat $ \pat' -> withPattern font $ \font' -> do
        ret <- fcFontRenderPrepare config' pat' font'
        thawPattern_ $ pure $ throwNull ret
-- | Variant of `fontRenderPrepare` which operates upon current configuration.
fontRenderPrepare' :: Pattern -> Pattern -> Pattern
fontRenderPrepare' pat font = unsafePerformIO $ withPattern pat $ \pat' ->
    withPattern font $ \font' -> do
        ret <- fcFontRenderPrepare nullPtr pat' font'
        thawPattern_ $ pure ret
foreign import ccall "FcFontRenderPrepare" fcFontRenderPrepare ::
    Config_ -> Pattern_ -> Pattern_ -> IO Pattern_

-- | Selects fonts matching p, creates patterns from those fonts containing only
-- the objects in os and returns the set of unique such patterns.
fontList :: Config -> Pattern -> ObjectSet -> FontSet
fontList config p os = unsafePerformIO $ withForeignPtr config $ \config' ->
    withPattern p $ \p' -> withObjectSet os $ \os' -> do
        ret <- fcFontList config' p' os'
        thawFontSet_ $ pure ret
-- | Variant of `fontList` which operates upon current configuration.
fontList' :: Pattern -> ObjectSet -> FontSet
fontList' p os = unsafePerformIO $ withPattern p $ \p' -> withObjectSet os $ \os' -> do
    ret <- fcFontList nullPtr p' os'
    thawFontSet_ $ pure ret
foreign import ccall "FcFontList" fcFontList ::
    Config_ -> Pattern_ -> ObjectSet_ -> IO FontSet_

{-configGetFilename :: Config -> String -> String
configGetFilename config name = unsafePerformIO $ withForeignPtr config $ \config' ->
    withCString name $ \name' -> do
        ret <- fcConfigGetFilename config' name'
        peekCString' ret
configGetFilename' :: String -> String
configGetFilename' name = unsafePerformIO $ withCString name $ \name' -> do
    ret <- fcConfigGetFilename nullPtr name'
    peekCString' ret
foreign import ccall "FcConfigGetFilename" fcConfigGetFilename :: Config_ -> CString -> IO CString
peekCString' txt = bracket (pure $ throwNull txt) free peekCString-}

-- | Walks the configuration in 'file' and constructs the internal representation
-- in 'config'. Any include files referenced from within 'file' will be loaded
-- and parsed. If 'complain' is `False`, no warning will be displayed if 'file'
-- does not exist. Error and warning messages will be output to stderr. 
configParseAndLoad :: Config -> String -> Bool -> IO Bool
configParseAndLoad config name complain = withForeignPtr config $ \config' ->
    withCString name $ \name' -> fcConfigParseAndLoad config' name' complain
-- Variant of `configParseAndLoad` which operates upon current configuration.
configParseAndLoad' :: String -> Bool -> IO Bool
configParseAndLoad' name complain =
    withCString name $ \name' -> fcConfigParseAndLoad nullPtr name' complain
foreign import ccall "FcConfigParseAndLoad" fcConfigParseAndLoad ::
    Config_ -> CString -> Bool -> IO Bool

-- | Walks the configuration in 'memory' and constructs the internal representation
-- in 'config'. Any includes files referenced from within 'memory' will be loaded
-- and dparsed. If 'complain' is `False`, no warning will be displayed if 'file'
-- does not exist. Error and warning messages will be output to stderr.
configParseAndLoadFromMemory :: Config -> String -> Bool -> IO Bool
configParseAndLoadFromMemory config buffer complain = withForeignPtr config $ \config' ->
    withCString buffer $ \buffer' ->
        fcConfigParseAndLoadFromMemory config' buffer' complain
-- | Variant of `configParseAndLoadFromMemory` which operates upon current
-- configuration.
configParseAndLoadFromMemory' :: String -> Bool -> IO Bool
configParseAndLoadFromMemory' buffer complain = withCString buffer $ \buffer' ->
    fcConfigParseAndLoadFromMemory nullPtr buffer' complain
foreign import ccall "FcConfigParseAndLoadFromMemory" fcConfigParseAndLoadFromMemory ::
    Config_ -> CString -> Bool -> IO Bool

-- | Obtains the system root directory in 'config' if available.
-- All files (including file properties in patterns) obtained from this 'config'
-- are relative to this system root directory.
-- This function isn't MT-safe. 
configGetSysRoot :: Config -> IO String
configGetSysRoot = flip withForeignPtr $ \config' -> do
    ret <- fcConfigGetSysRoot config'
    peekCString $ throwNull ret
-- | Variant of `configGetSysRoot` which operates upon current configuration.
configGetSysRoot' :: IO String
configGetSysRoot' = (peekCString .throwNull) =<< fcConfigGetSysRoot nullPtr
foreign import ccall "FcConfigGetSysRoot" fcConfigGetSysRoot :: Config_ -> IO CString

-- | Set 'sysroot' as the system root directory. All file paths used or created
-- with this 'config' (including file properties in patterns) will be considered
-- or made relative to this 'sysroot'. This allows a host to generate caches for
-- targets at build time. This also allows a cache to be re-targeted to a
-- different base directory if 'configGetSysRoot' is used to resolve file paths.
-- When setting this on the current config this causes changing current config.
configSetSysRoot :: Config -> String -> IO ()
configSetSysRoot config val = withForeignPtr config $ \config' -> withCString val $
    fcConfigSetSysRoot config'
-- | Variant of `configSetSysRoot` which operates upon current configuration.
configSetSysRoot' :: String -> IO ()
configSetSysRoot' val = withCString val $ fcConfigSetSysRoot nullPtr
foreign import ccall "FcConfigSetSysRoot" fcConfigSetSysRoot :: Config_ -> CString -> IO ()

-- | Retrieves a list of all filepaths & descriptions for all fonts in this
-- configuration alongside whether each is enabled.
-- Not thread-safe.
configGetFileInfo :: Config -> IO [(FilePath, String, Bool)]
configGetFileInfo config =
  withForeignPtr config $ \config' -> allocaBytes configFileInfoIter'Size $ \iter' -> do
    fcConfigFileInfoIterInit config' iter'
    let readEnt = alloca $ \name' -> alloca $ \description' -> alloca $ \enabled' -> do {
        ok <- fcConfigFileInfoIterGet config' iter' name' description' enabled';
        if ok then do
            name <- peekCString =<< peek name'
            description <- peekCString =<< peek description'
            enabled <- peek enabled'
            return $ Just (name, description, enabled)
        else return Nothing}
    let go = do {
        ent' <- readEnt;
        case ent' of
            Just ent -> do
                ok <- fcConfigFileInfoIterNext config' iter'
                ents <- if ok then go else return []
                return (ent : ents)
            Nothing -> return []}
    go
-- | Variant `configGetFileInfo` which operates upon current configuration.
configGetFileInfo' :: IO [(FilePath, String, Bool)]
configGetFileInfo' = configGetCurrent >>= configGetFileInfo
data ConfigFileInfoIter'
foreign import ccall "size_ConfigFileInfoIter" configFileInfoIter'Size :: Int
type ConfigFileInfoIter_ = Ptr ConfigFileInfoIter'
foreign import ccall "FcConfigFileInfoIterInit" fcConfigFileInfoIterInit ::
    Config_ -> ConfigFileInfoIter_ -> IO ()
foreign import ccall "FcConfigFileInfoIterNext" fcConfigFileInfoIterNext ::
    Config_ -> ConfigFileInfoIter_ -> IO Bool
foreign import ccall "FcConfigFileInfoIterGet" fcConfigFileInfoIterGet ::
    Config_ -> ConfigFileInfoIter_ -> Ptr CString -> Ptr CString -> Ptr Bool -> IO Bool

------

configStrsFunc :: (Config_ -> IO StrList_) -> Config -> IO StrList
configStrsFunc cb config = thawStrList_ $ withForeignPtr config cb
