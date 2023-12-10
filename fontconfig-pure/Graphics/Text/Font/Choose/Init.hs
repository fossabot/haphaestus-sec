module Graphics.Text.Font.Choose.Init (Config, initLoadConfig, initLoadConfigAndFonts,
    init, fini, reinit, bringUptoDate, version) where

import Prelude hiding (init)

import Graphics.Text.Font.Choose.Config

-- | Loads the default configuration file and returns the resulting configuration.
-- Does not load any font information.
initLoadConfig = fcInitLoadConfig >>= ptr2config
foreign import ccall "FcInitLoadConfig" fcInitLoadConfig :: IO Config_
-- | Loads the default configuration file and builds information about the available fonts.
-- Returns the resulting configuration.
initLoadConfigAndFonts = fcInitLoadConfigAndFonts >>= ptr2config
foreign import ccall "FcInitLoadConfigAndFonts" fcInitLoadConfigAndFonts :: IO Config_

-- | Initialize fontconfig library
-- Loads the default configuration file and the fonts referenced therein and
-- sets the default configuration to that result.
-- Returns whether this process succeeded or not.
-- If the default configuration has already been loaded,
-- this routine does nothing and returns FcTrue.
foreign import ccall "FcInit" init :: IO Bool
-- | Frees all data structures allocated by previous calls to fontconfig functions.
-- Fontconfig returns to an uninitialized state,
-- requiring a new call to one of the FcInit functions before any other
-- fontconfig function may be called.
foreign import ccall "FcFini" fini :: IO ()
-- | Forces the default configuration file to be reloaded
-- and resets the default configuration. Returns `False` if the configuration
-- cannot be reloaded (due to configuration file errors,
-- allocation failures or other issues) and leaves the existing configuration
-- unchanged. Otherwise returns True.
foreign import ccall "FcInitReinitialize" reinit :: IO Bool
-- | Checks the rescan interval in the default configuration,
-- checking the configuration if the interval has passed and
-- reloading the configuration if when any changes are detected.
-- Returns False if the configuration cannot be reloaded (see FcInitReinitialize).
-- Otherwise returns True.
foreign import ccall "FcInitBringUptoDate" bringUptoDate :: IO Bool

-- | Library version number
-- Returns the version number of the library.
foreign import ccall "FcGetVersion" version :: Int
