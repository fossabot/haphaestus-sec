module Network.URI.Types(Application(..)) where

import Network.URI

data Application = Application {
    name :: String,
    icon :: URI,
    description :: String,
    appId :: String -- internal
}
