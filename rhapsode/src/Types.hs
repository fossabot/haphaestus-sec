module Types(CArray, Page(..), Application(..), RhapsodeCSS, readStrict) where

import SpeechStyle (SpeechStyle)
import Data.CSS.Preprocessor.Conditions (ConditionalStyles, conditionalStyles)
import Data.CSS.Preprocessor.Text (TextStyle)

import Network.URI.Fetch (Application(..), url)
import Network.URI.Fetch.XML (Page(..), readStrict)

import System.FilePath ((</>))
import Foreign.Ptr

type CArray a = Ptr a
type RhapsodeCSS = ConditionalStyles (TextStyle SpeechStyle)
