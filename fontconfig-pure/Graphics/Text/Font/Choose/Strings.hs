module Graphics.Text.Font.Choose.Strings (StrSet, StrSet_, StrList, StrList_,
    withStrSet, withFilenameSet, thawStrSet, thawStrSet_,
    withStrList, thawStrList, thawStrList_) where

import Data.Set (Set)
import qualified Data.Set as Set
import Graphics.Text.Font.Choose.Result (throwNull, throwFalse)

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.String (CString, withCString, peekCString)
import Control.Exception (bracket)
import Control.Monad (forM)

-- | Set of strings, as exposed by other FreeType APIs.
type StrSet = Set String

data StrSet'
type StrSet_ = Ptr StrSet'

withNewStrSet :: (StrSet_ -> IO a) -> IO a
withNewStrSet = bracket (throwNull <$> fcStrSetCreate) fcStrSetDestroy
foreign import ccall "FcStrSetCreate" fcStrSetCreate :: IO StrSet_
foreign import ccall "FcStrSetDestroy" fcStrSetDestroy :: StrSet_ -> IO ()

withStrSet :: StrSet -> (StrSet_ -> IO a) -> IO a
withStrSet strs cb = withNewStrSet $ \strs' -> do
    forM (Set.elems strs) $ \str ->
        throwFalse <$> (withCString str $ fcStrSetAdd strs')
    cb strs'
foreign import ccall "FcStrSetAdd" fcStrSetAdd :: StrSet_ -> CString -> IO Bool

withFilenameSet :: StrSet -> (StrSet_ -> IO a) -> IO a
withFilenameSet paths cb = withNewStrSet $ \paths' -> do
    forM (Set.elems paths) $ \path ->
        throwFalse <$> (withCString path $ fcStrSetAddFilename paths')
    cb paths'
foreign import ccall "FcStrSetAddFilename" fcStrSetAddFilename ::
    StrSet_ -> CString -> IO Bool

thawStrSet :: StrSet_ -> IO StrSet
thawStrSet strs = Set.fromList <$> withStrList strs thawStrList

thawStrSet_ :: IO StrSet_ -> IO StrSet
thawStrSet_ cb = bracket (throwNull <$> cb) fcStrSetDestroy thawStrSet

------------

-- | Output string lists from FontConfig.
type StrList = [String]

data StrList'
type StrList_ = Ptr StrList'

withStrList :: StrSet_ -> (StrList_ -> IO a) -> IO a
withStrList strs = bracket (throwNull <$> fcStrListCreate strs) fcStrListDone
foreign import ccall "FcStrListCreate" fcStrListCreate :: StrSet_ -> IO StrList_
foreign import ccall "FcStrListDone" fcStrListDone :: StrList_ -> IO ()

thawStrList :: StrList_ -> IO StrList
thawStrList strs' = do
    fcStrListFirst strs'
    go
  where
    go = do
        item' <- fcStrListNext strs'
        if item' == nullPtr then return []
        else do
            item <- peekCString item'
            items <- go
            return (item : items)
foreign import ccall "FcStrListFirst" fcStrListFirst :: StrList_ -> IO ()
foreign import ccall "FcStrListNext" fcStrListNext :: StrList_ -> IO CString

thawStrList_ :: IO StrList_ -> IO StrList
thawStrList_ cb = bracket (throwNull <$> cb) fcStrListDone thawStrList
