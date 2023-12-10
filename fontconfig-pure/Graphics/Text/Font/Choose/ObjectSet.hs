module Graphics.Text.Font.Choose.ObjectSet where

import Foreign.Ptr (Ptr)
import Foreign.C.String (CString, withCString)

import Control.Monad (forM)
import Control.Exception (bracket)
import Graphics.Text.Font.Choose.Result (throwFalse, throwNull)

-- | An `ObjectSet` holds a list of pattern property names;
-- it is used to indicate which properties are to be returned in the patterns
-- from `FontList`.
type ObjectSet = [String]

------
--- LowLevel
------
data ObjectSet'
type ObjectSet_ = Ptr ObjectSet'

withObjectSet :: ObjectSet -> (ObjectSet_ -> IO a) -> IO a
withObjectSet objs cb = withNewObjectSet $ \objs' -> do
    forM objs $ \obj -> withCString obj $ \obj' ->
        throwFalse <$> fcObjectSetAdd objs' obj'
    cb objs'
foreign import ccall "FcObjectSetAdd" fcObjectSetAdd ::
    ObjectSet_ -> CString -> IO Bool

withNewObjectSet :: (ObjectSet_ -> IO a) -> IO a
withNewObjectSet = bracket (throwNull <$> fcObjectSetCreate) fcObjectSetDestroy
foreign import ccall "FcObjectSetCreate" fcObjectSetCreate :: IO ObjectSet_
foreign import ccall "FcObjectSetDestroy" fcObjectSetDestroy :: ObjectSet_ -> IO ()
