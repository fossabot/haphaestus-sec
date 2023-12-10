module Graphics.Text.Font.Choose.CharSet where

import Data.IntSet (IntSet, union)
import qualified Data.IntSet as IntSet
import Graphics.Text.Font.Choose.Result (throwNull, throwFalse)
import System.IO.Unsafe (unsafeInterleaveIO)

import Data.Word (Word32)
import Foreign.Ptr
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr)
import Control.Exception (bracket)
import Foreign.Storable (peek)
import Control.Monad (forM)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import GHC.Base (unsafeChr)
import Data.Char (ord, isHexDigit)
import Numeric (readHex)

-- | An FcCharSet is a set of Unicode chars.
type CharSet = IntSet

parseChar :: String -> Int
parseChar str | ((x, _):_) <- readHex str = toEnum x
replaceWild :: Char -> String -> String
replaceWild ch ('?':rest) = ch:replaceWild ch rest
replaceWild ch (c:cs) = c:replaceWild ch cs
replaceWild _ "" = ""
parseWild :: Char -> String -> Int
parseWild ch str = parseChar $ replaceWild ch str
-- | Utility for parsing "unicode-range" @font-face property.
parseCharSet :: String -> Maybe CharSet
parseCharSet ('U':rest) = parseCharSet ('u':rest) -- lowercase initial "u"
parseCharSet ('u':'+':cs)
    | (start@(_:_), '-':ends) <- span isHexDigit cs,
        (end@(_:_), rest) <- span isHexDigit ends, Just set <- parseCharSet' rest =
            Just $ union set $ IntSet.fromList [parseChar start..parseChar end]
    | (codepoint@(_:_), rest) <- span isHexDigit cs, Just set <- parseCharSet' rest =
        Just $ flip IntSet.insert set $ parseChar codepoint
    | (codepoint@(_:_), rest) <- span (\c -> isHexDigit c || c == '?') cs,
        Just set <- parseCharSet' rest =
            Just $ IntSet.union set $ IntSet.fromList [
                parseWild '0' codepoint..parseWild 'f' codepoint]
parseCharSet _ = Nothing
parseCharSet' :: String -> Maybe CharSet
parseCharSet' (',':rest) = parseCharSet rest
parseCharSet' "" = Just IntSet.empty
parseCharSet' _ = Nothing

------
--- Low-level
------

data CharSet'
type CharSet_ = Ptr CharSet'

withNewCharSet :: (CharSet_ -> IO a) -> IO a
withNewCharSet cb = bracket (throwNull <$> fcCharSetCreate) fcCharSetDestroy cb
foreign import ccall "FcCharSetCreate" fcCharSetCreate :: IO CharSet_
foreign import ccall "FcCharSetDestroy" fcCharSetDestroy :: CharSet_ -> IO ()

withCharSet :: CharSet -> (CharSet_ -> IO a) -> IO a
withCharSet chars cb = withNewCharSet $ \chars' -> do
    forM (IntSet.elems chars) $ \ch' ->
        throwFalse <$> (fcCharSetAddChar chars' $ fromIntegral ch')
    cb chars'
foreign import ccall "FcCharSetAddChar" fcCharSetAddChar :: CharSet_ -> Word32 -> IO Bool

thawCharSet :: CharSet_ -> IO CharSet
thawCharSet chars'
    | chars' == nullPtr = return IntSet.empty
    | otherwise = do
        iter' <- throwNull <$> fcCharSetIterCreate chars'
        iter <- newForeignPtr (fcCharSetIterDestroy) iter'
        x <- withForeignPtr iter fcCharSetIterStart
        let go x' | fcCharSetIterDone x' = return []
                | otherwise = unsafeInterleaveIO $ do
                    y <- withForeignPtr iter fcCharSetIterNext
                    xs <- go y
                    return (x':xs)
        ret <- go x
        return $ IntSet.fromList $ map (fromIntegral) ret
data CharSetIter'
type CharSetIter_ = Ptr CharSetIter'
foreign import ccall "my_FcCharSetIterCreate" fcCharSetIterCreate ::
    CharSet_ -> IO CharSetIter_
foreign import ccall "&my_FcCharSetIterDestroy" fcCharSetIterDestroy ::
    FunPtr (CharSetIter_ -> IO ())
foreign import ccall "my_FcCharSetIterStart" fcCharSetIterStart ::
    CharSetIter_ -> IO Word32
foreign import ccall "my_FcCharSetIterNext" fcCharSetIterNext ::
    CharSetIter_ -> IO Word32
foreign import ccall "my_FcCharSetIterDone" fcCharSetIterDone :: Word32 -> Bool

thawCharSet_ :: IO CharSet_ -> IO CharSet
thawCharSet_ cb = bracket (throwNull <$> cb) fcCharSetDestroy thawCharSet
thawCharSet' :: Ptr CharSet_ -> IO CharSet
thawCharSet' = thawCharSet_ . peek
