module Graphics.Text.Font.Choose.LangSet (LangSet, defaultLangs, langs,
    langSetCompare, langNormalize, langCharSet,
    LangSet_, withLangSet, thawLangSet) where

import Data.Set (Set)
import qualified Data.Set as Set
import Graphics.Text.Font.Choose.Strings (thawStrSet, thawStrSet_, StrSet_)
import Graphics.Text.Font.Choose.CharSet (thawCharSet, CharSet_, CharSet)
import Graphics.Text.Font.Choose.Result (throwNull, throwFalse)

import Foreign.Ptr (Ptr)
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.Marshal.Alloc (free)
import Control.Exception (bracket)
import Control.Monad (forM)
import System.IO.Unsafe (unsafePerformIO)

-- | An `LangSet` is a set of language names (each of which include language and
-- an optional territory). They are used when selecting fonts to indicate which
-- languages the fonts need to support. Each font is marked, using language
-- orthography information built into fontconfig, with the set of supported languages.
type LangSet = Set String

-- | Returns a string set of the default languages according to the environment
-- variables on the system. This function looks for them in order of FC_LANG,
-- LC_ALL, LC_CTYPE and LANG then. If there are no valid values in those
-- environment variables, "en" will be set as fallback.
defaultLangs :: IO LangSet
defaultLangs = thawStrSet =<< fcGetDefaultLangs
foreign import ccall "FcGetDefaultLangs" fcGetDefaultLangs :: IO StrSet_

-- | Returns a string set of all known languages.
langs :: LangSet
langs = unsafePerformIO $ thawStrSet_ $ fcGetLangs
foreign import ccall "FcGetLangs" fcGetLangs :: IO StrSet_

-- | Result of language comparisons.
data LangResult = SameLang | DifferentTerritory | DifferentLang
    deriving (Enum, Eq, Read, Show)
-- | `langSetCompare` compares language coverage for ls_a and ls_b.
-- If they share any language and territory pair, returns `SameLang`.
-- If they share a language but differ in which territory that language is for,
-- this function returns `DifferentTerritory`.
-- If they share no languages in common, this function returns `DifferentLang`.
langSetCompare :: LangSet -> LangSet -> LangResult
langSetCompare a b = unsafePerformIO $ withLangSet a $ \a' -> withLangSet b $ \b' ->
    (toEnum <$> fcLangSetCompare a' b')
foreign import ccall "FcLangSetCompare" fcLangSetCompare ::
    LangSet_ -> LangSet_ -> IO Int

-- | `langSetContains` returns FcTrue if ls_a contains every language in ls_b.
-- ls_a will 'contain' a language from ls_b if ls_a has exactly the language,
-- or either the language or ls_a has no territory.
langSetContains :: LangSet -> LangSet -> Bool
langSetContains a b = unsafePerformIO $ withLangSet a $ \a' -> withLangSet b $
    fcLangSetContains a'
foreign import ccall "FcLangSetContains" fcLangSetContains ::
    LangSet_ -> LangSet_ -> IO Bool

-- | FcLangSetHasLang checks whether ls supports lang.
-- If ls has a matching language and territory pair, this function returns
-- `SameLang`. If ls has a matching language but differs in which territory
-- that language is for, this function returns `DifferentTerritory`. If ls has
-- no matching language, this function returns `DifferentLang`.
langSetHasLang :: LangSet -> String -> LangResult
langSetHasLang a b = unsafePerformIO $ withLangSet a $ \a' -> withCString b $ \b' ->
    (toEnum <$> fcLangSetHasLang a' b')
foreign import ccall "FcLangSetHasLang" fcLangSetHasLang :: LangSet_ -> CString -> IO Int

-- | Returns a string to make lang suitable on FontConfig.
langNormalize :: String -> String
langNormalize "" = ""
langNormalize lang = unsafePerformIO $ withCString lang (peekCString_ . fcLangNormalize)
foreign import ccall "FcLangNormalize" fcLangNormalize :: CString -> CString
peekCString_ str' = do
    str <- peekCString $ throwNull str'
    free str'
    return str

-- | Returns the FcCharMap for a language.
langCharSet :: String -> CharSet
langCharSet lang = unsafePerformIO $
    withCString lang (thawCharSet . throwNull . fcLangGetCharSet)
foreign import ccall "FcLangGetCharSet" fcLangGetCharSet :: CString -> CharSet_

------
--- Low-level
------

data LangSet'
type LangSet_ = Ptr LangSet'

withNewLangSet :: (LangSet_ -> IO a) -> IO a
withNewLangSet = bracket (throwNull <$> fcLangSetCreate) fcLangSetDestroy
foreign import ccall "FcLangSetCreate" fcLangSetCreate :: IO LangSet_
foreign import ccall "FcLangSetDestroy" fcLangSetDestroy :: LangSet_ -> IO ()

withLangSet :: LangSet -> (LangSet_ -> IO a) -> IO a
withLangSet langs cb = withNewLangSet $ \langs' -> do
    forM (Set.elems langs) $ flip withCString $ \lang' ->
        throwFalse <$> fcLangSetAdd langs' lang'
    cb langs'
foreign import ccall "FcLangSetAdd" fcLangSetAdd :: LangSet_ -> CString -> IO Bool

thawLangSet :: LangSet_ -> IO LangSet
thawLangSet = thawStrSet_ . fcLangSetGetLangs
foreign import ccall "FcLangSetGetLangs" fcLangSetGetLangs :: LangSet_ -> IO StrSet_
