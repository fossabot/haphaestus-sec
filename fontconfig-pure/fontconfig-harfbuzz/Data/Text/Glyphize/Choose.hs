module Data.Text.Glyphize.Choose where

import Data.Text.Glyphize (Font, createFace, createFontWithOptions,
                          Variation, FontOptions (..), defaultFontOptions)
import Graphics.Text.Font.Choose (Pattern, getValue0, getValue', Value(..),
                                normalizePattern, )
import qualified Data.ByteString as B

import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromMaybe)

-- Warning: file-read sideeffect.
pattern2hbfont :: Pattern -> [Variation] -> Font
pattern2hbfont pat variations = createFontWithOptions options face
  where
    bytes = unsafePerformIO $ B.readFile $ getValue0 "file" pat
    face = createFace bytes $ toEnum $ fromMaybe 0 $ getValue' "index" pat
    options = foldl value2opt defaultFontOptions $ normalizePattern pat

    value2opt opts ("slant", (_, ValueInt x):_) = opts {
        optionSynthSlant = Just $ realToFrac x
      }
    value2opt opts ("fontvariations", _:_) = opts {optionVariations = variations}
    value2opt opts ("size", (_, ValueDouble x):_) = opts {optionPtEm = Just $ realToFrac x}
    value2opt opts ("pixelsize", (_, ValueDouble x):_) = opts {
        optionPPEm = Just (toEnum $ fromEnum x, toEnum $ fromEnum x)
      }
    value2opt opts _ = opts
