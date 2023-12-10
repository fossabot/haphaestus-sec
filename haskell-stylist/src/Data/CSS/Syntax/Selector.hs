-- | Parses CSS selectors
-- See `parseSelectors`
--
-- Backwards-compatibility module, this API has been moved out into "stylist-traits".
module Data.CSS.Syntax.Selector(
        Selector(..), SimpleSelector(..), PropertyTest(..), PropertyFunc(..),
        parseSelectors
    ) where

import Stylist.Parse.Selector
