module NicoLang.Types where

import Data.Text (Text)

-- | Same as Show, but codomain of showT is Text
class ShowT a where
  showT :: a -> Text
