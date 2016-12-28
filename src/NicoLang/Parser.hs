-- | For generating nico-lang AST
module NicoLang.Parser
  ( parse
  ) where

import Control.Applicative
import Data.Attoparsec.Text (Parser)
import Data.Text (Text)
import NicoLang.Parser.Items
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T


-- | Parse nico-lang source code
parse :: NicoLangSourceCode -> NicoLangSyntaxAbstract
parse = undefined

parseInput :: Parser a -> Text -> Either String a
parseInput p = P.parseOnly $ p <* P.endOfInput
