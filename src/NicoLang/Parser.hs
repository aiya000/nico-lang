{-# LANGUAGE OverloadedStrings #-}

-- | For generating nico-lang abstract syntax list
module NicoLang.Parser
  ( parse
  ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser)
import Data.Maybe (fromJust)
import Data.Text (Text)
import NicoLang.Parser.Items
import qualified Data.Attoparsec.Text as P
import qualified Data.Map.Lazy as M


-- |
-- Parse nico-lang source code.
-- If parsing is succeed, return result.
-- Otherwise, throw the exception.
parse :: NicoLangSourceCode -> Either String NicoLangProgram
parse = P.parseOnly $ codeParser

codeParser :: Parser NicoLangProgram
codeParser = do
  nicoText <- P.many' operationParser
  --TODO: Don't use fromJust
  let nicoOp = map (fromJust . flip M.lookup operationMap) $ nicoText
  return $ nicoOp

operationParser :: Parser Text
operationParser = P.try (P.string nicoForwardText)
              <|> P.try (P.string nicoBackwordText)
              <|> P.try (P.string nicoIncrText)
              <|> P.try (P.string nicoDecrText)
              <|> P.try (P.string nicoOutputText)
              <|> P.try (P.string nicoInputText)
              <|> P.try (P.string nicoLoopBeginText)
              <|> P.try (P.string nicoLoopEndText)
