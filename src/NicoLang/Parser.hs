{-# LANGUAGE OverloadedStrings #-}

-- | Parse a text to an abstract program
module NicoLang.Parser
  ( parse
  ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser)
import Data.List (foldl1')
import Data.Maybe (fromJust)
import Data.Text (Text)
import NicoLang.Parser.Items
import qualified Data.Attoparsec.Text as P
import qualified Data.Map.Lazy as M
import qualified Data.Text as T


-- |
-- Parse nico-lang source code.
-- If parsing is succeed, return result.
-- Otherwise, throw the exception.
parse :: Text -> Either String NicoLangProgram
parse = P.parseOnly codeParser

codeParser :: Parser [NicoOperation]
codeParser = do
  tokens <- P.many' tokenParser
  let mayOperations = map (fromJust . flip M.lookup operationMap) tokens
  return mayOperations

tokenParser :: Parser Text
tokenParser = nextTokenParser [ nicoForwardText
                              , nicoBackwordText
                              , nicoIncrText
                              , nicoDecrText
                              , nicoOutputText
                              , nicoInputText
                              , nicoLoopBeginText
                              , nicoLoopEndText
                              ]
  where
    nextTokenParser :: [Text] -> Parser Text
    nextTokenParser tokens = do
      let heads       = map T.head tokens
          matchTokens = foldl1' (<|>) . map (P.try . P.string) $ tokens
      P.skipWhile (`notElem` heads)
      matchTokens <|> (P.anyChar >> nextTokenParser tokens)
