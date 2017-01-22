{-# LANGUAGE OverloadedStrings #-}


-- | Parse a text to an abstract program
module NicoLang.Parser
  ( parse
  ) where

import Control.Applicative ((<|>))
import Control.Exception.Safe (MonadCatch, throw)
import Control.Monad (mapM)
import Data.Attoparsec.Text (Parser)
import Data.List (foldl1')
import Data.Text (Text)
import NicoLang.Parser.Items
import qualified Data.Attoparsec.Text as P
import qualified Data.Map.Lazy as M
import qualified Data.Text as T


-- |
-- Parse nico-lang source code.
-- If parsing is succeed, return result.
-- Otherwise, throw the exception.
parse :: MonadCatch m => Text -> m NicoLangProgram
parse txt =
  case P.parseOnly codeParser txt of
    Left  e        -> throw $ NicoParserException e
    Right Nothing  -> throw $ NicoParserException tokenParseErrorMsg
    Right (Just a) -> return a
  where
    -- Throw this message if any looking up a token is failure
    tokenParseErrorMsg = "Fatal error:\n" ++
                         "An other than token is detected in the code parser\n" ++
                         "`tokenParser` should return the tokens only"

codeParser :: Parser (Maybe [NicoOperation])
codeParser = do
  tokens <- P.many' tokenParser
  let mayOperations = mapM (flip M.lookup operationMap) tokens
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
