{-# LANGUAGE OverloadedStrings #-}

-- | Parse a text to an abstract program
module NicoLang.Parser
  ( parse
  ) where

import Control.Applicative ((<|>))
import Control.Exception.Safe (MonadCatch, throw)
import Control.Monad (void, mapM)
import Data.Attoparsec.Text (Parser)
import Data.List (foldl1')
import Data.Text (Text)
import NicoLang.Parser.Items
import qualified Control.Applicative as P (many)
import qualified Data.Attoparsec.Combinator as P (lookAhead)
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

tokenTexts :: [Text]
tokenTexts = [ nicoForwardText
             , nicoBackwordText
             , nicoIncrText
             , nicoDecrText
             , nicoOutputText
             , nicoInputText
             , nicoLoopBeginText
             , nicoLoopEndText
             ]

-- Parse a code to [NicoOperation]
codeParser :: Parser (Maybe [NicoOperation])
codeParser = do
  --TODO: I think this procedure can be written oneline
  tokens <- tokensParser
  let mayOperations = mapM fromToken tokens
  return mayOperations

-- Parse a code to tokens
tokensParser :: Parser [Text]
tokensParser = do
  blocks <- P.many tokenBlockParser
  let tokens = concat blocks
  return tokens

-- A block means "あああ にっこにっこにーにこにーって覚えてラブニコ！ いいい" .
--                       |^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^|
-- And parse its text to [にっこにっこにー, にこにーって覚えてラブニコ！]
tokenBlockParser :: Parser [Text]
tokenBlockParser = do
  skipNonToken
  P.many tokenParser

-- Match any token
tokenParser :: Parser Text
tokenParser = foldl1' (<|>) . map (P.try . P.string) $ tokenTexts

-- Skip other than the token
-- (Seek to just before the token)
skipNonToken :: Parser ()
skipNonToken = do
  let heads = map T.head tokenTexts
  P.skipWhile (`notElem` heads)
  (void $ P.lookAhead tokenParser) <|> (P.anyChar >> skipNonToken)
