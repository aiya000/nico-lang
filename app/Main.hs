-- | The entry point of the program
module Main where

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import NicoLang.Parser.Items
import qualified Data.Attoparsec.Text as P
import qualified NicoLang.Parser as NicoParser


-- | The entry point of the program
main :: IO ()
main = do
  --TODO: implement some options
  nicoFile <- head <$> getArgs
  nicoCode <- T.pack <$> readFile nicoFile
  let nicoAbstract = NicoParser.parse nicoCode
  --TODO: Do evaluate
  case nicoAbstract of
    Left e  -> putStrLn $ "Caught the error: " ++ e
    Right a -> print a
