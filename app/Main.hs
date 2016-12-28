-- | The entry point of the program
module Main where

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified NicoLang.Parser as NicoParser


-- | The entry point of the program
main :: IO ()
main = do
  --TODO: implement some options
  nicoFile <- head <$> getArgs
  nicoCode <- T.pack <$> readFile nicoFile
  let nicoAbstract = NicoParser.parse nicoCode
  --TODO: Do evaluate
  print nicoAbstract
