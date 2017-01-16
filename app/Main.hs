-- | The entry point of the program
module Main where

import Control.Monad (void)
import NicoLang.Evaluator (emptyMachine, eval, runNicoState)
import NicoLang.Parser (parse)
import System.Environment (getArgs)
import qualified Data.Text as T


-- | The entry point of the program
main :: IO ()
main = do
  --TODO: implement some options
  nicoFile <- head <$> getArgs
  nicoCode <- T.pack <$> readFile nicoFile
  let nicoAbstract = parse nicoCode
  case nicoAbstract of
    Left  e -> putStrLn $ "Caught the error: " ++ e
    Right a -> void $ flip runNicoState emptyMachine $ eval a
