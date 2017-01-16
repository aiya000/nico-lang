-- | The entry point of the program
module Main where

import Control.Monad (void)
import Control.Monad.Trans.State.Lazy (runStateT)
import NicoLang.Evaluator (emptyMachine, eval)
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
  case nicoAbstract of
    Left  e -> putStrLn $ "Caught the error: " ++ e
    Right a -> void $ flip runStateT emptyMachine $ eval a
