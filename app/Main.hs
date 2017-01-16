-- | The entry point of the program
module Main where

import CmdOptions (NicoRunOptions(nicoRunTargetSourceFile, nicoRunDebug, nicoRunShowResultMemory), nicoRunOptions)
import Control.Monad (when, mapM_)
import NicoLang.Evaluator (emptyMachine, eval, runNicoState)
import NicoLang.Parser (parse)
import System.Console.CmdArgs (cmdArgs)
import qualified Data.Text as T


-- | The entry point of the program
main :: IO ()
main = do
  options <- cmdArgs nicoRunOptions
  case nicoRunTargetSourceFile options of
    Nothing       -> error "Please specify the source code"
    Just nicoFile -> do
      nicoCode <- T.pack <$> readFile nicoFile
      case parse nicoCode of
        Left  e -> error $ "Caught the error: " ++ e
        Right a -> do
          ((mem, logs), _) <- flip runNicoState emptyMachine $ eval a
          putStr "\n"
          when (nicoRunDebug options) $ do
            putStr "\n"
            mapM_ putStrLn logs
          when (nicoRunShowResultMemory options) $ do
            putStr "\n"
            print mem
