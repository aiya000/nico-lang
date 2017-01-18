-- | The entry point of the program
module Main where

import CmdOptions (NicoRunOptions(nicoRunTargetSourceFile, nicoRunTransToBF, nicoRunDebug, nicoRunShowResultMemory), nicoRunOptions)
import Control.Monad (mapM_)
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
        Right a -> if nicoRunTransToBF options
          then mapM_ (putStr . show) a >> putStrLn ""
          else do
            (results, _) <- flip runNicoState emptyMachine $ eval a
            branch results options
  where
    branch results@(_, logs) opts | nicoRunDebug opts = do
      putStrLn ""
      mapM_ putStrLn logs
      branch results (opts { nicoRunDebug = False })

    branch results@(mem, _) opts | nicoRunShowResultMemory opts = do
      putStrLn ""
      print mem
      branch results (opts { nicoRunShowResultMemory = False })

    -- The end of branches
    branch _ _ = return ()
