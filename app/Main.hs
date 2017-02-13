-- | The entry point of the program
module Main where

import Brainhack.Evaluator (emptyMachine, eval, runBrainState)
import Brainhack.Parser (parse)
import Control.Exception.Safe (SomeException)
import Control.Monad (mapM_)
import Data.Text (Text)
import NicoLang.CliOptions (NicoRunOptions(nicoRunTargetSourceFile, nicoRunTransToBF, nicoRunDebug, nicoRunShowResultMemory), nicoRunOptions)
import NicoLang.Parser.Items (NicoToken (NicoToken))
import System.Console.CmdArgs (cmdArgs)
import qualified Brainhack.Parser.Items as B
import qualified Data.Text as T


-- | The entry point of the program
main :: IO ()
main = do
  options <- cmdArgs nicoRunOptions
  case nicoRunTargetSourceFile options of
    Nothing       -> error "Please specify the source code"
    Just nicoFile -> do
      nicoCode <- NicoToken . T.pack <$> readFile nicoFile
      case parse nicoCode of
        Left  e -> error $ "Caught the error: " ++ show (e :: SomeException)
        Right a -> if nicoRunTransToBF options
          then mapM_ (putStr . show) a >> putStrLn ""
          else do
            (results, _) <- flip runBrainState emptyMachine $ eval a
            executeOptions results options
  where
    executeOptions results@(_, logs) opts | nicoRunDebug opts = do
      putStrLn ""
      mapM_ putStrLn logs
      executeOptions results (opts { nicoRunDebug = False })

    executeOptions results@(mem, _) opts | nicoRunShowResultMemory opts = do
      putStrLn ""
      print mem
      executeOptions results (opts { nicoRunShowResultMemory = False })

    -- The end of branches
    executeOptions _ _ = return ()
