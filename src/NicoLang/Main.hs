-- | The entry point of the program
module NicoLang.Main (defaultMain) where

import Control.Exception.Safe (SomeException)
import Control.Monad (mapM_)
import NicoLang.CliOptions (NicoRunOptions(nicoRunTargetSourceFile, nicoRunTransToBF, nicoRunDebug, nicoRunShowResultMemory), nicoRunOptions)
import NicoLang.Evaluator (emptyMachine, eval, runNicoState)
import NicoLang.Parser (parse)
import System.Console.CmdArgs (cmdArgs)
import qualified Data.Text as T


-- | The entry point of the program
defaultMain :: IO ()
defaultMain = do
  options <- cmdArgs nicoRunOptions
  case nicoRunTargetSourceFile options of
    Nothing       -> error "Please specify the source code"
    Just nicoFile -> do
      nicoCode <- T.pack <$> readFile nicoFile
      case parse nicoCode of
        Left  e -> error $ "Caught the error: " ++ show (e :: SomeException)
        Right a -> if nicoRunTransToBF options
          then mapM_ (putStr . show) a >> putStrLn ""
          else do
            (results, _) <- flip runNicoState emptyMachine $ eval a
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
