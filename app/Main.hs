-- | The entry point of the program
module Main where

import Control.Monad (void)
import Control.Monad.Trans.State.Lazy (execStateT)
import System.Environment (getArgs)
import qualified Data.IntMap.Lazy as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified NicoLang.Evaluator as NicoEval
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
    Right a -> let initialState = (M.empty, 0)
               in void $ flip execStateT initialState $ NicoEval.eval a
