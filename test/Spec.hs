{-# LANGUAGE LambdaCase #-}

import Control.Monad (forM)
import Data.Bifunctor (second)
import Data.List (nub)
import Data.Tuple.Extra ((&&&))
import NicoLang.Evaluator (emptyMachine, eval, runNicoState)
import NicoLang.Parser (parse)
import System.EasyFile (getDirectoryContents)
import System.IO.Silently (capture_)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit ((@?=))
import Text.Printf (printf)
import qualified Data.Text as T
import qualified NicoLangTest.ParserTest as PT
import qualified Test.Tasty as Test (defaultMain, testGroup)
import qualified Test.Tasty.HUnit as Test (testCase, assertFailure)


main :: IO ()
main = do
  inOutTests <- getInOutTests
  Test.defaultMain $
    Test.testGroup "nico-lang test" $
      [ PT.test
      , inOutTests
      ]

getInOutTests :: IO TestTree
getInOutTests = do
  inOutPairs  <- map inOutFiles . testNames . filter (`notElem` [".", ".."]) <$> getDirectoryContents "test/in-out"
  inOutPairs' <- sequence . map (twiceMapM readFile) $ inOutPairs
  -- `init` removes the line break of the tail
  let inOutPairs'' = map (second init) inOutPairs'
  resultPairs <- forM inOutPairs'' $ firstMapM $ \source -> do
    case parse . T.pack $ source of
      Left  e -> return . Left $ "Parse error: " ++ show e
      Right a -> return . Right =<< (capture_ . flip runNicoState emptyMachine $ eval a)
  return $ Test.testGroup "in-out matching test" $
    flip map resultPairs $ \case
      (Left e, outData)       -> Test.testCase (show outData) $ Test.assertFailure e
      (Right inData, outData) -> Test.testCase (show outData) $ inData @?= outData
  where
    testNames :: [FilePath] -> [FilePath]
    testNames  = nub . map (takeWhile (/='.'))
    inOutFiles :: FilePath -> (FilePath, FilePath)
    inOutFiles = (printf "test/in-out/%s.nico") &&& (printf "test/in-out/%s.out")

twiceMapM :: Monad m => (a -> m b) -> (a, a) -> m (b, b)
twiceMapM f (x, y) = do
  x' <- f x
  y' <- f y
  return (x', y')

firstMapM :: Monad m => (a -> m b) -> (a, x) -> m (b, x)
firstMapM f (x, y) = do
  x' <- f x
  return (x', y)
