import Test.Tasty
import qualified NicoLangTest.EvalTest as ET
import qualified NicoLangTest.ParserTest as PT


main :: IO ()
main = defaultMain $
  testGroup "nico-lang test"
    [ PT.test
    , ET.test
    ]
