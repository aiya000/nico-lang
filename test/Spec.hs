import Test.Tasty
import qualified NicoLangTest.ParserTest as PT


main :: IO ()
main = defaultMain $
  testGroup "nico-lang test"
    [ PT.test
    ]
