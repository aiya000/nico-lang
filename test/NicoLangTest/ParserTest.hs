{-# LANGUAGE OverloadedStrings #-}

module NicoLangTest.ParserTest where

import Control.Exception.Safe (SomeException)
import Data.Text (Text)
import NicoLang.Parser.Items
import NicoLang.Types
import Test.Tasty
import Test.Tasty.HUnit
import qualified Brainhack.Parser as Parser
import qualified Data.Text as T


simplySourceCode :: Text
simplySourceCode = "笑顔届ける矢澤にこにこ！にっこにっこにーにっこにっこにーにこにーって覚えてラブニコ！にこにーって覚えてラブニコ！だめだめだめっ！あなたのハートににこにこにー！ぴょんぴょんぴょんっ！"

simplySourceCodeAbstract :: NicoLangProgram
simplySourceCodeAbstract = [NicoForward, NicoIncr, NicoIncr, NicoDecr, NicoDecr, NicoBackword, NicoInput, NicoOutput]

sourceCode :: Text
sourceCode = "笑顔届ける矢澤にこにこ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにこにーはみんなのもの！だめだめだめっ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにー笑顔届ける矢澤にこにこ！にこにーって覚えてラブニコ！ｷﾓﾁﾜﾙｲだめだめだめっ！ぴょんぴょんぴょんっ！笑顔届ける矢澤にこにこ！笑顔届ける矢澤にこにこ！にっこにっこにーにっこにっこにーにっこにっこにーにこにーはみんなのもの！だめだめだめっ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにー笑顔届ける矢澤にこにこ！にこにーって覚えてラブニコ！ｷﾓﾁﾜﾙｲだめだめだめっ！ぴょんぴょんぴょんっ！笑顔届ける矢澤にこにこ！笑顔届ける矢澤にこにこ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにこにーはみんなのもの！だめだめだめっ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにー笑顔届ける矢澤にこにこ！にこにーって覚えてラブニコ！ｷﾓﾁﾜﾙｲだめだめだめっ！ぴょんぴょんぴょんっ！笑顔届ける矢澤にこにこ！笑顔届ける矢澤にこにこ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにこにーはみんなのもの！だめだめだめっ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにー笑顔届ける矢澤にこにこ！にこにーって覚えてラブニコ！ｷﾓﾁﾜﾙｲだめだめだめっ！ぴょんぴょんぴょんっ！笑顔届ける矢澤にこにこ！笑顔届ける矢澤にこにこ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにこにーはみんなのもの！だめだめだめっ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにー笑顔届ける矢澤にこにこ！にこにーって覚えてラブニコ！ｷﾓﾁﾜﾙｲだめだめだめっ！にっこにっこにーぴょんぴょんぴょんっ！笑顔届ける矢澤にこにこ！"


test :: TestTree
test =
  testGroup "Parser test"
    [ testCase "Parse from source code text to nico-lang abstract" $
        case (Parser.parse simplySourceCode :: Either SomeException NicoLangProgram) of
          Left _  -> assertFailure "Parsing is fail"
          Right x -> x @?= simplySourceCodeAbstract
    , testCase "Parse result can be restored by ShowT" $
        case (Parser.parse sourceCode :: Either SomeException NicoLangProgram) of
          Left _  -> assertFailure "Parsing is fail"
          Right x -> (T.concat . map showT $ x) @?= sourceCode
    ]
