{-# LANGUAGE OverloadedStrings #-}

module NicoLangTest.ParserTest where

import NicoLang.Parser.Items
import NicoLang.Types
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T
import qualified NicoLang.Parser as NicoParser


simplySourceCode :: NicoLangSourceCode
simplySourceCode = "笑顔届ける矢澤にこにこ！にっこにっこにーにっこにっこにーにこにーって覚えてラブニコ！にこにーって覚えてラブニコ！だめだめだめっ！あなたのハートににこにこにー！ぴょんぴょんぴょんっ！"

simplySourceCodeAbstract :: NicoLangAbstractSyntaxList
simplySourceCodeAbstract = [NicoForward, NicoIncr, NicoIncr, NicoDecr, NicoDecr, NicoBackword, NicoInput, NicoOutput]

sourceCode :: NicoLangSourceCode
sourceCode = "笑顔届ける矢澤にこにこ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにこにーはみんなのもの！だめだめだめっ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにー笑顔届ける矢澤にこにこ！にこにーって覚えてラブニコ！ｷﾓﾁﾜﾙｲだめだめだめっ！ぴょんぴょんぴょんっ！笑顔届ける矢澤にこにこ！笑顔届ける矢澤にこにこ！にっこにっこにーにっこにっこにーにっこにっこにーにこにーはみんなのもの！だめだめだめっ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにー笑顔届ける矢澤にこにこ！にこにーって覚えてラブニコ！ｷﾓﾁﾜﾙｲだめだめだめっ！ぴょんぴょんぴょんっ！笑顔届ける矢澤にこにこ！笑顔届ける矢澤にこにこ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにこにーはみんなのもの！だめだめだめっ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにー笑顔届ける矢澤にこにこ！にこにーって覚えてラブニコ！ｷﾓﾁﾜﾙｲだめだめだめっ！ぴょんぴょんぴょんっ！笑顔届ける矢澤にこにこ！笑顔届ける矢澤にこにこ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにこにーはみんなのもの！だめだめだめっ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにー笑顔届ける矢澤にこにこ！にこにーって覚えてラブニコ！ｷﾓﾁﾜﾙｲだめだめだめっ！ぴょんぴょんぴょんっ！笑顔届ける矢澤にこにこ！笑顔届ける矢澤にこにこ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにこにーはみんなのもの！だめだめだめっ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにー笑顔届ける矢澤にこにこ！にこにーって覚えてラブニコ！ｷﾓﾁﾜﾙｲだめだめだめっ！にっこにっこにーぴょんぴょんぴょんっ！笑顔届ける矢澤にこにこ！"


test :: TestTree
test =
  testGroup "Parser test"
    [ testCase "Parse from source code text to nico-lang abstract" $
        case NicoParser.parse simplySourceCode of
          Left _  -> assertFailure "Parsing is fail"
          Right x -> x @?= simplySourceCodeAbstract
    , testCase "Parse result can be restored by ShowT" $
        case NicoParser.parse sourceCode of
          Left _  -> assertFailure "Parsing is fail"
          Right x -> (T.concat . map showT $ x) @?= sourceCode
    ]
