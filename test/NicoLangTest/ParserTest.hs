{-# LANGUAGE OverloadedStrings #-}

module NicoLangTest.ParserTest where

import NicoLang.Parser
import NicoLang.Parser.Items
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified NicoLang.Parser as NicoParser

simplySourceCode :: NicoLangSourceCode
simplySourceCode = "笑顔届ける矢澤にこにこ！にっこにっこにーにっこにっこにーにこにーって覚えてラブニコ！にこにーって覚えてラブニコ！だめだめだめっ！あなたのハートににこにこにー！ぴょんぴょんぴょんっ！"

simplySourceCodeAbstract :: NicoLangSyntaxAbstract
simplySourceCodeAbstract = [NicoForward, NicoAdd, NicoAdd, NicoSub, NicoSub, NicoBackword, NicoInput, NicoOutput]

sourceCode :: NicoLangSourceCode
sourceCode = "笑顔届ける矢澤にこにこ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにこにーはみんなのもの！だめだめだめっ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにー笑顔届ける矢澤にこにこ！にこにーって覚えてラブニコ！ｷﾓﾁﾜﾙｲだめだめだめっ！ぴょんぴょんぴょんっ！笑顔届ける矢澤にこにこ！笑顔届ける矢澤にこにこ！にっこにっこにーにっこにっこにーにっこにっこにーにこにーはみんなのもの！だめだめだめっ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにー笑顔届ける矢澤にこにこ！にこにーって覚えてラブニコ！ｷﾓﾁﾜﾙｲだめだめだめっ！ぴょんぴょんぴょんっ！笑顔届ける矢澤にこにこ！笑顔届ける矢澤にこにこ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにこにーはみんなのもの！だめだめだめっ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにー笑顔届ける矢澤にこにこ！にこにーって覚えてラブニコ！ｷﾓﾁﾜﾙｲだめだめだめっ！ぴょんぴょんぴょんっ！笑顔届ける矢澤にこにこ！笑顔届ける矢澤にこにこ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにこにーはみんなのもの！だめだめだめっ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにー笑顔届ける矢澤にこにこ！にこにーって覚えてラブニコ！ｷﾓﾁﾜﾙｲだめだめだめっ！ぴょんぴょんぴょんっ！笑顔届ける矢澤にこにこ！笑顔届ける矢澤にこにこ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにこにーはみんなのもの！だめだめだめっ！にっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにーにっこにっこにー笑顔届ける矢澤にこにこ！にこにーって覚えてラブニコ！ｷﾓﾁﾜﾙｲだめだめだめっ！にっこにっこにーぴょんぴょんぴょんっ！笑顔届ける矢澤にこにこ！"


test :: [TestTree]
test =
  [ testProperty "Parse from source code text to nico-lang abstract" $
      NicoParser.parse simplySourceCode @?= simplySourceCodeAbstract
  , testProperty "show . parse = id" $
      (show . NicoParser.parse $ sourceCode) @?= sourceCode
  ]
