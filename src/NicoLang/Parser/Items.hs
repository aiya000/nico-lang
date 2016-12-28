{-# LANGUAGE OverloadedStrings #-}

-- | Define nico-lang's abstract syntax tree
module NicoLang.Parser.Items where

import Data.Text (Text)

-- | Same as brainf*ck's ><+-.,[]
data NicoOperation = NicoForward | NicoBackword | NicoAdd | NicoSub | NicoOutput | NicoInput | NicoLoopBegin | NicoLoopEnd

instance NicoOperation Show where
  show NicoForward

-- | The whole of nico-lang source code abstract
type NicoLangProgramAbstract = [NicoOperation]

-- | nico-lang's source code is Text
type NicoLangSourceCode = Text


-- | This text means the forward of nico-lang syntax
nicoForwardText :: Text
nicoForwardText = "笑顔届ける矢澤にこにこ！"

nicoBackwordText :: Text
nicoBackwordText = "だめだめだめっ！"

nicoAddText :: Text
nicoAddText = "にっこにっこにー"

nicoSubText :: Text
nicoSubText = "にこにーって覚えてラブニコ！"

nicoOutputText :: Text
nicoOutputText "=ぴょんぴょんぴょんっ！"

nicoInputText :: Text
nicoInputText = "あなたのハートににこにこにー！"

nicoLoopBeginText :: Text
nicoLoopBeginText "=にこにーはみんなのもの！"

nicoLoopEndText :: Text
nicoLoopEndText = "ｷﾓﾁﾜﾙｲ"
