{-# LANGUAGE OverloadedStrings #-}

-- | Define nico-lang's abstract syntax tree
module NicoLang.Parser.Items where

import Data.Text (Text)
import qualified Data.Text as T

--TODO: Rename NicoAdd to NicoIncr
--TODO: Rename NicoSub to NicoDecr
-- | Same as brainf*ck's ><+-.,[]
data NicoOperation = NicoForward | NicoBackword | NicoAdd | NicoSub | NicoOutput | NicoInput | NicoLoopBegin | NicoLoopEnd
  deriving (Eq)

-- | NicoOperation can be restored to the source code by (Text.pack . show)
instance Show NicoOperation where
  show NicoForward   = T.unpack nicoForwardText
  show NicoBackword  = T.unpack nicoBackwordText
  show NicoAdd       = T.unpack nicoAddText
  show NicoSub       = T.unpack nicoSubText
  show NicoOutput    = T.unpack nicoOutputText
  show NicoInput     = T.unpack nicoInputText
  show NicoLoopBegin = T.unpack nicoLoopBeginText
  show NicoLoopEnd   = T.unpack nicoLoopEndText

-- | The whole of nico-lang source code abstract
type NicoLangSyntaxAbstract = [NicoOperation]

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
nicoOutputText = "ぴょんぴょんぴょんっ！"

nicoInputText :: Text
nicoInputText = "あなたのハートににこにこにー！"

nicoLoopBeginText :: Text
nicoLoopBeginText = "にこにーはみんなのもの！"

nicoLoopEndText :: Text
nicoLoopEndText = "ｷﾓﾁﾜﾙｲ"
