{-# LANGUAGE OverloadedStrings #-}

-- | Define nico-lang's abstract syntax tree
module NicoLang.Parser.Items where

import Data.Text (Text)
import NicoLang.Types (ShowT, showT)
import qualified Data.Text as T

--TODO: Rename NicoAdd to NicoIncr
--TODO: Rename NicoSub to NicoDecr
-- | Same as brainf*ck's ><+-.,[]
data NicoOperation = NicoForward | NicoBackword | NicoAdd | NicoSub | NicoOutput | NicoInput | NicoLoopBegin | NicoLoopEnd
  deriving (Eq)

-- | Convert to the brainf*ck code for the debug
instance Show NicoOperation where
  show NicoForward   = ">"
  show NicoBackword  = "<"
  show NicoAdd       = "+"
  show NicoSub       = "-"
  show NicoOutput    = "."
  show NicoInput     = ","
  show NicoLoopBegin = "["
  show NicoLoopEnd   = "]"

-- | NicoOperation can be restored to the source code by show T
instance ShowT NicoOperation where
  showT NicoForward   = nicoForwardText
  showT NicoBackword  = nicoBackwordText
  showT NicoAdd       = nicoAddText
  showT NicoSub       = nicoSubText
  showT NicoOutput    = nicoOutputText
  showT NicoInput     = nicoInputText
  showT NicoLoopBegin = nicoLoopBeginText
  showT NicoLoopEnd   = nicoLoopEndText

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
