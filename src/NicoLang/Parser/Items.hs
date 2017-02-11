{-# LANGUAGE OverloadedStrings #-}

-- | Define about the types used in around the parser
module NicoLang.Parser.Items where

import Brainhack.Parser.Items (BrainfuckOperation (..), BrainfuckProgram)
import Data.Text (Text)
import NicoLang.Types (ShowT, showT)
import qualified Data.Map.Lazy as M

-- | The nico-lang's expression of the brainf*ck's ><+-.,[]
data NicoOperation = NicoForward    -- ^ >
                   | NicoBackword   -- ^ <
                   | NicoIncr       -- ^ +
                   | NicoDecr       -- ^ -
                   | NicoOutput     -- ^ .
                   | NicoInput      -- ^ ,
                   | NicoLoopBegin  -- ^ [
                   | NicoLoopEnd    -- ^ ]
  deriving (Eq)

instance BrainfuckOperation NicoOperation where
  forward   = NicoForward
  backword  = NicoBackword
  incr      = NicoIncr
  decr      = NicoDecr
  output    = NicoOutput
  input     = NicoInput
  loopBegin = NicoLoopBegin
  loopEnd   = NicoLoopEnd
  toToken   = showT
  fromToken = flip M.lookup $ M.fromList [ (nicoForwardText   , NicoForward)
                                         , (nicoBackwordText  , NicoBackword)
                                         , (nicoIncrText      , NicoIncr)
                                         , (nicoDecrText      , NicoDecr)
                                         , (nicoOutputText    , NicoOutput)
                                         , (nicoInputText     , NicoInput)
                                         , (nicoLoopBeginText , NicoLoopBegin)
                                         , (nicoLoopEndText   , NicoLoopEnd)
                                         ]


-- | Convert to the brainf*ck code for the debug
instance Show NicoOperation where
  show NicoForward   = ">"
  show NicoBackword  = "<"
  show NicoIncr      = "+"
  show NicoDecr      = "-"
  show NicoOutput    = "."
  show NicoInput     = ","
  show NicoLoopBegin = "["
  show NicoLoopEnd   = "]"

-- | NicoOperation can be restored to the source code by showT
instance ShowT NicoOperation where
  showT NicoForward   = nicoForwardText
  showT NicoBackword  = nicoBackwordText
  showT NicoIncr      = nicoIncrText
  showT NicoDecr      = nicoDecrText
  showT NicoOutput    = nicoOutputText
  showT NicoInput     = nicoInputText
  showT NicoLoopBegin = nicoLoopBeginText
  showT NicoLoopEnd   = nicoLoopEndText

-- | The whole of the nico-lang source code abstract
type NicoLangProgram = BrainfuckProgram NicoOperation


-- | This text means the forward of nico-lang syntax
nicoForwardText :: Text
nicoForwardText = "笑顔届ける矢澤にこにこ！"

nicoBackwordText :: Text
nicoBackwordText = "だめだめだめっ！"

nicoIncrText :: Text
nicoIncrText = "にっこにっこにー"

nicoDecrText :: Text
nicoDecrText = "にこにーって覚えてラブニコ！"

nicoOutputText :: Text
nicoOutputText = "ぴょんぴょんぴょんっ！"

nicoInputText :: Text
nicoInputText = "あなたのハートににこにこにー！"

nicoLoopBeginText :: Text
nicoLoopBeginText = "にこにーはみんなのもの！"

nicoLoopEndText :: Text
nicoLoopEndText = "ｷﾓﾁﾜﾙｲ"
