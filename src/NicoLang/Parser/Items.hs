{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Define about the types used in around the parser
module NicoLang.Parser.Items where

import Brainhack.Parser.Items (BrainfuckToken (..), BrainfuckOperator (..))
import Data.List (lookup)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T

newtype NicoToken = NicoToken { unNicoToken :: Text }
  deriving (Eq, IsString)

instance BrainfuckToken NicoToken where
  forwardToken   = "笑顔届ける矢澤にこにこ！"
  backwordToken  = "だめだめだめっ！"
  incrToken      = "にっこにっこにー"
  decrToken      = "にこにーって覚えてラブニコ！"
  outputToken    = "ぴょんぴょんぴょんっ！"
  inputToken     = "あなたのハートににこにこにー！"
  loopBeginToken = "にこにーはみんなのもの！"
  loopEndToken   = "ｷﾓﾁﾜﾙｲ"
  toText         = unNicoToken
  fromText       = NicoToken
  toOperator     = flip lookup [ ("笑顔届ける矢澤にこにこ！", ForwardOp)
                               , ("だめだめだめっ！", BackwardOp)
                               , ("にっこにっこにー", IncrOp)
                               , ("にこにーって覚えてラブニコ！", DecrOp)
                               , ("ぴょんぴょんぴょんっ！", OutputOp)
                               , ("あなたのハートににこにこにー！", InputOp)
                               , ("にこにーはみんなのもの！", LoopBeginOp)
                               , ("ｷﾓﾁﾜﾙｲ", LoopEndOp)
                               ]

-- | Convert to the brainf*ck code for the debug
instance Show NicoToken where
  show "笑顔届ける矢澤にこにこ！"       = ">"
  show "だめだめだめっ！"               = "<"
  show "にっこにっこにー"               = "+"
  show "にこにーって覚えてラブニコ！"   = "-"
  show "ぴょんぴょんぴょんっ！"         = "."
  show "あなたのハートににこにこにー！" = ","
  show "にこにーはみんなのもの！"       = "["
  show "ｷﾓﾁﾜﾙｲ"                         = "]"
  show (NicoToken x)                    = T.unpack x
