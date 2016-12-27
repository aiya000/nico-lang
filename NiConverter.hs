{-# LANGUAGE OverloadedStrings #-}

-- A converter tool of brainf*ck code to nico-lang code
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T


-- Read brainf*ck code from ./hello.bf .
-- Output nico-lang code to stdout,
-- its code means same as its brainf*ck code.
main :: IO ()
main = do
  bfCode <- T.pack <$> readFile "./hello.bf"
  let nicoCode = T.concatMap niconvert bfCode
  T.putStrLn nicoCode


-- Convert bf char to nico-lang text
niconvert :: Char -> Text
niconvert '>' = "笑顔届ける矢澤にこにこ！"
niconvert '<' = "だめだめだめっ！"
niconvert '+' = "にっこにっこにー"
niconvert '-' = "にこにーって覚えてラブニコ！"
niconvert '.' = "ぴょんぴょんぴょんっ！"
niconvert ',' = "あなたのハートににこにこにー！"
niconvert '[' = "にこにーはみんなのもの！"
niconvert ']' = "ｷﾓﾁﾜﾙｲ"
niconvert c   = T.singleton c
