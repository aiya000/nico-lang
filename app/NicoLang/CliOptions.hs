{-# LANGUAGE DeriveDataTypeable #-}

-- | The command line options
module NicoLang.CliOptions
  ( NicoRunOptions (..)
  , nicoRunOptions
  ) where

import Data.Data (Data, Typeable)
import System.Console.CmdArgs ((&=), name, help, explicit, program, summary, args)

data NicoRunOptions = NicoRunOptions
  { nicoRunTargetSourceFile :: Maybe FilePath
  , nicoRunTransToBF        :: Bool
  , nicoRunDebug            :: Bool
  , nicoRunShowResultMemory :: Bool
  } deriving (Data, Typeable)


nicoRunOptions :: NicoRunOptions
nicoRunOptions = NicoRunOptions
  { nicoRunTargetSourceFile = Nothing &= args
  , nicoRunTransToBF        = False &= name "trans-bf" &= help "Don't run, compile to the brainf*ck code" &= explicit
  , nicoRunDebug            = False &= name "debug" &= help "show the trace" &= explicit
  , nicoRunShowResultMemory = False &= name "show-result-memory" &= help "show the app result" &= explicit
  }
  &= program "nicorun"
  &= summary "Run the nico-lang program"
