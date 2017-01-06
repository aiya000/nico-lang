-- | For executing nico-lang abstract syntax list
module NicoLang.Evaluator
  ( NicoMemory
  , NicoPointer
  , NicoMachine
  , emptyMachine
  , eval
  ) where

import Control.Monad.Trans.State.Lazy (StateT, get, put)
import Data.IntMap.Lazy (IntMap)
import NicoLang.Parser.Items
import qualified Data.IntMap.Lazy as M

-- | The virtual machine's memory
type NicoMemory = IntMap Int

-- | The state of current pointer
type NicoPointer = Int

-- | The virtual machine's state
type NicoMachine = (NicoMemory, NicoPointer)


-- | The initial state of NicoMachine
emptyMachine :: NicoMachine
emptyMachine = (M.empty, 0)


-- | Evaluate and execute NicoLangAbstractSyntaxList with the virtual machine state
eval :: NicoLangAbstractSyntaxList -> StateT NicoMachine IO NicoMemory
eval [] = do
  (mem, _) <- get
  return mem

eval (NicoForward:rest) = do
  (mem, p) <- get
  case M.lookup p mem of
    Nothing  -> put $ (M.insert p 1 mem, p)  -- 1 is `0 (initial state) + 1 (forward)`
    Just val -> let newVal = val + 1
                in put $ (M.insert p newVal mem, p)  -- Replace val to newVal on mem
  return mem

eval (NicoBackword:rest)  = undefined
eval (NicoIncr:rest)      = undefined
eval (NicoDecr:rest)      = undefined
eval (NicoOutput:rest)    = undefined
eval (NicoInput:rest)     = undefined
eval (NicoLoopBegin:rest) = undefined
eval (NicoLoopEnd:rest)   = undefined
