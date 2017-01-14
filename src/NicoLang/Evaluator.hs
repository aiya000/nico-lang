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
import Data.Stack (Stack, push, pop, runStack, stack)
import NicoLang.Parser.Items
import qualified Data.IntMap.Lazy as M

-- |
-- The virtual machine's memory,
-- The pairs of the memory address and the value.
type NicoMemory = IntMap Int

-- | The state of the virtual machine cell's current pointer
type NicoPointer = Int

-- | The virtual machine (The running program's state)
data NicoMachine = NicoMachine
  { nicoMemory            :: NicoMemory  -- ^ The state of the NicoProgram result
  , nicoPointer           :: NicoPointer  -- ^ The NicoProgram's current cell
  , nicoLoopBeginPointers :: Stack NicoPointer NicoPointer  -- ^ The NicoPointer is pushed to here when The NicoLoopBegin was evaluated
  }


-- | The initial state of NicoMachine
emptyMachine :: NicoMachine
emptyMachine = let illigalPointer = (-1)
                   emptyStack     = stack $ \xs -> (illigalPointer, [])
                   startPoint     = 0
               in NicoMachine M.empty startPoint emptyStack


-- | Evaluate and execute NicoLangAbstractSyntaxList with the virtual machine state
eval :: NicoLangAbstractSyntaxList -> StateT NicoMachine IO NicoMemory
eval [] = do
  (NicoMachine mem _ _) <- get
  return mem

eval (NicoForward:rest) = do
  machine@(NicoMachine mem np _) <- get
  put machine { nicoPointer = np + 1 }
  return mem

eval (NicoBackword:rest)  = undefined
eval (NicoIncr:rest)      = undefined
eval (NicoDecr:rest)      = undefined
eval (NicoOutput:rest)    = undefined
eval (NicoInput:rest)     = undefined
eval (NicoLoopBegin:rest) = undefined
eval (NicoLoopEnd:rest)   = undefined
