-- | For executing nico-lang abstract syntax list
module NicoLang.Evaluator
  ( NicoMemory
  , NicoPointer
  , NicoMachine
  , emptyMachine
  , eval
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy (StateT, get, put)
import Data.Char (chr, ord)
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

eval (NicoBackword:rest) = do
  machine@(NicoMachine mem np _) <- get
  put machine { nicoPointer = np - 1 }
  return mem

eval (NicoIncr:rest) = do
  (NicoMachine mem np st) <- get
  case M.lookup np mem of
    Nothing  -> let newMem = M.insert np 1 mem  -- 1 is `0 (initial state) + 1 (forward)`
                in put $ NicoMachine newMem np st
    Just val -> let newMem = M.insert np (val + 1) mem  -- Replace val to `val + 1` on mem
                in put $ NicoMachine newMem np st
  return mem

eval (NicoDecr:rest) = do
  (NicoMachine mem np st) <- get
  case M.lookup np mem of
    Nothing  -> let newMem = M.insert np (-1) mem  -- 1 is `0 (initial state) - 1 (backward)`
                in put $ NicoMachine newMem np st
    Just val -> let newMem = M.insert np (val - 1) mem  -- Replace val to `val - 1` on mem
                in put $ NicoMachine newMem np st
  return mem

eval (NicoOutput:rest) = do
  (NicoMachine mem np st) <- get
  case M.lookup np mem of
    Nothing  -> liftIO $ putChar $ chr 0
    Just val -> liftIO $ putChar $ chr val
  return mem

eval (NicoInput:rest) = do
  machine@(NicoMachine mem np st) <- get
  val <- liftIO . fmap ord $ getChar
  let newMem = M.insert np val mem
  put machine { nicoMemory = newMem }
  return newMem

eval (NicoLoopBegin:rest) = undefined
eval (NicoLoopEnd:rest)   = undefined
