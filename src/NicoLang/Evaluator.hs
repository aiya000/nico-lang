{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | For executing nico-lang abstract syntax list
module NicoLang.Evaluator
  ( VMMemory
  , VMMemoryPointer
  , BFVirtualMachine (BFVirtualMachine)
  , emptyMachine
  , BrainState
  , runBrainState
  , eval
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (MonadState, gets)
import Control.Monad.State.Lazy (StateT, get, put, runStateT)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.Writer.Lazy (WriterT, tell, runWriterT)
import Data.Char (chr, ord)
import Data.IntMap.Lazy (IntMap)
import NicoLang.Parser.Items
import qualified Data.IntMap.Lazy as M

-- |
-- The virtual machine's memory,
-- The pairs of the memory address and the value.
type VMMemory = IntMap Int

-- | The state of the virtual machine cell's current pointer
type VMMemoryPointer = Int

-- | The ongoing program's position
type BFProgramPointer = Int

-- | The running brainf*ck program's state
data BFVirtualMachine = BFVirtualMachine
  { vmMemory         :: VMMemory  -- ^ The state of the NicoProgram result
  , vmMemoryPointer  :: VMMemoryPointer  -- ^ The running machine's memory pointer
  , bfProgramPointer :: BFProgramPointer
  , loopBeginPointerStack :: [BFProgramPointer]-- ^ The VMMemoryPointer is pushed to here when The NicoLoopBegin was evaluated
  }

instance Show BFVirtualMachine where
  show (BFVirtualMachine mem memP opP lbSt) =
    "---"
    ++ "\nMemory: " ++ show mem
    ++ "\nMemory Pointer: " ++ show memP
    ++ "\nOperation Pointer: " ++ show opP
    ++ "\nLoop Begin Pointer Stack: " ++ show lbSt
    ++ "\n---"


-- | The initial state of BFVirtualMachine
emptyMachine :: BFVirtualMachine
emptyMachine = BFVirtualMachine { vmMemory         = M.empty
                                , vmMemoryPointer  = 0
                                , bfProgramPointer = 0
                                , loopBeginPointerStack = []
                                }

-- The state of BFVirtualMachine with the logging
newtype BrainState result = BrainState
  { _runBrainState :: WriterT [String] (StateT BFVirtualMachine IO) result
  } deriving ( Functor, Applicative, Monad
             , MonadWriter [String], MonadState BFVirtualMachine, MonadIO
             )

runBrainState :: BrainState result -> BFVirtualMachine -> IO ((result, [String]), BFVirtualMachine)
runBrainState s a = flip runStateT a . runWriterT . _runBrainState $ s


-- | Evaluate and execute NicoLangProgram with the virtual machine state
eval :: NicoLangProgram -> BrainState VMMemory
eval operationList = do
  opP <- gets bfProgramPointer
  if operationAreFinished operationList opP
    then gets vmMemory
    else do
      let op = operationList !! opP
      executeOperation op
      eval operationList
  where
    operationAreFinished :: NicoLangProgram -> BFProgramPointer -> Bool
    operationAreFinished xs ptr = length xs == ptr


-- Proceed the bfProgramPointer to the next memory address
programGoesToNext :: BrainState ()
programGoesToNext = do
  machine@(BFVirtualMachine _ _ opP _) <- get
  put machine { bfProgramPointer = opP + 1 }

-- Get the vmMemoryPointer pointed value in the vmMemory
getCurrentCell :: BrainState Int
getCurrentCell = do
  (BFVirtualMachine mem memP _ _) <- get
  case M.lookup memP mem of
    Nothing  -> return 0  -- 0 is the initial value of the cell
    Just val -> return val

-- Set the value to the vmMemoryPointer pointed vmMemory address
setCurrentCell :: Int -> BrainState ()
setCurrentCell val = do
  machine@(BFVirtualMachine mem memP _ _) <- get
  put machine { vmMemory = M.insert memP val mem }


-- Execute a specified operation
executeOperation :: NicoOperation -> BrainState ()
executeOperation NicoForward = do
  logging
  machine <- get
  memP    <- gets vmMemoryPointer
  put machine { vmMemoryPointer = memP + 1 }
  programGoesToNext
  where
    logging :: BrainState ()
    logging = do
      memP <- gets vmMemoryPointer
      tell ["Forward the vmMemoryPointer to " ++ show (memP + 1)]

executeOperation NicoBackword = do
  logging
  machine <- get
  memP    <- gets vmMemoryPointer
  put machine { vmMemoryPointer = memP - 1 }
  programGoesToNext
  where
    logging :: BrainState ()
    logging = do
      memP <- gets vmMemoryPointer
      tell ["Backward the vmMemoryPointer to " ++ show (memP - 1)]

executeOperation NicoIncr = do
  logging
  cell    <- getCurrentCell
  setCurrentCell $ cell + 1
  programGoesToNext
  where
    logging :: BrainState ()
    logging = do
      cell <- getCurrentCell
      memP <- gets vmMemoryPointer
      tell ["Increment (vmMemory !! " ++ show memP ++ ") to " ++ show (cell + 1)]

executeOperation NicoDecr = do
  logging
  cell    <- getCurrentCell
  setCurrentCell $ cell - 1
  programGoesToNext
  where
    logging :: BrainState ()
    logging = do
      cell <- getCurrentCell
      memP <- gets vmMemoryPointer
      tell ["Decrement (vmMemory !! " ++ show memP ++ ") to " ++ show (cell - 1)]

executeOperation NicoOutput = do
  cell <- getCurrentCell
  liftIO $ putChar $ chr cell
  programGoesToNext

executeOperation NicoInput = do
  val <- liftIO . fmap ord $ getChar
  logging val
  setCurrentCell val
  programGoesToNext
  where
    logging :: Int -> BrainState ()
    logging val = do
      memP <- gets vmMemoryPointer
      tell ["Get " ++ show val ++ " from stdin, " ++ "and set it to (vmMemory !! " ++ show memP ++ ")"]

executeOperation NicoLoopBegin = do
  logging
  machine@(BFVirtualMachine _ _ opP lbPStack) <- get
  put machine { loopBeginPointerStack = opP:lbPStack }
  programGoesToNext
  where
    logging = do
      opP <- gets bfProgramPointer
      tell ["Push " ++ show opP ++ " to the pointer stack"]

executeOperation NicoLoopEnd = do
  machine  <- get
  lbPStack <- gets loopBeginPointerStack
  case lbPStack of
    []              -> error "Cannot find the loop jump destination :("
    (lbP:lbPStack') -> do
      put machine { loopBeginPointerStack = lbPStack' }
      cell <- getCurrentCell
      if cell /= 0
        then do
          loggingForLoopJump lbP
          put machine { bfProgramPointer = lbP }
        else do
          loggingForLoopFinish lbP
          programGoesToNext
  where
    loggingForLoopJump :: Int -> BrainState ()
    loggingForLoopJump lbP = tell ["Pop " ++ show lbP ++ " from the pointer stack, and set it as the next program pointer"]
    loggingForLoopFinish :: Int -> BrainState ()
    loggingForLoopFinish lbP = tell ["Pop " ++ show lbP ++ " from the pointer stack, and leave from the one of loop"]
