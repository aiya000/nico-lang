-- | For executing nico-lang abstract syntax list
module NicoLang.Evaluator
  ( NicoMemory
  , NicoMemoryPointer
  , nicoProgramPointer
  , NicoMachine (NicoMachine)
  , emptyMachine
  , eval
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy (StateT, get, put, evalStateT, gets)
import Data.Char (chr, ord)
import Data.IntMap.Lazy (IntMap)
import Data.Maybe (isNothing)
import NicoLang.Parser.Items
import qualified Data.IntMap.Lazy as M

-- |
-- The virtual machine's memory,
-- The pairs of the memory address and the value.
type NicoMemory = IntMap Int

-- | The state of the virtual machine cell's current pointer
type NicoMemoryPointer = Int

-- | The ongoing program's position
type NicoProgramPointer = Int

-- | The virtual machine (The running program's state)
data NicoMachine = NicoMachine
  { nicoMemory         :: NicoMemory  -- ^ The state of the NicoProgram result
  , nicoMemoryPointer  :: NicoMemoryPointer  -- ^ The running machine's memory pointer
  , nicoProgramPointer :: NicoProgramPointer
  , nicoLoopBeginPointerStack :: [NicoProgramPointer]-- ^ The NicoMemoryPointer is pushed to here when The NicoLoopBegin was evaluated
  }

instance Show NicoMachine where
  show (NicoMachine mem memP opP lbSt) =
    "---"
    ++ "\nMemory: " ++ show mem
    ++ "\nMemory Pointer: " ++ show memP
    ++ "\nOperation Pointer: " ++ show opP
    ++ "\nLoop Begin Pointer Stack: " ++ show lbSt
    ++ "\n---"


-- | The initial state of NicoMachine
emptyMachine :: NicoMachine
emptyMachine = NicoMachine { nicoMemory         = M.empty
                           , nicoMemoryPointer  = 0
                           , nicoProgramPointer = 0
                           , nicoLoopBeginPointerStack = []
                           }


-- | Evaluate and execute NicoLangAbstractSyntaxList with the virtual machine state
eval :: NicoLangAbstractSyntaxList -> StateT NicoMachine IO NicoMemory
eval operationList = do
  opP <- gets nicoProgramPointer
  if operationAreFinished operationList opP
    then gets nicoMemory
    else do
      let op = operationList !! opP
      executeOperation op
      eval operationList
  where
    operationAreFinished :: NicoLangAbstractSyntaxList -> NicoProgramPointer -> Bool
    operationAreFinished xs ptr = length xs == ptr


-- Proceed the nicoProgramPointer to the next memory address
programGoesToNext :: StateT NicoMachine IO ()
programGoesToNext = do
  machine@(NicoMachine _ _ opP _) <- get
  put machine { nicoProgramPointer = opP + 1 }

-- Get the nicoMemoryPointer pointed value in the nicoMemory
getCurrentCell :: StateT NicoMachine IO Int
getCurrentCell = do
  (NicoMachine mem memP _ _) <- get
  case M.lookup memP mem of
    Nothing  -> return 0  -- 0 is the initial value of the cell
    Just val -> return val

-- Set the value to the nicoMemoryPointer pointed nicoMemory address
setCurrentCell :: Int -> StateT NicoMachine IO ()
setCurrentCell val = do
  machine@(NicoMachine mem memP _ _) <- get
  put machine { nicoMemory = M.insert memP val mem }


-- Execute a specified operation
executeOperation :: NicoOperation -> StateT NicoMachine IO ()
executeOperation NicoForward = do
  --logging
  machine <- get
  memP    <- gets nicoMemoryPointer
  put machine { nicoMemoryPointer = memP + 1 }
  programGoesToNext
  --where
  --  logging = do
  --    memP <- gets nicoMemoryPointer
  --    liftIO $ putStrLn $ "Forward the nicoMemoryPointer to " ++ show (memP + 1)

executeOperation NicoBackword = do
  --logging
  machine <- get
  memP    <- gets nicoMemoryPointer
  put machine { nicoMemoryPointer = memP - 1 }
  programGoesToNext
  --where
  --  logging = do
  --    memP <- gets nicoMemoryPointer
  --    liftIO $ putStrLn $ "Backward the nicoMemoryPointer to " ++ show (memP - 1)

executeOperation NicoIncr = do
  --logging
  machine <- get
  cell    <- getCurrentCell
  setCurrentCell $ cell + 1
  programGoesToNext
  --where
  --  logging = do
  --    cell <- getCurrentCell
  --    memP <- gets nicoMemoryPointer
  --    liftIO $ putStrLn $ "Increment (nicoMemory !! " ++ show memP ++ ") to " ++ show (cell + 1)

executeOperation NicoDecr = do
  --logging
  machine <- get
  cell    <- getCurrentCell
  setCurrentCell $ cell - 1
  programGoesToNext
  --where
  --  logging = do
  --    cell <- getCurrentCell
  --    memP <- gets nicoMemoryPointer
  --    liftIO $ putStrLn $ "Decrement (nicoMemory !! " ++ show memP ++ ") to " ++ show (cell - 1)

executeOperation NicoOutput = do
  cell <- getCurrentCell
  liftIO $ putChar $ chr cell
  programGoesToNext

executeOperation NicoInput = do
  val <- liftIO . fmap ord $ getChar
  --logging val
  setCurrentCell val
  programGoesToNext
  --where
  --  logging val = do
  --    memP <- gets nicoMemoryPointer
  --    liftIO $ putStrLn $ "Get " ++ show val ++ " from stdin, " ++ "and set it to (nicoMemory !! " ++ show memP ++ ")"

executeOperation NicoLoopBegin = do
  --logging
  machine@(NicoMachine _ _ opP lbPStack) <- get
  put machine { nicoLoopBeginPointerStack = opP:lbPStack }
  programGoesToNext
  --where
  --  logging = do
  --    opP <- gets nicoProgramPointer
  --    liftIO $ putStrLn $ "Push " ++ show opP ++ " to the pointer stack"

executeOperation NicoLoopEnd = do
  machine@(NicoMachine mem memP _ lbPStack) <- get
  case lbPStack of
    []              -> error "Cannot find the loop jump destination :("
    (lbP:lbPStack') -> do
      put machine { nicoLoopBeginPointerStack = lbPStack' }
      cell <- getCurrentCell
      if cell /= 0
        then do
          --loggingForLoopJump lbP
          put machine { nicoProgramPointer = lbP }
        else do
          --loggingForLoopFinish lbP
          programGoesToNext
  --where
  --  loggingForLoopJump   lbP = liftIO $ putStrLn $ "Pop " ++ show lbP ++ " from the pointer stack, and set it as the next program pointer"
  --  loggingForLoopFinish lbP = liftIO $ putStrLn $ "Pop " ++ show lbP ++ " from the pointer stack, and leave from the one of loop"
