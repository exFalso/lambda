{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Runtime.Interpreter where

import           Bytecode.Instruction

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer

import qualified Data.Map             as Map
import qualified Data.Set             as Set
import qualified Data.Vector          as Vector

import           Safe

class Monad m => Interpreter m where
  wrapCurrentInstruction :: Instruction Addr -> m () -> m ()
  pushState :: m ()

  discardData :: Int -> m ()
  peekData :: Int -> m DataStackElem
  pushData :: DataStackElem -> m ()
  pushCont :: Instruction Addr -> m ()
  popData :: m DataStackElem
  popCont :: m (Maybe (Instruction Addr))
  allocate :: HeapCell -> m Addr
  dereference :: Addr -> m HeapCell
  writeReference :: Addr -> HeapCell -> m ()

  toThunkValue :: DataStackElem -> m Thunk
  toThunkAddr :: DataStackElem -> m Addr
  toHeapCellThunk :: HeapCell -> m Thunk
  toHeapCellCode :: HeapCell -> m [ExecCont Addr]
  toThunkPrimitive :: Thunk -> m Primitive

  evaluatePrimitiveOp :: PrimitiveOp -> m ()
  getKnownThunkAddr :: Symbol -> m Addr

  bail :: m ()


instance Interpreter RuntimeInterpreter where
  wrapCurrentInstruction instruction i = do
    modify (\istate -> istate { runtimeStateCurrentInstruction = Just instruction })
    i
    modify (\istate -> istate { runtimeStateCurrentInstruction = Nothing })

  pushState = tell . return =<< get

  discardData n = do
    istate@RuntimeState{..} <- get
    put istate { runtimeStateDataStack = drop n runtimeStateDataStack }
  peekData offset = do
    RuntimeState{..} <- get
    case atMay runtimeStateDataStack offset of
      Nothing -> throwError $ "Stack offset " ++ show offset ++ " out of bounds"
      Just stackElem -> return stackElem

  pushData stackElem = do
    istate@RuntimeState{..} <- get
    put istate { runtimeStateDataStack = stackElem : runtimeStateDataStack }
  pushCont stackElem = do
    istate@RuntimeState{..} <- get
    put istate { runtimeStateContStack = stackElem : runtimeStateContStack }
  popData = do
    istate@RuntimeState{..} <- get
    case runtimeStateDataStack of
      (a : as) -> put istate { runtimeStateDataStack = as } >> return a
      [] -> throwError "popData called, but data stack is empty"
  popCont = do
    istate@RuntimeState{..} <- get
    case runtimeStateContStack of
      (a : as) -> put istate { runtimeStateContStack = as } >> return (Just a)
      [] -> return Nothing
  allocate cell = do
    istate@RuntimeState{..} <- get
    put istate { runtimeStateHeap = Heap (heapData runtimeStateHeap `Vector.snoc` cell) }
    return $ Vector.length (heapData runtimeStateHeap)
  dereference addr = do
    RuntimeState{..} <- get
    case heapData runtimeStateHeap Vector.!? addr of
      Nothing -> throwError $ "Invalid reference " ++ show addr
      Just cell -> return cell
  writeReference addr value = do
    istate@RuntimeState{..} <- get
    put istate { runtimeStateHeap = Heap (heapData runtimeStateHeap Vector.// [(addr, value)]) }

  toHeapCellCode (HeapCellCode instrs) = return instrs
  toHeapCellCode _ = throwError "toHeapCellCode: not code"

  toHeapCellThunk (HeapCellThunk thunk) = return thunk
  toHeapCellThunk _ = throwError "toHeapCellThunk: not thunk"

  toThunkAddr (StackElemThunkAddr addr) = return addr
  toThunkAddr StackElemThunkValue{} = throwError "toThunkAddr: ThunkValue"

  toThunkValue StackElemThunkAddr{} = throwError "toThunkValue: ThunkAddr"
  toThunkValue (StackElemThunkValue value) = return value

  toThunkPrimitive (ThunkPrimitive prim) = return prim
  toThunkPrimitive _ = throwError "toThunkPrimitive: not primitive"

  evaluatePrimitiveOp op = case op of
    Add -> do
      a <- toThunkPrimitive =<< toThunkValue =<< popData
      b <- toThunkPrimitive =<< toThunkValue =<< popData
      pushData $ StackElemThunkValue (ThunkPrimitive (a + b))
    EqInt -> do
      a <- toThunkPrimitive =<< toThunkValue =<< popData
      b <- toThunkPrimitive =<< toThunkValue =<< popData
      if a == b
        then pushData . StackElemThunkAddr =<< getKnownThunkAddr "#true"
        else pushData . StackElemThunkAddr =<< getKnownThunkAddr "#false"
      pushCont Whnf

  getKnownThunkAddr symbol = do
    RuntimeState{..} <- get
    case Map.lookup symbol runtimeStateSymbolMap of
      Nothing -> throwError $ "getKnownThunkAddr: symbol " <> show symbol <> " not found"
      Just addr -> return addr

  bail = throwError "Bail"

data RuntimeState
  = RuntimeState
    { runtimeStateHeap               :: Heap
    , runtimeStateDataStack          :: [DataStackElem]
    , runtimeStateContStack          :: [Instruction Addr]
    , runtimeStateSymbolMap          :: Map.Map Symbol Addr
    , runtimeStateCurrentInstruction :: Maybe (Instruction Addr)
    , runtimeStateBreakpoints        :: Set.Set Addr
    }
  deriving (Show)

newtype RuntimeInterpreter a = RuntimeInterpreter (ExceptT String (WriterT [RuntimeState] (State RuntimeState)) a)
  deriving ( Functor, Applicative, Monad
           , MonadError String, MonadState RuntimeState, MonadWriter [RuntimeState])

-- thunk is in whnf if primitive or closure is unsaturated
data Thunk
  = ThunkClosure Closure
  | ThunkPrimitive Primitive
  deriving (Show)

data DataStackElem
  = StackElemThunkAddr Addr
  | StackElemThunkValue Thunk
  deriving (Show)

data Heap
  = Heap
    { heapData :: Vector.Vector HeapCell
    }
  deriving (Show)

data HeapCell
  = HeapCellCode [ExecCont Addr]
  | HeapCellThunk Thunk
  | HeapCellUninitializedCaf
  deriving (Show)


data Closure
  = ClosurePartial
    { closurePartialHead      :: Addr -- thunk addr
    , closurePartialArguments :: [Addr] -- thunk addr
    }
  | ClosureRoot
    { closureSaturation :: Int
    , closureCode       :: Addr            -- points to actual code
    }
  deriving (Eq, Show)

initRuntimeState :: RuntimeState
initRuntimeState
  = RuntimeState
    { runtimeStateHeap = Heap Vector.empty
    , runtimeStateDataStack = []
    , runtimeStateContStack = []
    , runtimeStateSymbolMap = Map.empty
    , runtimeStateCurrentInstruction = Nothing
    , runtimeStateBreakpoints = Set.empty
    }

runRuntimeInterpreter :: RuntimeInterpreter a -> ((Either String a, [RuntimeState]), RuntimeState)
runRuntimeInterpreter (RuntimeInterpreter m) = runState (runWriterT (runExceptT m)) initRuntimeState
