{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Runtime.Runtime where

import           Bytecode.Definition
import           Bytecode.Instruction
import           Runtime.Interpreter

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import qualified Data.Traversable     as Traversable
import qualified Data.Vector          as Vector

unionOrFail :: forall k v. (Show k, Ord k) => Map.Map k v -> Map.Map k v -> RuntimeInterpreter (Map.Map k v)
unionOrFail m0 m1 = foldM insertIfNotPresent m0 $ Map.toList m1
  where
    insertIfNotPresent :: Map.Map k v -> (k, v) -> RuntimeInterpreter (Map.Map k v)
    insertIfNotPresent m (key, val)
      | Map.member key m = throwError $ "unionOrFail: Key " ++ show key ++ " already in map"
      | otherwise = return $ Map.insert key val m

breakpoint :: Symbol -> RuntimeInterpreter ()
breakpoint symbol = do
  istate@RuntimeState{..} <- get
  case Map.lookup symbol runtimeStateSymbolMap of
    Nothing -> throwError $ "Cannot set breakpoint, symbol " ++ show symbol ++ " not loaded"
    Just addr -> put istate { runtimeStateBreakpoints = Set.insert addr runtimeStateBreakpoints }

resolveDefinitions :: [Definition Symbol] -> RuntimeInterpreter [Definition Addr]
resolveDefinitions defs = do
  istate@RuntimeState{..} <- get
  let
    defSymbols = map (\Definition{..} -> definitionSymbol) defs
    symbolAddrMap = reserve defSymbols runtimeStateHeap
  mergedSymbolMap <- unionOrFail runtimeStateSymbolMap symbolAddrMap
  resolvedDefs <-
    Traversable.for defs $ \def -> Traversable.for def $ \symbol ->
     case Map.lookup symbol mergedSymbolMap of
       Nothing -> throwError $ "Definition of symbol " ++ show symbol ++ " not found"
       Just addr -> return addr
  put istate { runtimeStateSymbolMap = mergedSymbolMap }
  return resolvedDefs

load :: Bool -> [Definition Symbol] -> RuntimeInterpreter ()
load trackStates defs = do
  resolvedDefs <- resolveDefinitions defs
  allocationStartAddr <- gets (Vector.length . heapData . runtimeStateHeap)
  let
    thunkOffset = length resolvedDefs
    allocateThunk lambdaOffset (Definition _ (DefinitionBodyLambda argCount _)) = do
      _ <- allocate (HeapCellThunk $ ThunkClosure (ClosureRoot (-argCount) (allocationStartAddr + thunkOffset + lambdaOffset)))
      return (lambdaOffset + 1)
    allocateThunk lambdaOffset (Definition _ (DefinitionBodyCaf _)) = do
      _ <- allocate HeapCellUninitializedCaf
      return lambdaOffset
    allocateLambdaCode (Definition _ (DefinitionBodyLambda _ instrs)) = allocate (HeapCellCode instrs) >> return ()
    allocateLambdaCode _ = return ()

    initializeCafThunks (Definition _ DefinitionBodyLambda{}) _ = return ()
    initializeCafThunks (Definition _ (DefinitionBodyCaf instrs)) thunkAddr = do
      mapM_ pushCont $ reverse instrs
      continue trackStates
      stackElem <- popData
      case stackElem of
        StackElemThunkValue thunk -> writeReference thunkAddr (HeapCellThunk thunk) >> return ()
        _ -> throwError $ "Unexpected result of CAF initialization " ++ show stackElem
  foldM_ allocateThunk (0 :: Int) resolvedDefs
  forM_ resolvedDefs allocateLambdaCode
  zipWithM_ initializeCafThunks resolvedDefs [allocationStartAddr ..]

evaluate :: Bool -> Symbol -> RuntimeInterpreter (Either Primitive Closure)
evaluate trackStates symbol = do
  RuntimeState{..} <- get
  case Map.lookup symbol runtimeStateSymbolMap of
    Nothing -> throwError $ "Symbol " ++ show symbol ++ " not found"
    Just thunkAddr -> do
      pushData (StackElemThunkAddr thunkAddr)
      pushCont Whnf
      continue trackStates
      stackElem <- popData
      case stackElem of
        StackElemThunkAddr _ -> throwError "Evaluation returned thunk address instead of proper value"
        StackElemThunkValue (ThunkPrimitive imm) -> return (Left imm)
        StackElemThunkValue (ThunkClosure closure) -> return (Right closure)

-- breakIfNeeded :: Addr -> RuntimeInterpreter ()
-- breakIfNeeded addr = do
--   RuntimeState{..} <- get
--   when (addr `Set.member` runtimeStateBreakpoints) $ throwError "Breakpoint"

interpret :: Interpreter m => Bool -> Instruction Addr -> m ()
interpret trackStates instruction = wrapCurrentInstruction instruction $ do
  when trackStates pushState
  case instruction of
    Enter presentArgNumber -> do
      topStackElem <- popData
      thunk <- toThunkValue topStackElem
      case thunk of
          ThunkPrimitive{} -> do
            -- when (presentArgNumber /= 0) $ throwError "Enter: primitive with args"
            pushData topStackElem
          ThunkClosure closure -> case closure of
              ClosurePartial closureHead closureArgs -> do
                forM_ closureArgs $ \argAddr -> do
                  pushData $ StackElemThunkAddr argAddr
                pushData $ StackElemThunkAddr closureHead
                pushCont (Enter $ presentArgNumber + length closureArgs)
                pushCont Whnf
              ClosureRoot saturation codeAddr -> do
                if (saturation + presentArgNumber < 0)
                  then do
                    pushData topStackElem
                    when (presentArgNumber /= 0) $ do
                      pushCont (CreateClosure presentArgNumber)
                      pushCont CreateThunk
                  else do
                    instrs <- toHeapCellCode =<< dereference codeAddr
                    when (saturation + presentArgNumber > 0) $
                      pushCont (Enter (saturation + presentArgNumber))
                    forM_ instrs $ \ec -> case ec of
                      Exec instr -> interpret trackStates instr
                      Cont instr -> pushCont instr
    Whnf -> do
      topStackElem <- popData
      addr <- toThunkAddr topStackElem
      thunk <- toHeapCellThunk =<< dereference addr
      case thunk of
        ThunkPrimitive{} -> pushData $ StackElemThunkValue thunk
        ThunkClosure{} -> do
            pushData $ StackElemThunkAddr addr
            pushData $ StackElemThunkValue thunk
            pushCont UpdateThunk -- update the thunk address with new value
            pushCont (Enter 0) -- evaluate thunk
    UpdateThunk -> do                 -- top element is value, top-1 is thunk address
      topStackElem <- popData
      thunkValue <- toThunkValue topStackElem
      addressElem <- popData
      thunkAddr <- toThunkAddr addressElem
      writeReference thunkAddr (HeapCellThunk thunkValue)
      pushData topStackElem
    CreateClosure arity -> do
      headAddr <- toThunkAddr =<< popData
      argAddrs <- reverse <$> replicateM arity (toThunkAddr =<< popData)
      pushData (StackElemThunkValue (ThunkClosure (ClosurePartial headAddr argAddrs)))
    CreateThunk -> do
      thunkValue <- toThunkValue =<< popData
      thunkAddr <- allocate (HeapCellThunk thunkValue)
      pushData (StackElemThunkAddr thunkAddr)
    Shift n -> do
      topElem <- popData
      discardData n
      pushData topElem
    Push p -> case p of
      PushableStackElem n -> do
        stackElem <- peekData n
        pushData stackElem
      PushableImmediate imm -> do
        pushData $ StackElemThunkValue (ThunkPrimitive imm)
      PushableKnownThunk thunkAddr -> do
        pushData $ StackElemThunkAddr thunkAddr
    PrimitiveOp op -> evaluatePrimitiveOp op
    Bail -> bail

continue :: Bool -> RuntimeInterpreter ()
continue trackStates = do
  mCont <- popCont
  case mCont of
    Nothing -> return ()
    Just cont -> interpret trackStates cont >> continue trackStates

reserve :: [Symbol] -> Heap -> Map.Map Symbol Addr
reserve symbols Heap{..} =
  Map.fromList $ zip symbols [ Vector.length heapData .. ]
