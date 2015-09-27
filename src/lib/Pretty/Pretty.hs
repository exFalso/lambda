{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Pretty.Pretty where

import           Bytecode.Definition
import           Bytecode.Instruction
import           Compile.SimpleLambda
import           Compile.Variable
import           Runtime.Interpreter
import           Syntax.Ast

import           Bound

import           Control.Applicative
import           Control.Monad
import           Data.List
import qualified Data.Map             as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text            as Text
import qualified Data.Vector          as Vector

vars :: [String]
vars = return <$> ['a' ..]

prettySimpleLambda :: SimpleLambda -> String
prettySimpleLambda (SimpleLambda sym n body) = Text.unpack sym <> " = \\" <> intercalate " " (take n vars) <> ". " <> prettySimpleLambdaBody body

prettyExp :: Exp Int Identifier -> String
prettyExp expr = case expr of
  Lam ls e -> "(\\" <> intercalate " " (Text.unpack <$> ls) <> ". " <> prettyExp (instantiate (\i -> Var $ ls !! i) e) <> ")"
  Var a -> Text.unpack a
  App a b -> "(" <> prettyExp a <> " " <> prettyExp b <> ")"
  Lit i -> show i

prettySimpleLambdaBody :: SimpleLambdaBody -> String
prettySimpleLambdaBody (SimpleApp a b) = "(" <> prettySimpleLambdaBody a <> " " <> prettySimpleLambdaBody b <> ")"
prettySimpleLambdaBody (SimpleVar var) = case var of
  Captured n -> vars !! n
  External t -> Text.unpack t
prettySimpleLambdaBody (SimpleLit l) = show l

prettyRuntimeState :: RuntimeState -> String
prettyRuntimeState RuntimeState{..} =
  "current instruction:\n" <>
  "  " <> show runtimeStateCurrentInstruction <> "\n" <>
  "data stack:\n" <>
  concatMap (\e -> "  " <> prettyDataStackElem thunkSymbolMap codeAddrSymbolMap (heapData runtimeStateHeap) e <> "\n") runtimeStateDataStack <>
  "continuation stack:\n" <>
  concatMap (\e -> "  " <> prettyInstruction e <> "\n") runtimeStateContStack <>
  "heap:\n" <>
  concatMap (\e -> "  " <> e <> "\n") (zipWith (\n heapCell -> show n <> " => " <> prettyHeapCell heapCell) [0 :: Int ..] (Vector.toList $ heapData runtimeStateHeap)) <>
  "loaded symbols:\n" <>
  concatMap (\(symbol, address) -> "  " <> show symbol <> " => " <> show address <> "\n") (Map.toList runtimeStateSymbolMap)
  where
    thunkSymbolMap = Map.fromList . map swap . Map.toList $ runtimeStateSymbolMap
    codeAddrSymbolMap = Map.fromList . catMaybes . map (\(a, t) -> fmap (\ca -> (ca, t)) (codeAddrOf a)) $ Map.toList thunkSymbolMap
    codeAddrOf addr = do
      cell <- heapData runtimeStateHeap Vector.!? addr
      case cell of
        HeapCellThunk (ThunkClosure (ClosureRoot _ ca)) -> return ca
        _ -> Nothing
    swap (a, b) = (b, a)

prettyAddr :: Int -> Map.Map Addr Symbol -> Map.Map Addr Symbol -> Vector.Vector HeapCell -> Addr -> String
prettyAddr 0 _ _ _ addr = "@" <> show addr
prettyAddr depth symbolMap codeAddrMap heap addr =
  fromMaybe ("@" <> show addr) $ msum
    [ do
         sym <- Map.lookup addr symbolMap
         return $ "T>" <> Text.unpack sym
    , do
         sym <- Map.lookup addr codeAddrMap
         return $ "C>" <> Text.unpack sym
    , do
         cell <- heap Vector.!? addr
         case cell of
           HeapCellCode _ -> return "[CODE]"
           HeapCellThunk thunk -> return $ "T>" <> prettyThunkValue depth symbolMap codeAddrMap heap thunk
           HeapCellUninitializedCaf -> return "[UNINIT CAF]"
    ]

prettyThunkValue :: Int -> Map.Map Addr Symbol -> Map.Map Addr Symbol -> Vector.Vector HeapCell -> Thunk -> String
prettyThunkValue depth symbolMap codeAddrMap heap thunk =
  ("V>" <>) $
    case thunk of
      ThunkClosure closure -> case closure of
        ClosurePartial headAddr args -> "(" <> intercalate " " (prettyAddr (depth - 1) symbolMap codeAddrMap heap <$> (headAddr : reverse args)) <> ")"
        ClosureRoot _ caddr -> prettyAddr (depth - 1) symbolMap codeAddrMap heap caddr
      ThunkPrimitive p -> show p

prettyDataStackElem :: Map.Map Addr Symbol -> Map.Map Addr Symbol -> Vector.Vector HeapCell -> DataStackElem -> String
prettyDataStackElem symbolMap codeAddrMap heap (StackElemThunkAddr addr) = prettyAddr 3 symbolMap codeAddrMap heap addr
prettyDataStackElem symbolMap codeAddrMap heap (StackElemThunkValue thunk) = prettyThunkValue 3 symbolMap codeAddrMap heap thunk

prettyInstruction :: Instruction Addr -> String
prettyInstruction = show

prettyHeapCell :: HeapCell -> String
prettyHeapCell = show

prettyDefinition :: Show a => Definition a -> String
prettyDefinition (Definition symbol body) =
  show symbol <> " [type=" <> case body of
    DefinitionBodyLambda arity instrs -> "lambda][arity=" <> show arity <> "]:\n" <> prettyExecConts instrs
    DefinitionBodyCaf instrs -> "caf]:\n" <> prettyInstrs instrs
  where
    prettyExecConts = concatMap (\instr -> "  " <> show instr <> "\n")
    prettyInstrs = concatMap (\instr -> "  " <> show instr <> "\n")
