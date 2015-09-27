{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
module Compile.Compile where

import           Bytecode.Definition
import           Bytecode.Instruction
import           Bytecode.Primitive
import           Compile.Recapture
import           Compile.SimpleLambda
import           Compile.Variable
import           Syntax.Ast

import           Bound
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Set             as Set
import qualified Data.Text            as Text
import qualified Data.Traversable     as Traversable

newtype Compile a =
  Compile { unCompile :: Except String a }
    deriving ( Functor, Applicative, Monad
             , MonadError String )

compile :: [Declaration] -> Compile ([SimpleLambda], [Definition Symbol])
compile declarations = do
  when (Set.size symbolsInScope /= length symbolsInScopeList) $ throwError "Duplicate symbolsl"
  simpleLambdas <- concat <$> mapM (toSimpleLambdas allSymbolsInScope) declarations
  definitions <- mapM toDefinition simpleLambdas
  return (simpleLambdas, definitions)
  where
    symbolsInScopeList = map (\(FunctionDeclaration ident _) -> ident) declarations
    symbolsInScope = Set.fromList symbolsInScopeList
    allSymbolsInScope = symbolsInScope `Set.union` primitiveSymbols
    primitiveSymbols = Set.fromList $ definitionSymbol <$> primitives

liftEither :: Either String a -> Compile a
liftEither e = Compile $ ExceptT (return e)

runCompile :: Compile a -> Either String a
runCompile (Compile e) = runExcept e

toSimpleLambdas :: Set.Set Text.Text -> Declaration -> Compile [SimpleLambda]
toSimpleLambdas symbolsInScope (FunctionDeclaration identifier topExpr) = do
  let recapturedExpr = lambdaLift (recapture topExpr)
  resolvedExpr <- resolveSymbols symbolsInScope recapturedExpr
  let (lambdas, _newCounter) = toSimpleLambdas' 0 resolvedExpr
  return lambdas
  where
    createSymbol counter = identifier <> Text.pack (replicate counter '\\')

    toSimpleLambdas' :: Int -> Exp Int Variable -> ([SimpleLambda], Int)
    toSimpleLambdas' counter expr = case expr of
      Lam args e ->
        let (body, others, newCounter) = toSimpleLambdaBody (counter + 1) (instantiate (Var . Captured) e)
        in (SimpleLambda (createSymbol counter) (length args) body : others, newCounter)
      _ ->
        let (body, others, newCounter) = toSimpleLambdaBody (counter + 1) expr
        in (SimpleLambda (createSymbol counter) 0 body : others, newCounter)

    toSimpleLambdaBody :: Int -> Exp Int Variable -> (SimpleLambdaBody, [SimpleLambda], Int)
    toSimpleLambdaBody counter expr = case expr of
      Var variable -> (SimpleVar variable, [], counter)
      Lam{} ->
        let (lambdas, newCounter) = toSimpleLambdas' counter expr
        in (SimpleVar $ External (createSymbol counter), lambdas, newCounter)
      App expr0 expr1 ->
        let (body0, others0, newCounter0) = toSimpleLambdaBody counter expr0
            (body1, others1, newCounter1) = toSimpleLambdaBody newCounter0 expr1
        in (SimpleApp body0 body1, others0 <> others1, newCounter1)
      Lit int -> (SimpleLit int, [], counter)

data TopStackThunkType
  = TopStackThunkAddr
  | TopStackThunkValue
  deriving (Show)

expectThunkValue :: (TopStackThunkType, [Instruction Symbol]) -> Compile [Instruction Symbol]
expectThunkValue (TopStackThunkValue, instrs) = return instrs
expectThunkValue _ = throwError "expectThunkValue: got thunk addr"

toThunkAddr :: (TopStackThunkType, [Instruction Symbol]) -> [Instruction Symbol]
toThunkAddr (TopStackThunkValue, instrs) = instrs ++ [CreateThunk]
toThunkAddr (TopStackThunkAddr, instrs) = instrs

toThunkValue :: (TopStackThunkType, [Instruction Symbol]) -> [Instruction Symbol]
toThunkValue (TopStackThunkValue, instrs) = instrs
toThunkValue (TopStackThunkAddr, instrs) = instrs ++ [Whnf]

-- the returned arguments are in reverse order (the rightmost is the first in the list)
flattenApplication :: SimpleLambdaBody -> (SimpleLambdaBody, [SimpleLambdaBody])
flattenApplication (SimpleApp expr0 expr1) = let (headExpr, args) = flattenApplication expr0 in (headExpr, expr1 : args)
flattenApplication expr = (expr, [])

toDefinition :: SimpleLambda -> Compile (Definition Symbol)
toDefinition (SimpleLambda lambdaName arity topLambdaBody) = Definition lambdaName <$> defBody
  where
    defBody
      | arity /= 0 = do
        closureCreated <- toThunkValue <$> topLevelToBytecode topLambdaBody
        return $ DefinitionBodyLambda arity $ map Cont $ reverse $ closureCreated <> [Enter 0, Shift arity]
      | otherwise = DefinitionBodyCaf <$> (expectThunkValue =<< topLevelToBytecode topLambdaBody)

    topLevelToBytecode :: SimpleLambdaBody -> Compile (TopStackThunkType, [Instruction Symbol])
    topLevelToBytecode body =
      let (ret, stackOffset) = runState (toBytecode body) 0
      in do
        when (stackOffset /= 1) $ throwError ("Stack offset is " <> show stackOffset <> "\n" <> show ret)
        return ret

    toBytecode :: SimpleLambdaBody -> State Int (TopStackThunkType, [Instruction Symbol])
    toBytecode lambdaBody = case lambdaBody of
      SimpleApp{} -> do
        let (headExpr, args) = flattenApplication lambdaBody
        argBytecodes <- forM args $ \arg -> toThunkAddr <$> toBytecode arg
        headBytecode <- toThunkAddr <$> toBytecode headExpr
        let argN = length args
        modify (\stackOffset -> stackOffset - argN)
        return
          ( TopStackThunkValue
          ,    concat argBytecodes  -- push argument thunks onto stack
            <> headBytecode         -- evaluate head
            <> [CreateClosure argN] -- create closure
          )

      SimpleVar variable -> do
        code <- case variable of
          Captured n -> do
            stackOffset <- get
            return [Push (PushableStackElem (n + stackOffset))]
          External symbol -> do
            return [Push (PushableKnownThunk symbol)]
        modify (+ 1)
        return (TopStackThunkAddr, code)

      SimpleLit lit -> do
        modify (+ 1)
        return (TopStackThunkValue, [Push (PushableImmediate lit)])

resolveSymbols :: Set.Set Text.Text -> Exp Int Identifier -> Compile (Exp Int Variable)
resolveSymbols symbolsInScope expr = Traversable.forM expr $ \ident -> do
  -- when (ident `Set.notMember` symbolsInScope) $ throwError $ "Cannot resolve symbol " <> show ident
  return (External ident)

getArity :: Exp Int a -> Int
getArity (Lam is _) = length is
getArity _ = 0
