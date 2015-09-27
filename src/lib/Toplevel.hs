{-# LANGUAGE OverloadedStrings #-}
module Toplevel where

import           Bytecode.Definition
import           Bytecode.Instruction
import           Bytecode.Primitive
import           Compile.Compile
import           Compile.SimpleLambda
import           Pretty.Pretty
import           Runtime.Interpreter
import           Runtime.Runtime
import           Syntax.Parser

import           Control.Monad.Except
import           Data.Attoparsec.Text
import           Data.Monoid
import qualified Data.Text            as Text
import qualified Data.Text.IO         as Text
import qualified Data.Vector          as Vector
import           System.Exit


compileFile :: String -> IO ([SimpleLambda], [Definition Symbol])
compileFile filename = do
  code <- Text.readFile filename
  case compileString code of
    Left err -> putStrLn ("ERROR: " <> err) >> exitFailure
    Right defs -> return defs

compileString :: Text.Text -> Either String ([SimpleLambda], [Definition Symbol])
compileString code = runCompile
                     (compile =<<
                      liftEither (parseOnly toplevel code))


justCompile :: String -> IO ()
justCompile filename = do
  (simpleLambdas, defs) <- compileFile filename
  mapM_ (putStrLn . prettySimpleLambda) simpleLambdas
  mapM_ (putStrLn . prettyDefinition) defs

runFile :: Bool -> String -> String -> IO ()
runFile trackStates filename symbol = do
  (_simpleLambdas, defs) <- compileFile filename
  let ((result, istates), istate) = runRuntimeInterpreter $ do
        load trackStates primitives
        load trackStates defs
        evaluate trackStates $ Text.pack symbol
  forM_ ( istates) $ \s -> do
    putStrLn (prettyRuntimeState s)
    -- getLine >> return ()
  case result of
    Left err -> do
      putStrLn $ prettyRuntimeState istate
      putStrLn $ "RUNTIME ERROR: " ++ err
      exitFailure
    Right value -> do
      print value
      print (Vector.length (heapData (runtimeStateHeap istate)))
