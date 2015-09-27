{-# LANGUAGE OverloadedStrings #-}
import           Bytecode.Definition
import           Bytecode.Instruction
import           Bytecode.Primitive
import           Pretty.Pretty
import           Runtime.Interpreter
import           Runtime.Runtime
import           Toplevel

import qualified Test.Framework                 as Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit

import           Data.Monoid
import qualified Data.Text                      as Text
import           System.Exit

checkExpression :: [Definition Symbol] -> Primitive -> Text.Text -> Assertion
checkExpression defs expected expr = do
  let toplevelDef = "__toplevel__ = " <> expr <> ";"
  (_, toplevel) <- case compileString toplevelDef of
    Left err -> do
      assertFailure $
        "ERROR while compiling expression " <> Text.unpack expr <> "\n" <>
        err
      undefined
    Right toplevel -> return toplevel

  let
    trackStates = True
    ((result, istates), istate) = runRuntimeInterpreter $ do
      load trackStates primitives
      load trackStates defs
      load trackStates toplevel

      evaluate trackStates $ "__toplevel__"
  case result of
    Left err ->
      assertFailure $
        "RUNTIME ERROR: " <> err <> "\n" <>
        prettyRuntimeState istate
    Right value -> Left expected @=? value

main = do
  (_, testDefs) <- compileFile "src/test/test.lam"
  Framework.defaultMain . hUnitTestToTests $ do
    "RuntimeInterpreter" ~:
      [ "Adding two numbers works" ~:
          checkExpression testDefs 3
            "#plus 1 2"
      , "Inefficient Fibonacci works" ~:
          checkExpression testDefs 55
            "fib 10"
      , "Efficient Fibonacci works" ~:
          checkExpression testDefs 55
            "fib2 10"
      , "Fix works" ~:
          checkExpression testDefs 20
            "sum (take 10 (fix (cons 2)))"
      ]
