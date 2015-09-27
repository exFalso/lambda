module Compile.SimpleLambda where

import           Bytecode.Instruction
import           Compile.Variable

-- a simple lambda is one that doesnt have a nested lambda expression
data SimpleLambda
  = SimpleLambda
    { simpleLambdaSymbol :: Symbol
    , simpleLambdaArity  :: Int
    , simpleLambdaBody   :: SimpleLambdaBody
    }
  deriving (Show)

data SimpleLambdaBody
  = SimpleApp SimpleLambdaBody SimpleLambdaBody
  | SimpleVar Variable
  | SimpleLit Int
  deriving (Show)
