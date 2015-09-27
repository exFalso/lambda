{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Bytecode.Definition where

import           Bytecode.Instruction

import qualified Data.Foldable        as Foldable
import qualified Data.Traversable     as Traversable

data Definition symbol
  = Definition
    { definitionSymbol :: symbol
    , definitionBody   :: DefinitionBody symbol
    }
  deriving (Show, Functor, Traversable.Traversable, Foldable.Foldable)

data DefinitionBody symbol
  = DefinitionBodyCaf [Instruction symbol] -- evaluated at initialization, creates the CAF thunk
  | DefinitionBodyLambda Int [ExecCont symbol] -- evaluated at runtime
  deriving (Show, Functor, Traversable.Traversable, Foldable.Foldable)
