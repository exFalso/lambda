{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Bytecode.Instruction where

import qualified Data.Foldable    as Foldable
import qualified Data.Text        as Text
import qualified Data.Traversable as Traversable

data ExecCont symbol
  = Exec (Instruction symbol)
  | Cont (Instruction symbol)
  deriving (Show, Functor, Traversable.Traversable, Foldable.Foldable)

data Instruction symbol
  = Enter Int
  | Whnf
  | CreateClosure Int
  | CreateThunk
  | Shift Int
  | Push (Pushable symbol)
  | PrimitiveOp PrimitiveOp
  | UpdateThunk
  | Bail
  deriving (Show, Functor, Traversable.Traversable, Foldable.Foldable)


data Pushable symbol
  = PushableStackElem Int
  | PushableKnownThunk symbol
  | PushableImmediate Primitive
  deriving (Show, Functor, Traversable.Traversable, Foldable.Foldable)

type Symbol = Text.Text
type Addr = Int

type Primitive = Int

data PrimitiveOp
  = Add
  | EqInt
  deriving (Show)
