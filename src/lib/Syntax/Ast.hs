{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
module Syntax.Ast where

import           Bound
import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import qualified Data.Text           as Text
import           Data.Traversable
import           Prelude.Extras

type Identifier = Text.Text

data Exp b a
  = Var a
  | Lam [Identifier] (Scope b (Exp b) a)
  | App (Exp b a) (Exp b a)
  | Lit Int
  deriving (Show, Traversable, Foldable)

instance Show b => Show1 (Exp b)

instance Functor (Exp b) where
  fmap f (Var a) = Var (f a)
  fmap f (Lam is e) = Lam is (fmap f e)
  fmap f (App a b) = App (fmap f a) (fmap f b)
  fmap _ (Lit n) = Lit n

instance Applicative (Exp b) where
  pure = Var
  (<*>) = ap

instance Monad (Exp b) where
  return = Var
  Var a >>= f = f a
  Lam is e >>= f = Lam is (e >>>= f)
  App a b >>= f = App (a >>= f) (b >>= f)
  Lit num >>= _ = Lit num

data Declaration
  = FunctionDeclaration Identifier (Exp Int Identifier)
  deriving (Show)

lambdaLift :: Exp Int a -> Exp Int a
lambdaLift (Var a) = Var a
lambdaLift (Lam is e) = case lambdaLift (fromScope e) of
  Lam ls e' -> Lam (is ++ ls) $ toScope (instantiate (\i -> pure $ B (length is + i)) $ e')
  other -> Lam is $ toScope other
lambdaLift (App a b) = App (lambdaLift a) (lambdaLift b)
lambdaLift (Lit num) = Lit num
