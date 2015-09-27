{-# LANGUAGE LambdaCase #-}
module Compile.Recapture where

import           Syntax.Ast

import           Bound
import           Data.Foldable
import qualified Data.List     as List
import qualified Data.Set      as Set

import           Prelude       hiding (foldl, foldr)

-- rearrange the expression so that all lambda subexpressions are closed
-- (\x -> y) ==> (\y0 x -> y0) y
-- x -> x
-- \y -> x y -> (\x0 y -> x0 y) x

-- \x -> x (\z -> x) ==> \x -> x ((\x1 z -> x1) x0)) x

recapture :: Exp Int Identifier -> Exp Int Identifier
recapture = recaptureWith Set.empty

recaptureWith :: Set.Set Identifier -> Exp Int Identifier -> Exp Int Identifier
recaptureWith outside = \case
  Lam ls e -> Lam ls $ abstract (\i -> List.elemIndex i ls) $ recapture' (outside `Set.union` Set.fromList ls) (instantiate (\i -> Var $ ls !! i) e)
  e@Var{} -> e
  e@Lit{} -> e
  App a b -> App (recapture a) (recapture b)
  where
    recapture' include e@Lam{} = etaExpand include (recaptureWith include e)
    recapture' _include e@Var{} = e
    recapture' _include e@Lit{} = e
    recapture' include (App a b) = App (recapture' include a) (recapture' include b)

  -- Lam{} -> instantiate (undefined) (toScope exp) -- undefined --instantiate (\ exp

-- x -> (\x0 -> x0) x

etaExpand :: Set.Set Identifier -> Exp Int Identifier -> Exp Int Identifier
etaExpand include expr =
  let
    idents = Set.toList $ Set.fromList (toList expr) `Set.intersection` include
  in foldl (\e i -> App e (Var i)) (Lam idents $ abstract (\i -> List.elemIndex i idents) expr) idents
