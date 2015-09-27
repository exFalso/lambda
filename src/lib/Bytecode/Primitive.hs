{-# LANGUAGE OverloadedStrings #-}
module Bytecode.Primitive where

import           Bytecode.Definition
import           Bytecode.Instruction

primitives :: [Definition Symbol]
primitives =
  [ Definition "#true" $
    DefinitionBodyLambda 2
    [ Exec (Push (PushableStackElem 0))
    , Exec (Shift 2)
    , Exec (Whnf)
    ]
  , Definition "#false" $
    DefinitionBodyLambda 2
    [ Exec (Push (PushableStackElem 1))
    , Exec (Shift 2)
    , Exec (Whnf)
    ]
  , Definition "#eq_int" $
    DefinitionBodyLambda 2
    [ Cont (Shift 1)
    , Cont ((PrimitiveOp EqInt))
    , Cont Whnf
    , Cont ((Push (PushableStackElem 1)))
    , Exec Whnf
    ]
  , Definition "#plus" $
    DefinitionBodyLambda 2
    [ Cont (Shift 1)
    , Cont ((PrimitiveOp Add))
    , Cont Whnf
    , Cont ((Push (PushableStackElem 1)))
    , Exec Whnf
    ]
  ]
