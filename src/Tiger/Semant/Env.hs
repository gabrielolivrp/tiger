module Tiger.Semant.Env where

import qualified Tiger.Semant.Symtab as S
import qualified Tiger.Semant.Types as T
import qualified Tiger.Syntax.Parser.Ast as A

data EnvEntry
  = VarEntry
      { ty :: T.Ty
      }
  | FunEntry
      { formals :: [T.Ty],
        result :: T.Ty
      }
  deriving (Show)

type TEnv = S.Symtab T.Ty

type VEnv = S.Symtab EnvEntry

baseTEnv :: TEnv
baseTEnv =
  S.fromList
    [ (A.Symbol "int", T.TyInt),
      (A.Symbol "string", T.TyString),
      (A.Symbol "unit", T.TyUnit),
      (A.Symbol "nil", T.TyNil)
    ]

baseVEnv :: VEnv
baseVEnv =
  S.fromList
    [ (A.Symbol "print", FunEntry [T.TyString] T.TyUnit),
      (A.Symbol "printi", FunEntry [T.TyInt] T.TyUnit),
      (A.Symbol "flush", FunEntry [] T.TyUnit),
      (A.Symbol "getchar", FunEntry [] T.TyString),
      (A.Symbol "ord", FunEntry [T.TyString] T.TyInt),
      (A.Symbol "chr", FunEntry [T.TyInt] T.TyString),
      (A.Symbol "size", FunEntry [T.TyString] T.TyInt),
      (A.Symbol "substring", FunEntry [T.TyString, T.TyInt, T.TyInt] T.TyInt),
      (A.Symbol "concat", FunEntry [T.TyString, T.TyString] T.TyString),
      (A.Symbol "not", FunEntry [T.TyInt] T.TyInt),
      (A.Symbol "exit", FunEntry [T.TyInt] T.TyUnit)
    ]
