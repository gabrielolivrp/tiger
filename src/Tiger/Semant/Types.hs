{-# LANGUAGE LambdaCase #-}

module Tiger.Semant.Types where

import Tiger.Semant.Unique
import qualified Tiger.Syntax.Parser.Ast as A

data Ty
  = TyInt
  | TyNil
  | TyUnit
  | TyString
  | TyRecord [(A.Symbol, Ty)] Unique
  | TyArray Ty Unique
  | TyName String Ty
  deriving (Eq)

instance Show Ty where
  show = \case
    TyInt -> "int"
    TyNil -> "nil"
    TyUnit -> "unit"
    TyString -> "string"
    TyRecord {} -> "record"
    TyArray {} -> "array"
    TyName x _ -> x
