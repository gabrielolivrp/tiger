{-# LANGUAGE LambdaCase #-}

module Tiger.Types where

import qualified Tiger.Syntax.Parser.Ast as A
import Tiger.Unique

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
