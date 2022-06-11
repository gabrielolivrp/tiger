{-# LANGUAGE LambdaCase #-}

module Tiger.Syntax.Parser.Ast where

import Data.ByteString
import Tiger.Syntax.Position

newtype Symbol = Symbol
  { symbol :: ByteString
  }
  deriving (Eq, Ord, Show)

data TyField = TyField
  { tyFieldSpan :: Span,
    tyFieldName :: Symbol,
    tyFieldType :: Symbol
  }
  deriving (Show)

data Ty
  = TyName Span Symbol
  | TyRecord Span [TyField]
  | TyArray Span Symbol
  deriving (Show)

data Dec
  = TypeDec
      { tyDecSpan :: Span,
        tyDecName :: Symbol,
        tyDecBody :: Ty
      }
  | VarDec
      { varDecSpan :: Span,
        varDecName :: Symbol,
        varDecTy :: Maybe Symbol,
        varDecBody :: Expr
      }
  | FunctDec
      { funDecSpan :: Span,
        funDecName :: Symbol,
        funDecParams :: [TyField],
        funDecTy :: Maybe Symbol,
        funDecBody :: Expr
      }
  deriving (Show)

data Var
  = SimpleVar Span Symbol
  | FieldVar Span Var Symbol
  | SubscriptVar Span Var Expr
  deriving (Show)

data Expr
  = ENil Span
  | EBreak Span
  | EInteger Span Integer
  | EString Span ByteString
  | EFunctCall Span Symbol [Expr]
  | EOp Span Expr Op Expr
  | EIf Span Expr Expr (Maybe Expr)
  | EWhile Span Expr Expr
  | EAssign Span Var Expr
  | EFor Span Symbol Expr Expr Expr
  | ELet Span [Dec] Expr
  | ESeq Span [Expr]
  | EArray Span Symbol Expr Expr
  | ERecord Span Symbol [(Symbol, Expr)]
  | EVar Span Var
  deriving (Show)

data Op
  = Plus
  | Minus
  | Times
  | Div
  | Eq
  | NEq
  | Gt
  | Lt
  | Ge
  | Le
  | And
  | Or
  deriving (Show)

instance HasSpan Expr where
  getSpan = \case
    ENil x -> x
    EBreak x -> x
    EInteger x _ -> x
    EString x _ -> x
    EFunctCall x _ _ -> x
    EOp x _ _ _ -> x
    EIf x _ _ _ -> x
    EWhile x _ _ -> x
    EAssign x _ _ -> x
    EFor x _ _ _ _ -> x
    ELet x _ _ -> x
    ESeq x _ -> x
    EArray x _ _ _ -> x
    ERecord x _ _ -> x
    EVar x _ -> x

instance HasSpan Ty where
  getSpan = \case
    TyName x _ -> x
    TyRecord x _ -> x
    TyArray x _ -> x

instance HasSpan Var where
  getSpan = \case
    SimpleVar x _ -> x
    FieldVar x _ _ -> x
    SubscriptVar x _ _ -> x

instance HasSpan Dec where
  getSpan = \case
    TypeDec x _ _ -> x
    VarDec x _ _ _ -> x
    FunctDec x _ _ _ _ -> x

instance HasSpan TyField where
  getSpan (TyField x _ _) = x
