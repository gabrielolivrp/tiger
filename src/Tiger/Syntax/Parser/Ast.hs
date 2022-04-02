{-# LANGUAGE LambdaCase #-}

module Tiger.Syntax.Parser.Ast where

import Data.ByteString
import Tiger.Syntax.Position

type Ident = ByteString

type TyId = ByteString

data TyField = TyField
  { tyFieldSpan :: Span,
    tyField :: Ident,
    tyFieldType :: TyId
  }
  deriving (Show)

data Ty
  = TyAlias Span TyId
  | TyRecord Span [TyField]
  | TyArray Span TyId
  deriving (Show)

data Dec
  = TypeDec
      { tyDecSpan :: Span,
        tyDecName :: Ident,
        tyDecBody :: Ty
      }
  | VarDec
      { varDecSpan :: Span,
        varDecName :: Ident,
        varDecTy :: Maybe TyId,
        varDecBody :: Expr
      }
  | FunctDec
      { funDecSpan :: Span,
        funDecName :: Ident,
        funDecParams :: [TyField],
        funDecTy :: Maybe TyId,
        funDecBody :: Expr
      }
  deriving (Show)

data LValue
  = LId Span Ident
  | LRecord Span LValue Ident
  | LArrayIndex Span LValue Expr
  deriving (Show)

data Expr
  = ENil Span
  | EBreak Span
  | EInteger Span Integer
  | EString Span ByteString
  | EFunctCall Span Ident [Expr]
  | EOp Span Expr Op Expr
  | EIf Span Expr Expr (Maybe Expr)
  | EWhile Span Expr Expr
  | EAssign Span LValue Expr
  | EFor Span Ident Expr Expr Expr
  | ELet Span [Dec] [Expr]
  | ESeq Span [Expr]
  | EArray Span TyId Expr Expr
  | ERecord Span TyId [(Ident, Expr)]
  | ELValue Span LValue
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
    ELValue x _ -> x

instance HasSpan Ty where
  getSpan = \case
    TyAlias x _ -> x
    TyRecord x _ -> x
    TyArray x _ -> x

instance HasSpan LValue where
  getSpan = \case
    LId x _ -> x
    LRecord x _ _ -> x
    LArrayIndex x _ _ -> x

instance HasSpan Dec where
  getSpan = \case
    TypeDec x _ _ -> x
    VarDec x _ _ _ -> x
    FunctDec x _ _ _ _ -> x

instance HasSpan TyField where
  getSpan (TyField x _ _) = x
