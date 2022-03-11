module Tiger.Syntax.Parser.Ast where

import Data.ByteString

type Id = ByteString

type TyId = ByteString

data TyField = TyField
  { tyField :: Id,
    tyFildId :: TyId
  }
  deriving (Show)

data Ty
  = TyAlias TyId
  | TyRecord [TyField]
  | TyArray TyId
  deriving (Show)

data Dec
  = DecType
      { dTyName :: Id,
        dTyBody :: Ty
      }
  | DecVar
      { dVarName :: Id,
        dVarTy :: Maybe TyId,
        dVarBody :: Expr
      }
  | DecFunct
      { dFunName :: Id,
        dFunParams :: [TyField],
        dFunRetType :: Maybe TyId,
        dFunBody :: Expr
      }
  deriving (Show)

data LValue
  = LId Id
  | LRecord LValue Id
  | LArrayIndex LValue Expr
  deriving (Show)

data Expr
  = ENil
  | EBreak
  | EInteger Integer
  | EString ByteString
  | EFunctCall Id [Expr]
  | EOp Expr Op Expr
  | EIf Expr Expr (Maybe Expr)
  | EWhile Expr Expr
  | EAssign LValue Expr
  | EFor Id Expr Expr Expr
  | ELet [Dec] [Expr]
  | ESeq [Expr]
  | EArray TyId Expr Expr
  | ERecord TyId [(Id, Expr)]
  | ELValue LValue
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
