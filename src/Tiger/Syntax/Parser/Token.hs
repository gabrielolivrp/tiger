module Tiger.Syntax.Parser.Token where

import Data.ByteString

data Token
  = KwArray
  | KwBreak
  | KwDo
  | KwElse
  | KwEnd
  | KwFor
  | KwFunction
  | KwIf
  | KwIn
  | KwLet
  | KwNil
  | KwOf
  | KwThen
  | KwTo
  | KwType
  | KwVar
  | KwWhile
  | SymSemicolon
  | SymLParen
  | SymRParen
  | SymLBrack
  | SymRBrack
  | SymLBrace
  | SymRBrace
  | SymAssign
  | SymComma
  | SymColon
  | SymNeq
  | SymLt
  | SymLe
  | SymGt
  | SymGe
  | SymEq
  | SymAnd
  | SymOr
  | SymDot
  | SymPlus
  | SymMinus
  | SymTimes
  | SymDiv
  | TkIdent !ByteString
  | LitString !ByteString
  | LitInteger !Integer
  | TkEof
  deriving (Show, Eq)
