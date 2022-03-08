module Tiger.Syntax.Lexer.Token where

import Data.ByteString
import Tiger.Syntax.Position

data Token
  = TkId ByteString
  | TkSymbol Symbol
  | TkKeyword Keyword
  | TkLiteral Literal
  | TkEof
  deriving (Show, Eq)

data Symbol
  = SymSemicolon
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
  deriving (Show, Eq)

data Keyword
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
  deriving (Show, Eq)

data Literal
  = LitString ByteString
  | LitInteger Integer
  deriving (Show, Eq)

data TokenInfo = TokenInfo
  { info :: Token,
    startPos :: !Position,
    endPos :: !Position
  }
  deriving (Show, Eq)
