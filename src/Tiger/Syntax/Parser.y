{
module Tiger.Syntax.Parser (runParser) where

import Tiger.Syntax.Parser.Token
import Tiger.Syntax.Parser.Ast
import Data.ByteString (ByteString)
import Tiger.Syntax.Parser.Lexer
import Tiger.Syntax.Parser.Monad
import Tiger.Syntax.Error.ParseError
import Tiger.Syntax.Position
}
-- | Syntactic Specifications
-- References: https://www.lrde.epita.fr/~tiger/tiger.split/Syntactic-Specifications.html#Syntactic-Specifications

%name parse Expr

%monad { Parser }
%tokentype { Loc Token }
%lexer { lexer } { Loc { locInfo = TkEof } }

%token
  array                     { Loc { locInfo = TkKeyword KwArray } }
  break                     { Loc { locInfo = TkKeyword KwBreak } }
  do                        { Loc { locInfo = TkKeyword KwDo } }
  else                      { Loc { locInfo = TkKeyword KwElse } }
  end                       { Loc { locInfo = TkKeyword KwEnd } }
  for                       { Loc { locInfo = TkKeyword KwFor } }
  function                  { Loc { locInfo = TkKeyword KwFunction } }
  if                        { Loc { locInfo = TkKeyword KwIf } }
  in                        { Loc { locInfo = TkKeyword KwIn } }
  let                       { Loc { locInfo = TkKeyword KwLet } }
  nil                       { Loc { locInfo = TkKeyword KwNil } }
  of                        { Loc { locInfo = TkKeyword KwOf } }
  then                      { Loc { locInfo = TkKeyword KwThen } }
  to                        { Loc { locInfo = TkKeyword KwTo } }
  type                      { Loc { locInfo = TkKeyword KwType } }
  var                       { Loc { locInfo = TkKeyword KwVar } }
  while                     { Loc { locInfo = TkKeyword KwWhile } }
  ";"                       { Loc { locInfo = TkSymbol SymSemicolon } }
  "("                       { Loc { locInfo = TkSymbol SymLParen } }
  ")"                       { Loc { locInfo = TkSymbol SymRParen } }
  "["                       { Loc { locInfo = TkSymbol SymLBrack } }
  "]"                       { Loc { locInfo = TkSymbol SymRBrack } }
  "{"                       { Loc { locInfo = TkSymbol SymLBrace } }
  "}"                       { Loc { locInfo = TkSymbol SymRBrace } }
  ":="                      { Loc { locInfo = TkSymbol SymAssign } }
  ","                       { Loc { locInfo = TkSymbol SymComma } }
  ":"                       { Loc { locInfo = TkSymbol SymColon } }
  "<>"                      { Loc { locInfo = TkSymbol SymNeq } }
  "<"                       { Loc { locInfo = TkSymbol SymLt } }
  "<="                      { Loc { locInfo = TkSymbol SymLe } }
  ">"                       { Loc { locInfo = TkSymbol SymGt } }
  ">="                      { Loc { locInfo = TkSymbol SymGe } }
  "="                       { Loc { locInfo = TkSymbol SymEq } }
  "&"                       { Loc { locInfo = TkSymbol SymAnd } }
  "|"                       { Loc { locInfo = TkSymbol SymOr } }
  "."                       { Loc { locInfo = TkSymbol SymDot } }
  "+"                       { Loc { locInfo = TkSymbol SymPlus } }
  "-"                       { Loc { locInfo = TkSymbol SymMinus } }
  "*"                       { Loc { locInfo = TkSymbol SymTimes } }
  "/"                       { Loc { locInfo = TkSymbol SymDiv } }
  integer                   { Loc { locInfo = TkLiteral (LitInteger _) } }
  string                    { Loc { locInfo = TkLiteral (LitString _) } }
  ident                     { Loc { locInfo = TkIdent _ } }

%nonassoc function var type then do of ":="
%nonassoc else
%left "+" "-"
%left "*" "/"
%nonassoc ">=" "<=" "=" "<>" "<" ">"
%left "|"
%left "&"
%left NEG

%%

RecordFields :: { [(Ident, Expr)]}
RecordFields
  : {- empty -}                                 { [] }
  | ident "=" Expr                              { [(getIdent $1, $3)] }
  | RecordFields "," ident "=" Expr             { foldr (:) [(getIdent $3, $5)] $1 }

Ty :: { Ty }
Ty
  : ident                                       { TyAlias (withSpan1 $1) (getIdent $1) }   -- Type alias.
  | "{" TyFields "}"                            { TyRecord (withSpan2 $1 $3) $2 }               -- Record type definition.
  | array of ident                              { TyArray (withSpan2 $1 $3) (getIdent $3) }   -- Array type definition.

TyFields :: { [TyField] }
TyFields
  : {- empty -}                                 { [] }
  | ident ":" ident                             { [TyField (withSpan2 $1 $3) (getIdent $1) (getIdent $3)] }
  | TyFields "," ident ":" ident                { foldr (:) [TyField (withSpan1 $3 <> withSpan1 $5) (getIdent $3) (getIdent $5)] $1 }

VarDec :: { Dec }
VarDec
  : var ident ":" ident ":=" Expr               { VarDec (withSpan2 $1 $6) (getIdent $2) ((Just . getIdent) $4) $6 }
  | var ident ":=" Expr                         { VarDec (withSpan2 $1 $4) (getIdent $2) Nothing $4 }

FunctDec :: { Dec }
FunctDec
  : function ident "(" TyFields ")" ":" ident "=" Expr      { FunctDec (withSpan2 $1 $9) (getIdent $2) $4 (Just (getIdent $7)) $9 }
  | function ident "(" TyFields ")" "=" Expr                { FunctDec (withSpan2 $1 $7) (getIdent $2) $4 Nothing $7 }

Dec :: { Dec }
Dec
  : type ident "=" Ty             { TypeDec (withSpan2 $1 $4) (getIdent $2) $4 }  -- Type declaration.
  | VarDec                        { $1 }                                          -- Variable declaration.
  | FunctDec                      { $1 }                                          -- Function declaration.

Decs :: { [Dec] }
Decs
  : {- empty -}                   { [] }
  | Decs Dec                      { foldr (:) [$2] $1 }

LValue :: { LValue }
LValue
  : ident                         { LId (withSpan1 $1) (getIdent $1) }
  | LValue2                       { $1 }

LValue2 :: { LValue }
LValue2
  : ident "." ident               { LRecord (withSpan2 $1 $3) (LId (withSpan1 $1) (getIdent $1)) (getIdent $3) }
  | LValue2 "." ident             { LRecord (withSpan2 $1 $3) $1 (getIdent $3) }
  | ident "[" Expr "]"            { LArrayIndex (withSpan2 $1 $4) (LId (withSpan1 $1) (getIdent $1)) $3 }
  | LValue2 "[" Expr "]"          { LArrayIndex (withSpan2 $1 $4) $1 $3 }

Args :: { [Expr] }
Args
  : {- empty -}                   { [] }
  | Expr                          { [$1] }
  | Args "," Expr                 { foldr (:) [$3] $1 }

Op :: { Expr }
Op
  : Expr "+" Expr                 { EOp (withSpan2 $1 $3) $1 Plus $3 }
  | Expr "-" Expr                 { EOp (withSpan2 $1 $3) $1 Minus $3 }
  | Expr "*" Expr                 { EOp (withSpan2 $1 $3) $1 Times $3 }
  | Expr "/" Expr                 { EOp (withSpan2 $1 $3) $1 Div $3 }
  | Expr "&" Expr                 { EOp (withSpan2 $1 $3) $1 And $3 }
  | Expr "|" Expr                 { EOp (withSpan2 $1 $3) $1 Or $3 }
  | Expr "=" Expr                 { EOp (withSpan2 $1 $3) $1 Eq $3 }
  | Expr ">" Expr                 { EOp (withSpan2 $1 $3) $1 Gt $3 }
  | Expr "<" Expr                 { EOp (withSpan2 $1 $3) $1 Lt $3 }
  | Expr ">=" Expr                { EOp (withSpan2 $1 $3) $1 Ge $3 }
  | Expr "<=" Expr                { EOp (withSpan2 $1 $3) $1 Le $3 }
  | Expr "<>" Expr                { EOp (withSpan2 $1 $3) $1 NEq $3 }
  | "-" Expr %prec NEG            { EOp (withSpan2 $1 $2) (EInteger (withSpan1 $2) 0) Minus $2 }

Literal :: { Expr }
Literal
  : string                        { EString (withSpan1 $1) (getString $1) }
  | integer                       { EInteger (withSpan1 $1) (getInteger $1) }
  | nil                           { ENil (withSpan1 $1) }

Expr :: { Expr }
Expr
  : Literal                                         { $1 }
  -- Array and record creations.
  | ident "[" Expr "]" of Expr                      { EArray (withSpan2 $1 $6) (getIdent $1) $3 $6 }
  | ident "{" RecordFields "}"                      { ERecord (withSpan2 $1 $4) (getIdent $1) $3 }
  -- Variables, field, elements of an array.
  | LValue                                          { ELValue (withSpan1 $1) $1 }
  -- Function call.
  | ident "(" Args ")"                              { EFunctCall (withSpan2 $1 $4) (getIdent $1) $3 }
  -- Operations.
  | Op                                              { $1 }
  | "(" Exprs ")"                                   { ESeq (withSpan2 $1 $3) $2 }
  -- Assignment.
  | LValue ":=" Expr                                { EAssign (withSpan2 $1 $3) $1 $3 }
  -- Control structures.
  | if Expr then Expr                               { EIf (withSpan2 $1 $4) $2 $4 Nothing }
  | if Expr then Expr else Expr                     { EIf (withSpan2 $1 $6) $2 $4 (Just $6) }
  | while Expr do Expr                              { EWhile (withSpan2 $1 $4) $2 $4 }
  | for ident ":=" Expr to Expr do Expr             { EFor (withSpan2 $1 $8) (getIdent $2) $4 $6 $8 }
  | break                                           { EBreak (withSpan1 $1) }
  | let Decs in Exprs end                           { ELet (withSpan2 $1 $5) $2 $4 }

Exprs :: { [Expr] }
Exprs
  : {- empty -}                   { [] }
  | Expr                          { [$1] }
  | Exprs ";" Expr                { foldr (:) [$3] $1 }

{
getString :: Loc Token -> ByteString
getString (Loc _ (TkLiteral (LitString x)) ) = x

getInteger :: Loc Token -> Integer
getInteger (Loc _ (TkLiteral (LitInteger x))) = x

getIdent :: Loc Token -> Ident
getIdent (Loc _ (TkIdent x)) = x

withSpan1 :: HasSpan a => a -> Span
withSpan1 = getSpan

withSpan2 :: (HasSpan a, HasSpan b) => a -> b -> Span
withSpan2 a b = getSpan a <> getSpan b

happyError :: Parser a
happyError = parseError "Parse error"

runParser :: SrcFile -> ByteString -> Either ParseError Expr
runParser file bs = runP file bs parse
}
