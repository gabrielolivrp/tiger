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
  integer                   { Loc { locInfo = TkLiteral (LitInteger $$) } }
  string                    { Loc { locInfo = TkLiteral (LitString $$) } }
  ident                     { Loc { locInfo = TkId $$ } }

%nonassoc function var type then do of ":="
%nonassoc else
%left "+" "-"
%left "*" "/"
%nonassoc ">=" "<=" "=" "<>" "<" ">"
%left "|"
%left "&"
%left NEG

%%

RecordFields :: { [(Id, Expr)]}
RecordFields
  : {- empty -}                       { [] }
  | ident "=" Expr                    { [($1, $3)] }
  | RecordFields "," ident "=" Expr   { foldr (:) [($3, $5)] $1 }

Ty :: { Ty }
Ty
  : ident                             { TyAlias $1 }  -- Type alias.
  | "{" TyFields "}"                  { TyRecord $2 } -- Record type definition.
  | array of ident                    { TyArray $3 }  -- Array type definition.

TyFields :: { [TyField] }
TyFields
  : {- empty -}                       { [] }
  | ident ":" ident                   { [TyField $1 $3] }
  | TyFields "," ident ":" ident      { foldr (:) [TyField $3 $5] $1 }

VarDec :: { Dec }
VarDec
  : var ident ":" ident ":=" Expr     { DecVar $2 (Just $4) $6 }
  | var ident ":=" Expr               { DecVar $2 Nothing $4 }

DecFunct :: { Dec }
DecFunct
  : function ident "(" TyFields ")" ":" ident "=" Expr      { DecFunct $2 $4 (Just $7) $9 }
  | function ident "(" TyFields ")" "=" Expr                { DecFunct $2 $4 Nothing $7 }

Dec :: { Dec }
Dec
  : type ident "=" Ty             { DecType $2 $4 } -- Type declaration.
  | VarDec                        { $1 }            -- Variable declaration.
  | DecFunct                      { $1 }            -- Function declaration.

Decs :: { [Dec] }
Decs
  : {- empty -}                   { [] }
  | Decs Dec                      { foldr (:) [$2] $1 }

LValue :: { LValue }
LValue
  : ident                         { LId $1 }
  | LValue2                       { $1 }

LValue2 :: { LValue }
LValue2
  : ident "." ident               { LRecord (LId $1) $3 }
  | LValue2 "." ident             { LRecord $1 $3 }
  | ident "[" Expr "]"            { LArrayIndex (LId $1) $3 }
  | LValue2 "[" Expr "]"          { LArrayIndex $1 $3 }

Args :: { [Expr] }
Args
  : {- empty -}                   { [] }
  | Expr                          { [$1] }
  | Args "," Expr                 { foldr (:) [$3] $1 }

Op :: { Expr }
Op
  : Expr "+" Expr                 { EOp $1 Plus $3 }
  | Expr "-" Expr                 { EOp $1 Minus $3 }
  | Expr "*" Expr                 { EOp $1 Times $3 }
  | Expr "/" Expr                 { EOp $1 Div $3 }
  | Expr "&" Expr                 { EOp $1 And $3 }
  | Expr "|" Expr                 { EOp $1 Or $3 }
  | Expr "=" Expr                 { EOp $1 Eq $3 }
  | Expr ">" Expr                 { EOp $1 Gt $3 }
  | Expr "<" Expr                 { EOp $1 Lt $3 }
  | Expr ">=" Expr                { EOp $1 Ge $3 }
  | Expr "<=" Expr                { EOp $1 Le $3 }
  | Expr "<>" Expr                { EOp $1 NEq $3 }
  | "-" Expr %prec NEG            { EOp (EInteger 0) Minus $2 }

Expr :: { Expr }
Expr
  -- Literals.
  : nil                                             { ENil }
  | string                                          { EString $1 }
  | integer                                         { EInteger $1 }
  -- Array and record creations.
  | ident "[" Expr "]" of Expr                      { EArray $1 $3 $6 }
  | ident "{" RecordFields "}"                      { ERecord $1 $3 }
  -- Variables, field, elements of an array.
  | LValue                                          { ELValue $1 }
  -- Function call.
  | ident "(" Args ")"                              { EFunctCall $1 $3 }
  -- Operations.
  | Op                                              { $1 }
  | "(" Exprs ")"                                   { ESeq $2 }
  -- Assignment.
  | LValue ":=" Expr                                { EAssign $1 $3 }
  -- Control structures.
  | if Expr then Expr                               { EIf $2 $4 Nothing }
  | if Expr then Expr else Expr                     { EIf $2 $4 (Just $6) }
  | while Expr do Expr                              { EWhile $2 $4 }
  | for ident ":=" Expr to Expr do Expr             { EFor $2 $4 $6 $8 }
  | break                                           { EBreak }
  | let Decs in Exprs end                           { ELet $2 $4 }

Exprs :: { [Expr] }
Exprs
  : {- empty -}                   { [] }
  | Expr                          { [$1] }
  | Exprs ";" Expr                { foldr (:) [$3] $1 }

{
happyError :: Parser a
happyError = parseError "Parse error"

runParser :: SrcFile -> ByteString -> Either ParseError Expr
runParser file bs = runP file bs parse
}
