{
module Tiger.Syntax.Parser where

import Tiger.Syntax.Parser.Token
import Tiger.Syntax.Parser.Ast
import Data.ByteString (ByteString)
import Tiger.Syntax.Parser.Lexer
import Tiger.Syntax.Parser.Monad
import Tiger.Syntax.Error.ParseError
}
-- | Syntactic Specifications
-- References: https://www.lrde.epita.fr/~tiger/tiger.split/Syntactic-Specifications.html#Syntactic-Specifications

%name parse Expr

%monad { Parser }
%tokentype { TokenInfo }
%errorhandlertype explist
%error { pError }
%lexer { lexer } { TokenInfo TkEof _ _ }

%token
  array                     { (TokenInfo (TkKeyword KwArray) _ _) }
  break                     { (TokenInfo (TkKeyword KwBreak) _ _) }
  do                        { (TokenInfo (TkKeyword KwDo) _ _) }
  else                      { (TokenInfo (TkKeyword KwElse) _ _) }
  end                       { (TokenInfo (TkKeyword KwEnd) _ _) }
  for                       { (TokenInfo (TkKeyword KwFor) _ _) }
  function                  { (TokenInfo (TkKeyword KwFunction) _ _) }
  if                        { (TokenInfo (TkKeyword KwIf) _ _) }
  in                        { (TokenInfo (TkKeyword KwIn) _ _) }
  let                       { (TokenInfo (TkKeyword KwLet) _ _) }
  nil                       { (TokenInfo (TkKeyword KwNil) _ _) }
  of                        { (TokenInfo (TkKeyword KwOf) _ _) }
  then                      { (TokenInfo (TkKeyword KwThen) _ _) }
  to                        { (TokenInfo (TkKeyword KwTo) _ _) }
  type                      { (TokenInfo (TkKeyword KwType) _ _) }
  var                       { (TokenInfo (TkKeyword KwVar) _ _) }
  while                     { (TokenInfo (TkKeyword KwWhile) _ _) }
  ";"                       { (TokenInfo (TkSymbol SymSemicolon) _ _) }
  "("                       { (TokenInfo (TkSymbol SymLParen) _ _) }
  ")"                       { (TokenInfo (TkSymbol SymRParen) _ _) }
  "["                       { (TokenInfo (TkSymbol SymLBrack) _ _) }
  "]"                       { (TokenInfo (TkSymbol SymRBrack) _ _) }
  "{"                       { (TokenInfo (TkSymbol SymLBrace) _ _) }
  "}"                       { (TokenInfo (TkSymbol SymRBrace) _ _) }
  ":="                      { (TokenInfo (TkSymbol SymAssign) _ _) }
  ","                       { (TokenInfo (TkSymbol SymComma) _ _) }
  ":"                       { (TokenInfo (TkSymbol SymColon) _ _) }
  "<>"                      { (TokenInfo (TkSymbol SymNeq) _ _) }
  "<"                       { (TokenInfo (TkSymbol SymLt) _ _) }
  "<="                      { (TokenInfo (TkSymbol SymLe) _ _) }
  ">"                       { (TokenInfo (TkSymbol SymGt) _ _) }
  ">="                      { (TokenInfo (TkSymbol SymGe) _ _) }
  "="                       { (TokenInfo (TkSymbol SymEq) _ _) }
  "&"                       { (TokenInfo (TkSymbol SymAnd) _ _) }
  "|"                       { (TokenInfo (TkSymbol SymOr) _ _) }
  "."                       { (TokenInfo (TkSymbol SymDot) _ _) }
  "+"                       { (TokenInfo (TkSymbol SymPlus) _ _) }
  "-"                       { (TokenInfo (TkSymbol SymMinus) _ _) }
  "*"                       { (TokenInfo (TkSymbol SymTimes) _ _) }
  "/"                       { (TokenInfo (TkSymbol SymDiv) _ _) }
  integer                   { (TokenInfo (TkLiteral (LitInteger $$)) _ _) }
  string                    { (TokenInfo (TkLiteral (LitString $$)) _ _) }
  identifier                { (TokenInfo (TkId $$) _ _) }


%nonassoc function var type then do of ":="
%nonassoc else
%left "+" "-"
%left "*" "/"
%nonassoc ">=" "<=" "=" "<>" "<" ">"
%left "|"
%left "&"

%%

Id :: { Id }
Id
  : identifier                    { $1 }

TyId :: { TyId }
TyId
  : identifier                    { $1 }

RecordFields :: { [(TyId, Expr)]}
RecordFields
  : {- empty -}                   { [] }
  | Id "=" Expr                   { [($1, $3)] }
  | RecordFields "," Id "=" Expr  { foldr (:) [($3, $5)] $1 }

Ty :: { Ty }
Ty
  : Id                            { TyAlias $1 }  -- Type alias.
  | "{" TyFields "}"              { TyRecord $2 } -- Record type definition.
  | array of Id                   { TyArray $3 }  -- Array type definition.

TyFields :: { [TyField] }
TyFields
  : {- empty -}                   { [] }
  | Id ":" TyId                   { [TyField $1 $3] }
  | TyFields "," Id ":" TyId      { foldr (:) [TyField $3 $5] $1 }

VarDec :: { Dec }
VarDec
  : var Id ":" TyId ":=" Expr     { DecVar $2 (Just $4) $6 }
  | var Id ":=" Expr              { DecVar $2 Nothing $4 }

DecFunct :: { Dec }
DecFunct
  : function Id "(" TyFields ")" ":" TyId "=" Expr      { DecFunct $2 $4 (Just $7) $9 }
  | function Id "(" TyFields ")" "=" Expr               { DecFunct $2 $4 Nothing $7 }

Dec :: { Dec }
Dec
  : type Id "=" Ty                { DecType $2 $4 } -- Type declaration.
  | VarDec                        { $1 }            -- Variable declaration.
  | DecFunct                      { $1 }            -- Function declaration.

Decs :: { [Dec] }
Decs
  : {- empty -}                   { [] }
  | Decs Dec                      { foldr (:) [$2] $1 }

LValue :: { LValue }
LValue
  : Id                            { LId $1 }
  | LValue "." Id                 { LRecord $1 $3}
  | LValue "[" Expr "]"           { LArrayIndex $1 $3 } -- TODO: reduce/reduce conflicts:

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

Expr :: { Expr }
Expr
  -- Literals.
  : nil                                             { ENil }
  | string                                          { EString $1 }
  | integer                                         { EInteger $1 }
  -- Array and record creations.
  | TyId "[" Expr "]" of Expr                       { EArray $1 $3 $6 }
  | TyId "{" RecordFields "}"                       { ERecord $1 $3 }
  -- Variables, field, elements of an array.
  | LValue                                          { ELValue $1 }
  -- Function call.
  | Id "(" Args ")"                                 { EFunctCall $1 $3 }
  -- Operations.
  | Op                                              { $1 }
  | "(" Exprs ")"                                   { ESeq $2 }
  -- Assignment.
  | LValue ":=" Expr                                { EAssign $1 $3 }
  -- Control structures.
  | if Expr then Expr                               { EIf $2 $4 Nothing }
  | if Expr then Expr else Expr                     { EIf $2 $4 (Just $6) }
  | while Expr do Expr                              { EWhile $2 $4 }
  | for Id ":=" Expr to Expr do Expr                { EFor $2 $4 $6 $8 }
  | break                                           { EBreak }
  | let Decs in Exprs end                           { ELet $2 $4 }

Exprs :: { [Expr] }
Exprs
  : {- empty -}                   { [] }
  | Expr                          { [$1] }
  | Exprs ";" Expr                { foldr (:) [$3] $1 }

{
pError :: (TokenInfo, [String]) -> Parser a
pError (tk, rest) =  parseError "Parse error"
-- error $ show tk ++ "==" ++ show rest --
runParser :: SrcFile -> ByteString -> Either ParseError Expr
runParser file bs = runP file bs parse
}

