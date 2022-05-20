{
{-# LANGUAGE PatternSynonyms #-}

module Tiger.Syntax.Parser (runParser) where

import Data.ByteString (ByteString)
import Tiger.Syntax.Parser.Ast
import Tiger.Syntax.Parser.Token
import Tiger.Syntax.Parser.Lexer
import Tiger.Syntax.Parser.Monad
import Tiger.Syntax.Position
import Tiger.Syntax.Error
}
-- | Syntactic Specifications
-- References: https://www.lrde.epita.fr/~tiger/tiger.split/Syntactic-Specifications.html#Syntactic-Specifications

%name parse Expr

%monad { Parser }
%tokentype { Loc Token }
%lexer { lexer } { PToken TkEof }

%token
  array                     { PToken KwArray }
  break                     { PToken KwBreak }
  do                        { PToken KwDo }
  else                      { PToken KwElse }
  end                       { PToken KwEnd }
  for                       { PToken KwFor }
  function                  { PToken KwFunction }
  if                        { PToken KwIf }
  in                        { PToken KwIn }
  let                       { PToken KwLet }
  nil                       { PToken KwNil }
  of                        { PToken KwOf }
  then                      { PToken KwThen }
  to                        { PToken KwTo }
  type                      { PToken KwType }
  var                       { PToken KwVar }
  while                     { PToken KwWhile }
  ";"                       { PToken SymSemicolon }
  "("                       { PToken SymLParen }
  ")"                       { PToken SymRParen }
  "["                       { PToken SymLBrack }
  "]"                       { PToken SymRBrack }
  "{"                       { PToken SymLBrace }
  "}"                       { PToken SymRBrace }
  ":="                      { PToken SymAssign }
  ","                       { PToken SymComma }
  ":"                       { PToken SymColon }
  "<>"                      { PToken SymNeq }
  "<"                       { PToken SymLt }
  "<="                      { PToken SymLe }
  ">"                       { PToken SymGt }
  ">="                      { PToken SymGe }
  "="                       { PToken SymEq }
  "&"                       { PToken SymAnd }
  "|"                       { PToken SymOr }
  "."                       { PToken SymDot }
  "+"                       { PToken SymPlus }
  "-"                       { PToken SymMinus }
  "*"                       { PToken SymTimes }
  "/"                       { PToken SymDiv }
  integer                   { PToken (LitInteger _) }
  string                    { PToken (LitString _) }
  symbol                    { PToken (TkIdent _) }

%nonassoc function var type then do of ":="
%nonassoc else
%left "+" "-"
%left "*" "/"
%nonassoc ">=" "<=" "=" "<>" "<" ">"
%left "|"
%left "&"
%left NEG

%%

RecordFields :: { [(Symbol, Expr)] }
RecordFields
  : {- empty -}                                 { [] }
  | symbol "=" Expr                             { [(getSymbol $1, $3)] }
  | RecordFields "," symbol "=" Expr            { foldr (:) [(getSymbol $3, $5)] $1 }

Ty :: { Ty }
Ty
  : symbol                                      { TyName (withSpan1 $1) (getSymbol $1) }      -- Type alias.
  | "{" TyFields "}"                            { TyRecord (withSpan2 $1 $3) $2 }             -- Record type definition.
  | array of symbol                             { TyArray (withSpan2 $1 $3) (getSymbol $3) }   -- Array type definition.

TyFields :: { [TyField] }
TyFields
  : {- empty -}                                 { [] }
  | symbol ":" symbol                           { [TyField (withSpan2 $1 $3) (getSymbol $1) (getSymbol $3)] }
  | TyFields "," symbol ":" symbol              { foldr (:) [TyField (withSpan1 $3 <> withSpan1 $5) (getSymbol $3) (getSymbol $5)] $1 }

VarDec :: { Dec }
VarDec
  : var symbol ":" symbol ":=" Expr             { VarDec (withSpan2 $1 $6) (getSymbol $2) ((Just . getSymbol) $4) $6 }
  | var symbol ":=" Expr                        { VarDec (withSpan2 $1 $4) (getSymbol $2) Nothing $4 }

FunctDec :: { Dec }
FunctDec
  : function symbol "(" TyFields ")" ":" symbol "=" Expr     { FunctDec (withSpan2 $1 $9) (getSymbol $2) $4 (Just (getSymbol $7)) $9 }
  | function symbol "(" TyFields ")" "=" Expr                { FunctDec (withSpan2 $1 $7) (getSymbol $2) $4 Nothing $7 }

Dec :: { Dec }
Dec
  : type symbol "=" Ty            { TypeDec (withSpan2 $1 $4) (getSymbol $2) $4 }  -- Type declaration.
  | VarDec                        { $1 }                                          -- Variable declaration.
  | FunctDec                      { $1 }                                          -- Function declaration.

Decs :: { [Dec] }
Decs
  : {- empty -}                   { [] }
  | Decs Dec                      { foldr (:) [$2] $1 }

Var :: { Var }
Var
  : symbol                        { SimpleVar (withSpan1 $1) (getSymbol $1) }
  | VarTail                       { $1 }

VarTail :: { Var }
VarTail
  : symbol "." symbol             { FieldVar (withSpan2 $1 $3) (SimpleVar (withSpan1 $1) (getSymbol $1)) (getSymbol $3) }
  | VarTail "." symbol            { FieldVar (withSpan2 $1 $3) $1 (getSymbol $3) }
  | symbol "[" Expr "]"           { SubscriptVar (withSpan2 $1 $4) (SimpleVar (withSpan1 $1) (getSymbol $1)) $3 }
  | VarTail "[" Expr "]"          { SubscriptVar (withSpan2 $1 $4) $1 $3 }

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
  | symbol "[" Expr "]" of Expr                     { EArray (withSpan2 $1 $6) (getSymbol $1) $3 $6 }
  | symbol "{" RecordFields "}"                     { ERecord (withSpan2 $1 $4) (getSymbol $1) $3 }
  -- Variables, field, elements of an array.
  | Var                                             { EVar (withSpan1 $1) $1 }
  -- Function call.
  | symbol "(" Args ")"                             { EFunctCall (withSpan2 $1 $4) (getSymbol $1) $3 }
  -- Operations.
  | Op                                              { $1 }
  | "(" Exprs ")"                                   { ESeq (withSpan2 $1 $3) $2 }
  -- Assignment.
  | Var ":=" Expr                                   { EAssign (withSpan2 $1 $3) $1 $3 }
  -- Control structures.
  | if Expr then Expr                               { EIf (withSpan2 $1 $4) $2 $4 Nothing }
  | if Expr then Expr else Expr                     { EIf (withSpan2 $1 $6) $2 $4 (Just $6) }
  | while Expr do Expr                              { EWhile (withSpan2 $1 $4) $2 $4 }
  | for symbol ":=" Expr to Expr do Expr            { EFor (withSpan2 $1 $8) (getSymbol $2) $4 $6 $8 }
  | break                                           { EBreak (withSpan1 $1) }
  | let Decs in Exprs end                           { ELet (withSpan2 $1 $5) $2 $4 }

Exprs :: { [Expr] }
Exprs
  : {- empty -}                   { [] }
  | Expr                          { [$1] }
  | Exprs ";" Expr                { foldr (:) [$3] $1 }

{
pattern PToken :: Token -> Loc Token
pattern PToken x <- Loc {locInfo = x}

getString :: Loc Token -> ByteString
getString (Loc _ (LitString x)) = x

getInteger :: Loc Token -> Integer
getInteger (Loc _ (LitInteger x)) = x

getSymbol :: Loc Token -> Symbol
getSymbol (Loc _ (TkIdent x)) = Symbol x

withSpan1 :: HasSpan a => a -> Span
withSpan1 = getSpan

withSpan2 :: (HasSpan a, HasSpan b) => a -> b -> Span
withSpan2 a b = getSpan a <> getSpan b

happyError :: Parser a
happyError = parseError ParseError "Parse error"

runParser :: SrcFilePath -> ByteString -> Either SyntaxError Expr
runParser file bs = runP file bs parse
}
