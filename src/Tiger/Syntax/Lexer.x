{
{-# LANGUAGE RecordWildCards #-}

module Tiger.Syntax.Lexer (runLexer) where

import Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.List as L
import Tiger.Syntax.Error.ParseError
import Tiger.Syntax.Lexer.Alex
import Tiger.Syntax.Lexer.AlexActions
import Tiger.Syntax.Lexer.Token
}

-- References: https://github.com/haskell/alex/blob/master/examples/haskell.x

$space = [\t\v\f\ ]
$newline = [\n\r]
$digit = 0-9
$whitechar = [ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\`\{\}]

$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]
$graphic   = [$small $large $digit $special \:\"\']

$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
	 | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
	 | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
	 | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]

@escape  = \\ ($charesc | @ascii )
@gap     = \\ $whitechar+ \\

@string  = $graphic # [\"\\] | " " | @escape | @gap
@integer = \-?[$digit]+
@identifier = $alpha [$alpha $digit \_]*

-- TODO: Add comment support
tiger :-

<0> $space+                   ;
<0> $newline+                 ;
<0> "array"                   { keyword KwArray }
<0> "break"                   { keyword KwBreak }
<0> "do"                      { keyword KwDo }
<0> "else"                    { keyword KwElse }
<0> "end"                     { keyword KwEnd }
<0> "for"                     { keyword KwFor }
<0> "function"                { keyword KwFunction }
<0> "if"                      { keyword KwIf }
<0> "in"                      { keyword KwIn }
<0> "let"                     { keyword KwLet }
<0> "nil"                     { keyword KwNil }
<0> "of"                      { keyword KwOf }
<0> "then"                    { keyword KwThen }
<0> "to"                      { keyword KwTo }
<0> "type"                    { keyword KwType }
<0> "var"                     { keyword KwVar }
<0> "while"                   { keyword KwWhile }
<0> ";"                       { symbol SymSemicolon }
<0> "("                       { symbol SymLParen }
<0> ")"                       { symbol SymRParen }
<0> "["                       { symbol SymLBrack }
<0> "]"                       { symbol SymRBrack }
<0> "{"                       { symbol SymLBrace }
<0> "}"                       { symbol SymRBrace }
<0> ":="                      { symbol SymAssign }
<0> ","                       { symbol SymComma }
<0> ":"                       { symbol SymColon }
<0> "<>"                      { symbol SymNeq }
<0> "<"                       { symbol SymLt }
<0> "<="                      { symbol SymLe }
<0> ">"                       { symbol SymGt }
<0> ">="                      { symbol SymGe }
<0> "="                       { symbol SymEq }
<0> "&"                       { symbol SymAnd }
<0> "|"                       { symbol SymOr }
<0> "."                       { symbol SymDot }
<0> "+"                       { symbol SymPlus }
<0> "-"                       { symbol SymMinus }
<0> "*"                       { symbol SymTimes }
<0> "/"                       { symbol SymDiv }
<0> @integer                  { literal LitInteger integer }
<0> \" @string* \"            { literal LitString id }
<0> @identifier               { identifier }

{
lexer :: Alex TokenInfo
lexer = do
  input <- alexGetInput
  case alexScan input 0 of
    AlexEOF -> alexEof
    AlexError _ -> alexError "Lexical error"
    AlexSkip input' _len -> do
      alexSetInput input'
      lexer
    AlexToken input' len action -> do
      alexSetInput input'
      action input len

lexerFold :: Alex [TokenInfo]
lexerFold = go []
  where
    go tokens = do
      token@TokenInfo {..} <- lexer
      case info of
        TkEof -> return $ L.reverse tokens
        _ -> go (token : tokens)

runAlex :: Alex a -> SrcFile -> ByteString -> Either ParseError a
runAlex (Alex f) srcFile input =
  let state = initAlexState srcFile input
   in case f state of
        Left err -> Left err
        Right (_, x) -> Right x

runLexer :: SrcFile -> ByteString -> Either ParseError [TokenInfo]
runLexer = runAlex lexerFold
}
