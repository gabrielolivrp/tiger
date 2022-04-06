{
{-# LANGUAGE RecordWildCards #-}

module Tiger.Syntax.Parser.Lexer
  ( runLexer,
    lexer,
  )
where

import Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.List as L
import Tiger.Syntax.Error.ParseError
import Tiger.Syntax.Parser.Alex
import Tiger.Syntax.Parser.AlexActions
import Tiger.Syntax.Parser.Monad
import Tiger.Syntax.Parser.Token
import Tiger.Syntax.Position
import Control.Monad.State
}

$alpha = [a-zA-Z]
$newline = [\n\r]
$digit = 0-9

@integer = [$digit]+
@identifier = $alpha [$alpha $digit \_]*
-- References: https://github.com/wasp-lang/wasp/blob/main/waspc/src/Wasp/Analyzer/Parser/Lexer.x
@linecomment = "//" [^\n\r]*
@blockcomment = "/*" (("*"[^\/]) | [^\*] | $white)* "*/"

tiger :-
<0> $white+                   ;
<0> $newline+                 ;
<0> @linecomment              ;
<0> @blockcomment             ;
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
<0> @identifier               { identifier }
<0>      \"                   { beginString }
<string> \"                   { endString }
<string> \\ a                 { appendString "\a" }
<string> \\ b                 { appendString "\b" }
<string> \\ f                 { appendString "\f" }
<string> \\ n                 { appendString "\n" }
<string> \\ \n                { appendString "\n" }
<string> \\ r                 { appendString "\r" }
<string> \\ v                 { appendString "\v" }
<string> \\ t                 { appendString "\t" }
<string> \\ \\                { appendString "\\" }
<string> \\ \"                { appendString "\"" }
<string> [^\\\"]              { onStringM appendString}

{
lexer' :: Parser (Loc Token)
lexer' = do
  input <- alexGetInput
  startCode <- getStartCode
  case alexScan input startCode of
    AlexEOF -> alexEof
    AlexError _ -> parseError "Lexical error"
    AlexSkip rest _len -> do
      alexSetInput rest
      lexer'
    AlexToken rest len action -> do
      alexSetInput rest
      action input rest len

lexer :: (Loc Token -> Parser a) -> Parser a
lexer = (lexer' >>=)

runLexer :: SrcFile -> ByteString -> Either ParseError [Loc Token]
runLexer file bs = runP file bs go
  where
    go = do
      token@Loc {..} <- lexer'
      case locInfo of
        TkEof -> return []
        _ -> (token :) <$> go

-- References: https://github.com/amuletml/amulet/blob/021460acaf016f5c31072336cd3253218a3093bf/src/Parser/Lexer.x#L166
beginString :: AlexAction (Loc Token)
beginString pInput cInput tokenLength = do
  let pos = lexPos pInput
  modify (\state -> state {pTkStartPos = pos})
  setStartCode string
  lexer'

endString :: AlexAction (Loc Token)
endString pInput _ _ = do
  buffer <- gets pStringBuffer
  startPos <- gets pTkStartPos
  let endPos = lexPos pInput
  let start = updateCol (- 1) startPos
  let end = updateCol (- 1) endPos
  modify (\state -> state {pStartCode = 0, pStringBuffer = ""})
  return $  Loc (Span start end) (TkLiteral $ LitString buffer)

appendString :: ByteString -> AlexAction (Loc Token)
appendString str _ _ _ = do
  modify (\state -> state {pStringBuffer = pStringBuffer state <> str})
  state <- get
  lexer'

onStringM :: (ByteString -> AlexAction a) -> AlexAction a
onStringM f pInput cInput tokenLength =
  let input = lexInput pInput
   in f (BS.take tokenLength input) pInput cInput tokenLength
}
