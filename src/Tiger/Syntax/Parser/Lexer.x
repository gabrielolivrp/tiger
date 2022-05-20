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
import Tiger.Syntax.Parser.Alex
import Tiger.Syntax.Parser.AlexActions
import Tiger.Syntax.Parser.Monad
import Tiger.Syntax.Parser.Token
import Tiger.Syntax.Position
import Tiger.Syntax.Error
import Control.Monad.State
}

$alpha = [a-zA-Z]
$newline = [\n\r]
$digit = 0-9

@integer = [$digit]+
@identifier = $alpha [$alpha $digit \_]*
-- References for comments implementation
-- https://github.com/wasp-lang/wasp/blob/main/waspc/src/Wasp/Analyzer/Parser/Lexer.x#L32,L33
@linecomment = "//" [^\n\r]*
@blockcomment = "/*" (("*"[^\/]) | [^\*] | $white)* "*/"

tiger :-
<0> $white+                   ;
<0> $newline+                 ;
<0> @linecomment              ;
<0> @blockcomment             ;
<0> "array"                   { action KwArray }
<0> "break"                   { action KwBreak }
<0> "do"                      { action KwDo }
<0> "else"                    { action KwElse }
<0> "end"                     { action KwEnd }
<0> "for"                     { action KwFor }
<0> "function"                { action KwFunction }
<0> "if"                      { action KwIf }
<0> "in"                      { action KwIn }
<0> "let"                     { action KwLet }
<0> "nil"                     { action KwNil }
<0> "of"                      { action KwOf }
<0> "then"                    { action KwThen }
<0> "to"                      { action KwTo }
<0> "type"                    { action KwType }
<0> "var"                     { action KwVar }
<0> "while"                   { action KwWhile }
<0> ";"                       { action SymSemicolon }
<0> "("                       { action SymLParen }
<0> ")"                       { action SymRParen }
<0> "["                       { action SymLBrack }
<0> "]"                       { action SymRBrack }
<0> "{"                       { action SymLBrace }
<0> "}"                       { action SymRBrace }
<0> ":="                      { action SymAssign }
<0> ","                       { action SymComma }
<0> ":"                       { action SymColon }
<0> "<>"                      { action SymNeq }
<0> "<"                       { action SymLt }
<0> "<="                      { action SymLe }
<0> ">"                       { action SymGt }
<0> ">="                      { action SymGe }
<0> "="                       { action SymEq }
<0> "&"                       { action SymAnd }
<0> "|"                       { action SymOr }
<0> "."                       { action SymDot }
<0> "+"                       { action SymPlus }
<0> "-"                       { action SymMinus }
<0> "*"                       { action SymTimes }
<0> "/"                       { action SymDiv }
<0> @integer                  { literal LitInteger integer }
<0> @identifier               { identifier }
-- References for strings implementation
-- https://github.com/amuletml/amulet/blob/021460acaf016f5c31072336cd3253218a3093bf/src/Parser/Lexer.x#L166
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
<string> [^\\\"]              { onStringM appendString }

{
lexer' :: Parser (Loc Token)
lexer' = do
  input <- alexGetInput
  startCode <- getStartCode
  case alexScan input startCode of
    AlexEOF -> alexEof
    AlexError _ -> parseError LexerError "Lexical error"
    AlexSkip rest _len -> do
      alexSetInput rest
      lexer'
    AlexToken rest len action -> do
      alexSetInput rest
      action input rest len

lexer :: (Loc Token -> Parser a) -> Parser a
lexer = (lexer' >>=)

runLexer :: SrcFilePath -> ByteString -> Either SyntaxError [Loc Token]
runLexer file bs = runP file bs go
  where
    go = do
      token@Loc {..} <- lexer'
      case locInfo of
        TkEof -> return []
        _ -> (token :) <$> go

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
  return $  Loc (Span start end) (LitString buffer)

appendString :: ByteString -> AlexAction (Loc Token)
appendString str _ _ _ = do
  modify (\state -> state {pStringBuffer = pStringBuffer state <> str})
  state <- get
  lexer'

onStringM :: (ByteString -> AlexAction a) -> AlexAction a
onStringM f pInput cInput tokenLength =
  let input = lexText pInput
   in f (BS.take tokenLength input) pInput cInput tokenLength
}
