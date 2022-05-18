module Tiger.Syntax.Parser.AlexActions where

import Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Tiger.Syntax.Parser.Alex
import Tiger.Syntax.Parser.Monad
import Tiger.Syntax.Parser.Token
import Tiger.Syntax.Position

type CurrentInput = AlexInput

type PreviousInput = AlexInput

type TokenLength = Int

type AlexAction a =
  PreviousInput ->
  CurrentInput ->
  TokenLength ->
  Parser a

mkToken ::
  Token ->
  PreviousInput ->
  CurrentInput ->
  Loc Token
mkToken token pInput cInput =
  let pPos = lexPos pInput
      cPos = lexPos cInput
      start = updateCol (- 1) pPos
      end = updateCol (- 1) cPos
   in Loc (Span start end) token

action :: Token -> AlexAction (Loc Token)
action token pInput cInput _ =
  return $ mkToken token pInput cInput

symbol :: Symbol -> AlexAction (Loc Token)
symbol s = action (TkSymbol s)

keyword :: Keyword -> AlexAction (Loc Token)
keyword s = action (TkKeyword s)

identifier :: AlexAction (Loc Token)
identifier pInput cInput tokenLength =
  let text = lexText pInput
      token = (TkIdent . BS.take tokenLength) text
   in return $ mkToken token pInput cInput

literal ::
  (a -> Literal) ->
  (ByteString -> a) ->
  AlexAction (Loc Token)
literal lit fun pInput cInput tokenLength =
  let text = lexText pInput
      token = (TkLiteral . lit . fun) (BS.take tokenLength text)
   in return $ mkToken token pInput cInput

integer :: ByteString -> Integer
integer = read . BC.unpack

alexEof :: Parser (Loc Token)
alexEof = do
  AlexInput {lexPos = pos} <- alexGetInput
  return $ Loc (Span pos pos) TkEof
