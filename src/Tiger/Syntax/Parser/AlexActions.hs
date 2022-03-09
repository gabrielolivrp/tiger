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
  TokenInfo
mkToken token pInput cInput =
  let pPos = lexPos pInput
      cPos = lexPos cInput
   in TokenInfo
        { info = token,
          startPos = updateCol (- 1) pPos,
          endPos = updateCol (- 1) cPos
        }

action :: Token -> AlexAction TokenInfo
action token pInput cInput _ =
  return $ mkToken token pInput cInput

symbol :: Symbol -> AlexAction TokenInfo
symbol s = action (TkSymbol s)

keyword :: Keyword -> AlexAction TokenInfo
keyword s = action (TkKeyword s)

identifier :: AlexAction TokenInfo
identifier pInput@AlexInput {lexInput = input} cInput tokenLength = do
  let token = TkId (BS.take tokenLength input)
  return $ mkToken token pInput cInput

literal :: (a -> Literal) -> (ByteString -> a) -> AlexAction TokenInfo
literal lit fun pInput@AlexInput {lexInput = input} cInput tokenLength = do
  let token = TkLiteral $ lit (fun (BS.take tokenLength input))
  return $ mkToken token pInput cInput

integer :: ByteString -> Integer
integer = read . BC.unpack

alexEof :: Parser TokenInfo
alexEof = do
  (AlexInput _ pos _ _ _) <- alexGetInput
  return $
    TokenInfo
      { info = TkEof,
        startPos = pos,
        endPos = pos
      }
