module Tiger.Syntax.Lexer.AlexActions where

import Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Tiger.Syntax.Lexer.Alex
import Tiger.Syntax.Lexer.Token
import Tiger.Syntax.Position

type CurrentInput = AlexInput

type PreviousInput = AlexInput

type TokenLength = Int

type AlexAction a = PreviousInput -> TokenLength -> Alex a

mkToken ::
  Token ->
  PreviousInput ->
  CurrentInput ->
  TokenInfo
mkToken token prevInput currentInput =
  let (Position pOffset pLine pCol) = lexPos prevInput
      (Position cOffset cLine cCol) = lexPos currentInput
   in TokenInfo
        { info = token,
          startPos = Position pOffset pLine (pCol - 1),
          endPos = Position cOffset cLine (cCol - 1)
        }

action :: Token -> AlexAction TokenInfo
action token prevInput _ = do
  mkToken token prevInput <$> alexGetInput

symbol :: Symbol -> AlexAction TokenInfo
symbol s = action (TkSymbol s)

keyword :: Keyword -> AlexAction TokenInfo
keyword s = action (TkKeyword s)

identifier :: AlexAction TokenInfo
identifier prevInput@AlexInput {lexInput = input} tokenLength = do
  currentInput <- alexGetInput
  let token = TkId (BS.take tokenLength input)
  return $ mkToken token prevInput currentInput

literal :: (a -> Literal) -> (ByteString -> a) -> AlexAction TokenInfo
literal lit fun prevInput@AlexInput {lexInput = input} tokenLength = do
  currentInput <- alexGetInput
  let token = TkLiteral $ lit (fun (BS.take tokenLength input))
  return $ mkToken token prevInput currentInput

alexEof :: Alex TokenInfo
alexEof = do
  (AlexInput _ pos _ _ _) <- alexGetInput
  return $
    TokenInfo
      { info = TkEof,
        startPos = pos,
        endPos = pos
      }

integer :: ByteString -> Integer
integer = read . BC.unpack
