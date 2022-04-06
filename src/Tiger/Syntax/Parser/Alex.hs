module Tiger.Syntax.Parser.Alex where

import Control.Monad.State
import Data.ByteString
import Data.ByteString.Internal
import Tiger.Syntax.Parser.Monad
import Tiger.Syntax.Position

data AlexInput = AlexInput
  { -- | Source file
    lexSrcFile :: !SrcFile,
    -- | Current position.
    lexPos :: !Position,
    -- | Previously read character.
    lexPrevChar :: !Char,
    -- | Current input.
    lexInput :: !ByteString
  }
  deriving (Show)

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte (AlexInput file pos prevChar inp) =
  case uncons inp of
    Just (hInp, tInp) ->
      let hInpChar = w2c hInp
          alexInput =
            AlexInput
              { lexSrcFile = file,
                lexPos = movePos pos hInpChar,
                lexPrevChar = hInpChar,
                lexInput = tInp
              }
       in Just (hInp, alexInput)
    Nothing -> Nothing

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = lexPrevChar

alexGetInput :: Parser AlexInput
alexGetInput = do
  state <- get
  return $
    AlexInput
      { lexSrcFile = pSrcFile state,
        lexPrevChar = pPrevChar state,
        lexPos = pPos state,
        lexInput = pInput state
      }

alexSetInput :: AlexInput -> Parser ()
alexSetInput input = modify go
  where
    go state =
      state
        { pSrcFile = lexSrcFile input,
          pPrevChar = lexPrevChar input,
          pPos = lexPos input,
          pInput = lexInput input
        }
