module Tiger.Syntax.Parser.Alex where

import Control.Monad.State
import Data.ByteString
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Internal
import Data.Word (Word8)
import Tiger.Syntax.Parser.Monad
import Tiger.Syntax.Position

type Byte = Word8

data AlexInput = AlexInput
  { -- | Source file path.
    lexSrcFilePath :: !SrcFilePath,
    -- | Current position.
    lexPos :: !Position,
    -- | Previously read character.
    lexPrevChar :: !Char,
    -- | Current text.
    lexText :: !ByteString
  }
  deriving (Show)

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte (AlexInput file pos prevChar text) = do
  (w, text') <- uncons text
  let w' = w2c w
  let alexInput =
        AlexInput
          { lexSrcFilePath = file,
            lexPos = movePos pos w',
            lexPrevChar = w',
            lexText = text'
          }
  return (w, alexInput)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = lexPrevChar

alexGetInput :: Parser AlexInput
alexGetInput = do
  state <- get
  return
    AlexInput
      { lexSrcFilePath = pSrcFilePath state,
        lexPrevChar = pPrevChar state,
        lexPos = pPos state,
        lexText = pText state
      }

alexSetInput :: AlexInput -> Parser ()
alexSetInput input = modify go
  where
    go state =
      state
        { pSrcFilePath = lexSrcFilePath input,
          pPrevChar = lexPrevChar input,
          pPos = lexPos input,
          pText = lexText input
        }
