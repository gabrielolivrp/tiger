{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Tiger.Syntax.Parser.Alex where

import Codec.Binary.UTF8.String
import Control.Monad.State
import Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Internal
import Tiger.Syntax.Parser.Monad
import Tiger.Syntax.Position

infixr 5 :<

pattern (:<) :: Byte -> ByteString -> ByteString
pattern b :< bs <- (uncons -> Just (b, bs))

pattern BSEmpty :: ByteString
pattern BSEmpty <- (uncons -> Nothing)

data AlexInput = AlexInput
  { -- | Source file
    lexSrcFile :: !SrcFile,
    -- | Current position.
    lexPos :: !Position,
    -- | Previously read character.
    lexPrevChar :: !Char,
    -- | rest of the bytes for the current char
    lexRestOfBytes :: [Byte],
    -- | Current input.
    lexInput :: !ByteString
  }
  deriving (Show)

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte (AlexInput f pos prevChar (b : bs) input) =
  Just (b, AlexInput f pos prevChar bs input)
alexGetByte (AlexInput _ _ _ [] BSEmpty) = Nothing
alexGetByte (AlexInput f pos _ [] input) =
  let hInput = BS.head input
      tInput = BS.tail input
      hInputChar = w2c hInput
      pos' = movePos pos hInputChar
   in case encode [hInputChar] of
        b : bs -> pos' `seq` Just (b, AlexInput f pos' hInputChar bs tInput)
        [] -> error $ "Not byte returned for " ++ show hInputChar

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
        lexInput = pInput state,
        lexRestOfBytes = pBytes state
      }

alexSetInput :: AlexInput -> Parser ()
alexSetInput input = modify go
  where
    go state =
      state
        { pSrcFile = lexSrcFile input,
          pPrevChar = lexPrevChar input,
          pPos = lexPos input,
          pInput = lexInput input,
          pBytes = lexRestOfBytes input
        }
