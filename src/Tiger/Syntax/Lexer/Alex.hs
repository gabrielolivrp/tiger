{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Tiger.Syntax.Lexer.Alex where

import Codec.Binary.UTF8.String
import Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Internal
import Data.Word
import Tiger.Syntax.Error.ParseError
import Tiger.Syntax.Position

type Byte = Word8

infixr 5 :<

pattern (:<) :: Word8 -> ByteString -> ByteString
pattern b :< bs <- (uncons -> Just (b, bs))

pattern BSEmpty :: ByteString
pattern BSEmpty <- (uncons -> Nothing)

type SrcFile = Maybe FilePath

data AlexInput = AlexInput
  { lexSrcFile :: !SrcFile,
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

data AlexState = AlexState
  { -- | File
    lexStateSrcFile :: !SrcFile,
    -- |  position at current input location
    lexStatePos :: !Position,
    -- | the current input
    lexStateInput :: !ByteString,
    -- | the character before the input
    lexStatePrevChar :: !Char,
    -- | rest of the bytes for the current char
    lexStateBytes :: [Byte]
  }

newtype Alex a = Alex
  { unAlex :: AlexState -> Either ParseError (AlexState, a)
  }

instance Functor Alex where
  fmap f a = Alex $
    \state ->
      case unAlex a state of
        Left err -> Left err
        Right (state', a') -> Right (state', f a')

instance Applicative Alex where
  pure a = Alex $ \state -> Right (state, a)
  fa <*> alex = Alex $
    \state ->
      case unAlex fa state of
        Left err -> Left err
        Right (state', f) ->
          case unAlex alex state' of
            Left err -> Left err
            Right (state'', b) -> Right (state'', f b)

instance Monad Alex where
  return = pure
  m >>= k = Alex $
    \state ->
      case unAlex m state of
        Left err -> Left err
        Right (state', a) -> unAlex (k a) state'

alexGetInput :: Alex AlexInput
alexGetInput = Alex $ \state ->
  let pos = lexStatePos state
      srcFile = lexStateSrcFile state
      prevChar = lexStatePrevChar state
      bytes = lexStateBytes state
      input = lexStateInput state
   in Right (state, AlexInput srcFile pos prevChar bytes input)

alexSetInput :: AlexInput -> Alex ()
alexSetInput (AlexInput srcFile pos prevChar bytes input) =
  Alex $ \state ->
    let state' =
          state
            { lexStatePos = pos,
              lexStateSrcFile = srcFile,
              lexStatePrevChar = prevChar,
              lexStateBytes = bytes,
              lexStateInput = input
            }
     in Right (state', ())

alexError :: ByteString -> Alex a
alexError msg =
  Alex $ \state ->
    let parseErr =
          ParseError
            { errSrcFile = lexStateSrcFile state,
              errPos = lexStatePos state,
              errMsg = msg
            }
     in Left parseErr

initAlexState :: SrcFile -> ByteString -> AlexState
initAlexState srcFile input =
  AlexState
    { lexStateSrcFile = srcFile,
      lexStatePos = initPos,
      lexStateInput = input,
      lexStatePrevChar = '\n',
      lexStateBytes = []
    }
