{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tiger.Syntax.Parser.Monad where

import Control.Monad.Except
import Control.Monad.State
import Data.ByteString
import Data.Word (Word8)
import Tiger.Syntax.Error.ParseError
import Tiger.Syntax.Position

type SrcFile = Maybe FilePath

type Byte = Word8

data ParseState = ParseState
  { -- | File
    pSrcFile :: !SrcFile,
    -- |  position at current input location
    pPos :: !Position,
    -- | the current input
    pInput :: !ByteString,
    -- | the character before the input
    pPrevChar :: !Char,
    -- | rest of the bytes for the current char
    pBytes :: ![Byte]
  }

initParseState :: SrcFile -> ByteString -> ParseState
initParseState srcFile input =
  ParseState
    { pSrcFile = srcFile,
      pPos = initPos,
      pInput = input,
      pPrevChar = '\n',
      pBytes = []
    }

newtype Parser a = Parser
  { unP :: StateT ParseState (Except ParseError) a
  }
  deriving (Functor, Applicative, Monad, MonadState ParseState, MonadError ParseError)

runP :: SrcFile -> ByteString -> Parser a -> Either ParseError a
runP file bs lex =
  let pState = initParseState file bs
      parse = unP lex
   in runExcept $ evalStateT parse pState

parseError :: ByteString -> Parser a
parseError msg = do
  state <- get
  throwError
    ParseError
      { errSrcFile = pSrcFile state,
        errPos = pPos state,
        errMsg = msg
      }
