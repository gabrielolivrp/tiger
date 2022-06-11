{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tiger.Syntax.Parser.Monad where

import Control.Monad.Except
import Control.Monad.State
import Data.ByteString
import qualified Data.ByteString as BC
import Tiger.Syntax.Error
import Tiger.Syntax.Position

data ParseState = ParseState
  { -- | file path
    pSrcFilePath :: !SrcFilePath,
    -- | position at current input location
    pPos :: !Position,
    -- | the current text of the source file
    pText :: !ByteString,
    -- | the character before the input
    pPrevChar :: !Char,
    -- | String Buffers
    pStringBuffer :: !ByteString,
    -- | Current startcode
    pStartCode :: !Int,
    -- | Token start position
    pTkStartPos :: !Position
  }

initParseState :: SrcFilePath -> ByteString -> ParseState
initParseState path text =
  ParseState
    { pSrcFilePath = path,
      pPos = initPos,
      pText = text,
      pPrevChar = '\n',
      pStringBuffer = "",
      pStartCode = 0,
      pTkStartPos = initPos
    }

newtype Parser a = Parser
  { unP :: StateT ParseState (Except SyntaxError) a
  }
  deriving (Functor, Applicative, Monad, MonadState ParseState, MonadError SyntaxError)

runP :: SrcFilePath -> ByteString -> Parser a -> Either SyntaxError a
runP file text parse =
  let state = initParseState file text
      parse' = unP parse
   in runExcept $ evalStateT parse' state

setStartCode :: Int -> Parser ()
setStartCode startCode = modify (\state -> state {pStartCode = startCode})

getStartCode :: Parser Int
getStartCode = gets pStartCode

parseError :: SyntaxErrorKind -> ByteString -> Parser a
parseError errKind msg = do
  state <- get
  throwError
    SyntaxError
      { pErrorKind = errKind,
        pErrText = pText state,
        pErrSrcFile = pSrcFilePath state,
        pErrPos = pPos state,
        pErrMsg = msg
      }
