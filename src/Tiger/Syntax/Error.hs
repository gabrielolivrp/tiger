{-# LANGUAGE LambdaCase #-}

module Tiger.Syntax.Error where

import Data.ByteString
import qualified Data.ByteString.Char8 as BC
import Prettyprinter
import Tiger.Error.RenderError
import Tiger.Syntax.Position

data SyntaxErrorKind
  = ParseError
  | LexerError
  deriving (Show)

data SyntaxError = SyntaxError
  { pErrorKind :: !SyntaxErrorKind,
    pErrSrcFile :: !SrcFilePath,
    pErrPos :: !Position,
    pErrMsg :: !ByteString,
    pErrText :: !ByteString
  }
  deriving (Show)

pprFile :: SrcFilePath -> Doc ann
pprFile = \case
  Just file -> pretty file <> colon
  Nothing -> emptyDoc

errorTitle :: SyntaxErrorKind -> Doc ann
errorTitle = \case
  ParseError -> "Parse Error"
  LexerError -> "Lexer Error"

instance Pretty SyntaxError where
  pretty (SyntaxError errType errSrcFile errPos errMsg errText) =
    vsep
      [ errorTitle errType,
        pprFile errSrcFile <> pretty errPos <> colon <+> pretty (BC.unpack errMsg)
      ]

instance RenderError SyntaxError where
  renderError = BC.pack . show . pretty
