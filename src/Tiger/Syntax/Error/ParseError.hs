{-# LANGUAGE LambdaCase #-}

module Tiger.Syntax.Error.ParseError
  ( pprParseError,
    ParseError (..),
  )
where

import Data.ByteString
import qualified Data.ByteString.Char8 as BC
import Prettyprinter
import Tiger.Syntax.Position

data ParseError = ParseError
  { errSrcFile :: Maybe FilePath,
    errPos :: !Position,
    errMsg :: ByteString
  }
  deriving (Show)

pprFile :: Maybe FilePath -> Doc ann
pprFile = \case
  Just file -> pretty file <> colon
  Nothing -> emptyDoc

instance Pretty ParseError where
  pretty (ParseError errSrcFile errPos errMsg) =
    let pprPos = pprFile errSrcFile <> pretty errPos <> colon
     in pprPos <+> pretty (BC.unpack errMsg)

pprParseError :: ParseError -> ByteString
pprParseError = BC.pack . show . pretty
