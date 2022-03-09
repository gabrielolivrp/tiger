module Main where

import qualified Data.ByteString.Char8 as BC
import Tiger.Syntax.Error.ParseError
import Tiger.Syntax.Parser.Lexer

main :: IO ()
main = do
  let input = BC.pack "let foo :=\"bar\" in print(foo) end"
  let result = runLexer Nothing input
  case result of
    Right tokens -> print tokens
    Left err -> (putStrLn . BC.unpack . pprParseError) err
