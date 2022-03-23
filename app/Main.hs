module Main where

import qualified Data.ByteString.Char8 as BC
import Tiger.Syntax (pprExpr, pprParseError, runParser)

main :: IO ()
main = do
  let filePath = "examples/add.tig"
  contents <- readFile filePath
  case runParser (Just filePath) (BC.pack contents) of
    Right ast -> (putStrLn . BC.unpack . pprExpr) ast
    Left err -> (putStrLn . BC.unpack . pprParseError) err
