module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Tiger.Error.RenderError
import Tiger.Syntax (pprExpr, runParser)

main :: IO ()
main = do
  let filePath = "examples/add.tig"
  contents <- BS.readFile filePath
  case runParser (Just filePath) contents of
    Right ast -> (BC.putStrLn . pprExpr) ast
    Left err -> (BC.putStrLn . renderError) err
