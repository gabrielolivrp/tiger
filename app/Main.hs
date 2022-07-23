module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Tiger.Semant (transProg)
import Tiger.Syntax (pprExpr, runParser)

main :: IO ()
main = do
  let filePath = "examples/add.tig"
  contents <- BS.readFile filePath
  case runParser (Just filePath) contents of
    Right ast ->
      case transProg ast of
        Left err -> print err
        Right _ -> putStrLn "ok"
    Left err -> print err
