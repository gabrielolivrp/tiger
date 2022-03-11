module Main where

import qualified Data.ByteString.Char8 as BC
import Tiger.Syntax.Parser

example =
  unwords
    [ "let\n",
      "   function add(a: integer, b:integer): integer =\n",
      "     a + b\n",
      "in\n",
      "   print(add(1, 2))\n",
      "end\n"
    ]

main :: IO ()
main = do
  let input = BC.pack example
  let ast = runParser Nothing input
  print ast
