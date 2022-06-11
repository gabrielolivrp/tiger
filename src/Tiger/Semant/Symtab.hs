module Tiger.Semant.Symtab where

import qualified Data.Map as M
import qualified Tiger.Syntax.Parser.Ast as A

newtype Symtab a = Symtab (M.Map A.Symbol a)

empty :: Symtab a
empty = Symtab M.empty

enter :: Symtab a -> A.Symbol -> a -> Symtab a
enter (Symtab env) k v = Symtab $ M.insert k v env

look :: Symtab a -> A.Symbol -> Maybe a
look (Symtab env) k = M.lookup k env

fromList :: [(A.Symbol, a)] -> Symtab a
fromList = Symtab . M.fromList
