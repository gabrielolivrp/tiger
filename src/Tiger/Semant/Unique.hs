module Tiger.Semant.Unique where

import Control.Monad.State

newtype Unique = Unique Int
  deriving (Show, Eq)

nextUnique :: Unique -> Unique
nextUnique (Unique x) = Unique $ x + 1
