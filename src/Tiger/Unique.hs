module Tiger.Unique where

import qualified Data.Unique as U
import GHC.IO (unsafePerformIO)

newtype Unique = Unique U.Unique
  deriving (Eq)

instance Show Unique where
  show (Unique x) = show $ U.hashUnique x

allocateUnique :: Unique
allocateUnique = Unique $ unsafePerformIO U.newUnique
{-# NOINLINE allocateUnique #-}
