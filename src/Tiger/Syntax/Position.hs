module Tiger.Syntax.Position where

import Prettyprinter

data Position = Position
  { -- | absolute character offset
    posOffset :: !Int,
    -- | line number
    posLine :: !Int,
    -- | column number
    posCol :: !Int
  }
  deriving (Show, Eq)

initPos :: Position
initPos = Position 0 1 1

movePos :: Position -> Char -> Position
movePos (Position a l _) '\n' = Position (a + 1) (l + 1) 1
movePos (Position a l c) _ = Position (a + 1) l (c + 1)

instance Pretty Position where
  pretty (Position _ l c) = "line:" <+> pretty l <> ", column:" <+> pretty c
