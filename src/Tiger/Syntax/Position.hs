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
  deriving (Show, Eq, Ord)

initPos :: Position
initPos = Position 0 1 1

movePos :: Position -> Char -> Position
movePos (Position a l _) '\n' = Position (a + 1) (l + 1) 1
movePos (Position a l c) _ = Position (a + 1) l (c + 1)

updateCol :: Int -> Position -> Position
updateCol x (Position a l c) = Position a l (c + x)

updateLine :: Int -> Position -> Position
updateLine x (Position a l c) = Position a (l + x) c

instance Pretty Position where
  pretty (Position _ l c) = "line:" <+> pretty l <> ", column:" <+> pretty c

data Span = Span
  { spanStart :: !Position,
    spanEnd :: !Position
  }
  deriving (Show, Eq)

instance Semigroup Span where
  (Span s1 e1) <> (Span s2 e2) = Span (min s1 s2) (max e1 e2)

data Loc a = Loc
  { locSpan :: !Span,
    locInfo :: a
  }
  deriving (Show)

class HasSpan a where
  getSpan :: a -> Span

instance HasSpan (Loc a) where
  getSpan = locSpan
