module KnightDef where

knightNext :: Int -> (Int, Int) -> [(Int, Int)]
knightNext n (x,y) = [ p
                     | p@(i, j) <- [(x+2, y+1), (x+1, y+2), (x-1, y+2), (x-2, y+1),
                                    (x-2, y-1), (x-1, y-2), (x+1, y-2), (x+2, y-1)]
                     , 1 <= i, i <= n
                     , 1 <= j, j <= n
                     ]

data Pos = MkPos Int (Int,Int) (Int,Int) [(Int,Int)]
    deriving (Eq, Show)
-- field order: board size, destination, current knight position, legal next positions

class Monad m => MonadKnight m where
    tellPosAskNext :: Pos -> m (Int,Int)
    buzz :: m ()

data KnightTrace a
  = Answer a
  | Step Pos ((Int,Int) -> KnightTrace a)
  | Buzz (KnightTrace a)
