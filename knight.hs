import Control.Monad

type KnightPos = (Int, Int)

-- all positions available
moveKnight :: KnightPos -> [KnightPos]
moveKnight (x, y) = do
  (x', y') <- [(x + 2, y + 1), (x + 2, y - 1), (x - 2, y - 1), (x - 2, y + 1),
               (x + 1, y + 2), (x + 1, y - 2), (x - 1, y - 2), (x - 1, y + 2)]
  
  guard (x' `elem` [1..8] && y' `elem` [1..8])
  return (x', y')

-- all positions available within three moves
knightPos :: KnightPos -> [KnightPos]
knightPos pos = do
  a <- moveKnight pos
  b <- moveKnight a
  moveKnight b

canReachIn3Moves :: KnightPos -> KnightPos -> Bool
canReachIn3Moves p q = elem q (knightPos p)