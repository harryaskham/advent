module TwentyTwenty.Day3 where

import qualified Data.Map.Strict as M

inputPath :: String
inputPath = "input/2020/3.txt"

data Square = Tree | Empty

squareFromChar :: Char -> Square
squareFromChar '.' = Empty
squareFromChar '#' = Tree

-- Store the raw grid plus its width for repeating
data Grid = Grid (M.Map (Int, Int) Square) Int

gridFromLines :: [String] -> Grid
gridFromLines ls =
  Grid
    ( M.fromList
        [ ((x, y), squareFromChar c)
          | (y, row) <- zip [0 ..] ls,
            (x, c) <- zip [0 ..] row
        ]
    )
    (length $ head ls)

-- Lookup in the grid with horizontal wraparound
gridLookup :: (Int, Int) -> Grid -> Maybe Square
gridLookup (x, y) (Grid grid width) = M.lookup (x `mod` width, y) grid

countTreesFrom :: (Int, Int) -> (Int, Int) -> Grid -> Int
countTreesFrom pos (incX, incY) grid = go pos grid 0
  where
    go (x, y) grid count =
      case gridLookup (x, y) grid of
        Just Tree -> go (x + incX, y + incY) grid (count + 1)
        Just Empty -> go (x + incX, y + incY) grid count
        Nothing -> count

part1 :: IO Int
part1 = do
  ls <- lines <$> readFile inputPath
  return $ countTreesFrom (0, 0) (3, 1) $ gridFromLines ls

part2 :: IO Int
part2 = do
  ls <- lines <$> readFile inputPath
  return $
    product $
      (\inc -> countTreesFrom (0, 0) inc (gridFromLines ls))
        <$> [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
