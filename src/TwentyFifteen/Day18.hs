module TwentyFifteen.Day18 where

import Coord (neighbors)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Grid (Grid, pretty, toGrid)
import Util (input, traceStrLn)

data Cell = On | Off deriving (Eq)

instance Show Cell where
  show On = "#"
  show Off = "."

fromChar :: Char -> Cell
fromChar '.' = Off
fromChar '#' = On

step :: Grid Cell -> Grid Cell
step grid =
  M.fromList
    [ ((x, y), c)
      | x <- [0 .. 99],
        y <- [0 .. 99],
        let ns = filter (\(x, y) -> M.lookup (x, y) grid == Just On) (neighbors (x, y))
            c =
              case grid M.! (x, y) of
                On -> if length ns == 2 || length ns == 3 then On else Off
                Off -> if length ns == 3 then On else Off
    ]

solve :: (Grid Cell -> Grid Cell) -> IO Int
solve f = do
  grid <- toGrid fromChar . lines <$> input 2015 18
  return $ M.size (M.filter (== On) (iterate f grid !! 100))

part1 :: IO Int
part1 = solve step

step2 :: Grid Cell -> Grid Cell
step2 grid' =
  traceStrLn (pretty grid) $
    traceStrLn "" $
      M.fromList
        [ ((x, y), c)
          | x <- [0 .. width -1],
            y <- [0 .. width -1],
            let ns = filter (\(x, y) -> M.lookup (x, y) grid == Just On) (neighbors (x, y))
                c =
                  if (x, y) `elem` [(0, 0), (0, width -1), (width -1, 0), (width -1, width -1)]
                    then On
                    else case grid M.! (x, y) of
                      On -> if length ns == 2 || length ns == 3 then On else Off
                      Off -> if length ns == 3 then On else Off
        ]
  where
    grid =
      foldl'
        (\g pos -> M.insert pos On g)
        grid'
        [(0, 0), (0, width -1), (width -1, 0), (width -1, width -1)]
    width = 100

part2 :: IO Int
part2 = solve step2
