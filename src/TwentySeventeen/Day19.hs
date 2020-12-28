module TwentySeventeen.Day19 where

import Data.List
import qualified Data.Map.Strict as M

inputPath :: String
inputPath = "input/2017/19.txt"

type Grid = M.Map (Int, Int) Char

data Dir = DirUp | DirDown | DirLeft | DirRight deriving (Eq)

readInput :: IO (M.Map (Int, Int) Char)
readInput = do
  rows <- lines <$> readFile inputPath
  return $
    M.fromList
      [ ((x, y), c)
        | (y, row) <- zip [0 ..] rows,
          (x, c) <- zip [0 ..] row,
          c /= ' '
      ]

move :: Dir -> (Int, Int) -> (Int, Int)
move DirUp (x, y) = (x, y - 1)
move DirDown (x, y) = (x, y + 1)
move DirLeft (x, y) = (x - 1, y)
move DirRight (x, y) = (x + 1, y)

opposite :: Dir -> Dir
opposite DirUp = DirDown
opposite DirDown = DirUp
opposite DirLeft = DirRight
opposite DirRight = DirLeft

walk :: Grid -> (Int, Int) -> Dir -> String -> Int -> (String, Int)
walk grid pos dir letters steps =
  case M.lookup pos grid of
    Nothing -> (reverse letters, steps)
    Just '-' -> continue
    Just '|' -> continue
    Just '+' -> turn
    Just l -> takeLetter l
  where
    continue = walk grid (move dir pos) dir letters (steps + 1)
    takeLetter l = walk grid (move dir pos) dir (l : letters) (steps + 1)
    nextDirs = delete (opposite dir) [DirUp, DirDown, DirLeft, DirRight]
    (turnDir, turnPos) =
      head
        [ (dir, move dir pos)
          | dir <- nextDirs,
            move dir pos `M.member` grid
        ]
    turn = walk grid turnPos turnDir letters (steps + 1)

part12 :: IO (String, Int)
part12 = do
  grid <- readInput
  let start = head [(x, y) | ((x, y), _) <- M.toList grid, y == 0]
  return $ walk grid start DirDown "" 0
