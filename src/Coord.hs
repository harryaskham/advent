module Coord where

data Dir2 = DirUp | DirDown | DirLeft | DirRight deriving (Show, Eq, Ord)

udlrToDir2 :: Char -> Dir2
udlrToDir2 'u' = DirUp
udlrToDir2 'd' = DirDown
udlrToDir2 'l' = DirLeft
udlrToDir2 'r' = DirRight
udlrToDir2 'U' = DirUp
udlrToDir2 'D' = DirDown
udlrToDir2 'L' = DirLeft
udlrToDir2 'R' = DirRight

nsewToDir2 :: Char -> Dir2
nsewToDir2 'n' = DirUp
nsewToDir2 's' = DirDown
nsewToDir2 'e' = DirRight
nsewToDir2 'w' = DirLeft
nsewToDir2 'N' = DirUp
nsewToDir2 'S' = DirDown
nsewToDir2 'E' = DirRight
nsewToDir2 'W' = DirLeft

type Coord2 = (Int, Int)

manhattan0 :: Coord2 -> Int
manhattan0 = (+) <$> (abs . fst) <*> (abs . snd)

manhattan :: Coord2 -> Coord2 -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

move :: Dir2 -> Int -> Coord2 -> Coord2
move DirUp n (x, y) = (x, y - n)
move DirDown n (x, y) = (x, y + n)
move DirLeft n (x, y) = (x - n, y)
move DirRight n (x, y) = (x + n, y)

turnCW :: Dir2 -> Dir2
turnCW DirUp = DirRight
turnCW DirRight = DirDown
turnCW DirDown = DirLeft
turnCW DirLeft = DirUp

turn180 :: Dir2 -> Dir2
turn180 = turnCW . turnCW

turnCCW :: Dir2 -> Dir2
turnCCW = turnCW . turnCW . turnCW

rlToTurn :: Char -> (Dir2 -> Dir2)
rlToTurn 'r' = turnCW
rlToTurn 'R' = turnCW
rlToTurn 'l' = turnCCW
rlToTurn 'L' = turnCCW

neighbors :: Coord2 -> [Coord2]
neighbors (x, y) =
  [ (x + xO, y + yO)
    | xO <- [-1 .. 1],
      yO <- [-1 .. 1],
      xO /= 0 || yO /= 0
  ]

neighborsNoDiags :: Coord2 -> [Coord2]
neighborsNoDiags (x, y) =
  [ (x + 1, y),
    (x - 1, y),
    (x, y + 1),
    (x, y -1)
  ]
