module Helper.Coord where

import Data.Array
import Helper.Collection
import Text.Megaparsec (count')

data Dir2 = DirUp | DirDown | DirLeft | DirRight deriving (Show, Eq, Ord, Enum, Bounded)

instance Ix Dir2 where
  range (a, b) = [a .. b]
  index (a, _) c = fromEnum c - fromEnum a
  inRange (a, b) c = c >= a && c <= b

data Dir3 = D3xP | D3xN | D3yP | D3yN | D3zP | D3zN deriving (Show, Eq, Ord, Enum, Bounded)

udlrToDir2 :: Char -> Dir2
udlrToDir2 'u' = DirUp
udlrToDir2 'd' = DirDown
udlrToDir2 'l' = DirLeft
udlrToDir2 'r' = DirRight
udlrToDir2 'U' = DirUp
udlrToDir2 'D' = DirDown
udlrToDir2 'L' = DirLeft
udlrToDir2 'R' = DirRight
udlrToDir2 c = error $ "Invalid udlr: " <> show c

nsewToDir2 :: Char -> Dir2
nsewToDir2 'n' = DirUp
nsewToDir2 's' = DirDown
nsewToDir2 'e' = DirRight
nsewToDir2 'w' = DirLeft
nsewToDir2 'N' = DirUp
nsewToDir2 'S' = DirDown
nsewToDir2 'E' = DirRight
nsewToDir2 'W' = DirLeft
nsewToDir2 c = error $ "Invalid nsew: " <> show c

fromArrow2 :: Char -> Dir2
fromArrow2 '^' = DirUp
fromArrow2 'v' = DirDown
fromArrow2 '>' = DirRight
fromArrow2 '<' = DirLeft
fromArrow2 c = error $ "Invalid arrow: " <> show c

type Coord2 = (Int, Int)

type Coord3 = (Int, Int, Int)

type Coord4 = (Int, Int, Int, Int)

class Coord a where
  fromXY :: (Int, Int) -> a
  toXY :: a -> (Int, Int)
  mapXY :: ((Int, Int) -> (Int, Int)) -> a -> a

instance Coord Coord2 where
  fromXY = id
  toXY = id
  mapXY f = f

manhattan0 :: Coord2 -> Int
manhattan0 = (+) <$> (abs . fst) <*> (abs . snd)

manhattan :: Coord2 -> Coord2 -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

manhattan3 :: Coord3 -> Coord3 -> Int
manhattan3 (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

manhattan4 :: Coord4 -> Coord4 -> Int
manhattan4 (x1, y1, z1, w1) (x2, y2, z2, w2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2) + abs (w1 - w2)

move :: Dir2 -> Int -> Coord2 -> Coord2
move DirUp n (x, y) = (x, y - n)
move DirDown n (x, y) = (x, y + n)
move DirLeft n (x, y) = (x - n, y)
move DirRight n (x, y) = (x + n, y)

move3 :: Dir3 -> Int -> Coord3 -> Coord3
move3 D3xP n (x, y, z) = (x + n, y, z)
move3 D3xN n (x, y, z) = (x - n, y, z)
move3 D3yP n (x, y, z) = (x, y + n, z)
move3 D3yN n (x, y, z) = (x, y - n, z)
move3 D3zP n (x, y, z) = (x, y, z + n)
move3 D3zN n (x, y, z) = (x, y, z - n)

turnCW :: Dir2 -> Dir2
turnCW DirUp = DirRight
turnCW DirRight = DirDown
turnCW DirDown = DirLeft
turnCW DirLeft = DirUp

turn180 :: Dir2 -> Dir2
turn180 = turnCW . turnCW

opposite :: Dir2 -> Dir2
opposite = turn180

turnCCW :: Dir2 -> Dir2
turnCCW = turnCW . turnCW . turnCW

rlToTurn :: Char -> (Dir2 -> Dir2)
rlToTurn 'r' = turnCW
rlToTurn 'R' = turnCW
rlToTurn 'l' = turnCCW
rlToTurn 'L' = turnCCW
rlToTurn c = error $ "Invalid rl: " <> show c

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
    (x, y - 1)
  ]

neighbors3 :: Coord3 -> [Coord3]
neighbors3 (x, y, z) =
  [ (x + xO, y + yO, z + zO)
    | xO <- [-1 .. 1],
      yO <- [-1 .. 1],
      zO <- [-1 .. 1],
      xO /= 0 || yO /= 0 || zO /= 0
  ]

partitionSpace :: Coord3 -> Coord3 -> [(Coord3, Coord3)]
partitionSpace (lx, ly, lz) (ux, uy, uz) =
  [ ((lx, ly, lz), (hx, hy, hz)),
    ((hx, ly, lz), (ux, hy, hz)),
    ((lx, hy, lz), (hx, uy, hz)),
    ((hx, hy, lz), (ux, uy, hz)),
    ((lx, ly, hz), (hx, hy, uz)),
    ((hx, ly, hz), (ux, hy, uz)),
    ((lx, hy, hz), (hx, uy, uz)),
    ((hx, hy, hz), (ux, uy, uz))
  ]
  where
    hx = (lx + ux) `div` 2
    hy = (ly + uy) `div` 2
    hz = (lz + uz) `div` 2

-- Gets the point-set between two lines. Assumes lines are provided in sorted order.
linePoints :: (Coord2, Coord2) -> [Coord2]
linePoints ((x1, y1), (x2, y2))
  | x1 == x2 || y1 == y2 = [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]
  | otherwise = [(x, y) | x <- [x1 .. x2], let y = y1 + (x - x1) * signum (y2 - y1)]

wrap :: Int -> Int -> Coord2 -> Coord2
wrap w h (x, y) = (x `mod` w, y `mod` h)
