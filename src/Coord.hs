module Coord where

data Dir2 = DirUp | DirDown | DirLeft | DirRight

type Coord2 = (Int, Int)

move :: Dir2 -> Coord2 -> Coord2
move DirUp (x, y) = (x, y - 1)
move DirDown (x, y) = (x, y + 1)
move DirLeft (x, y) = (x - 1, y)
move DirRight (x, y) = (x + 1, y)

turnCW :: Dir2 -> Dir2
turnCW DirUp = DirRight
turnCW DirRight = DirDown
turnCW DirDown = DirLeft
turnCW DirLeft = DirUp

turn180 :: Dir2 -> Dir2
turn180 = turnCW . turnCW

turnCCW :: Dir2 -> Dir2
turnCCW = turnCW . turnCW . turnCW
