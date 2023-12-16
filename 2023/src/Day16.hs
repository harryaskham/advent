module Day16 (part1, part2) where

energized :: Grid Char -> (Coord2, Dir2) -> Int
energized g start =
  let go seen Empty = size (setMap fst seen)
      go seen ((c, d) :<| q)
        | c ∉ g || (c, d) ∈ seen = go seen q
        | otherwise =
            let ds =
                  case g ||! c of
                    '.' -> [d]
                    '-' -> bool [d] [DirLeft, DirRight] (d ∈ [DirUp, DirDown])
                    '|' -> bool [d] [DirUp, DirDown] (d ∈ [DirLeft, DirRight])
                    '/' -> [d & bool turnCW turnCCW (d ∈ [DirLeft, DirRight])]
                    '\\' -> [d & bool turnCW turnCCW (d ∈ [DirUp, DirDown])]
             in go ((c, d) |-> seen) (q >< (mkSeq [(move d 1 c, d) | d <- ds]))
   in go (∅) (mkSeq [start])

part1 :: Int
part1 = energized $(grid input 16) ((0, 0), DirRight)

part2 :: Int
part2 =
  let g = $(grid input 16)
   in maximum [energized g (c, d) | d <- enumerate, c <- (perimeter g) `pFrom` d]
