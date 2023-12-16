module Day16 (part1, part2) where

energized :: Grid Char -> (Coord2, Dir2) -> Int
energized g start =
  let go seen Empty = setSize (setMap fst seen)
      go seen ((c, d) :<| q)
        | c ||∉ g || (c, d) ∈ seen = go seen q
        | otherwise =
            let ds =
                  case (d, g ||! c) of
                    (_, '.') -> [d]
                    (DirRight, '-') -> [d]
                    (DirLeft, '-') -> [d]
                    (_, '-') -> [DirLeft, DirRight]
                    (DirUp, '|') -> [d]
                    (DirDown, '|') -> [d]
                    (_, '|') -> [DirUp, DirDown]
                    (DirRight, '/') -> [DirUp]
                    (DirLeft, '/') -> [DirDown]
                    (DirUp, '/') -> [DirRight]
                    (DirDown, '/') -> [DirLeft]
                    (DirRight, '\\') -> [DirDown]
                    (DirLeft, '\\') -> [DirUp]
                    (DirUp, '\\') -> [DirLeft]
                    (DirDown, '\\') -> [DirRight]
                    e -> error $ show e
             in go ((c, d) |-> seen) (q >< (mkSeq [(move d 1 c, d) | d <- ds]))
   in go (∅) (mkSeq [start])

part1 :: Int
part1 = energized $(grid input 16) ((0, 0), DirRight)

-- part1 = energized $(grid exampleInput 16) ((0, 0), DirRight)

part2 :: Int
part2 =
  let g = $(grid input 16)
      -- let g = $(grid exampleInput 16)
      (maxX, maxY) = maxXY g
   in maximum $
        energized g
          <$> mconcat
            [ ((0, 0),) <$> [DirRight, DirDown],
              ((maxX, 0),) <$> [DirLeft, DirDown],
              ((0, maxY),) <$> [DirRight, DirUp],
              ((maxX, maxY),) <$> [DirLeft, DirUp],
              [((x, 0), DirDown) | x <- [1 .. maxX - 1]],
              [((x, maxY), DirUp) | x <- [1 .. maxX - 1]],
              [((0, y), DirDown) | y <- [1 .. maxY - 1]],
              [((maxX, y), DirUp) | y <- [1 .. maxY - 1]]
            ]
