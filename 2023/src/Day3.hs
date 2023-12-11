module Day3 (part1, part2) where

data Cell = None | Digit Char | Mark Char deriving (Eq, Ord, Show)

instance GridCell Cell where
  charMap =
    mkBimap $
      [(None, '.')]
        <> [(Digit i, i) | i <- ['0' .. '9']]
        <> [(Mark c, c) | c <- "!@#$%^&*()-=/+"]

getNumbers :: Grid Cell -> [(Int, [Coord2])]
getNumbers g = go 0 0 "" []
  where
    (maxX, maxY) = maxXY g
    go x y currentNum currentCoords
      | y > maxY = []
      | x > maxX =
          case (currentNum, currentCoords) of
            (s@(_ : _), cs@(_ : _)) -> (uread s, nub cs) : go 0 (y + 1) "" []
            _ -> go 0 (y + 1) "" []
      | otherwise =
          case g |! (x, y) of
            Digit c -> go (x + 1) y (currentNum <> [c]) (currentCoords <> neighbors (x, y))
            _ ->
              case (currentNum, currentCoords) of
                (s@(_ : _), cs@(_ : _)) -> (uread s, nub cs) : go (x + 1) y "" []
                _ -> go (x + 1) y "" []

isMark :: Cell -> Bool
isMark (Mark _) = True
isMark _ = False

part1 :: Int
part1 =
  $(input 3)
    & readGrid
    & ( \g ->
          getNumbers g
            & filter (\(_, cs) -> any isMark $ catMaybes [g |? c | c <- cs])
      )
    & fmap fst
    & sum

part2 :: Int
part2 =
  $(input 3)
    & readGrid
    & ( \g ->
          getNumbers g
            & fmap (\(n, cs) -> mkMap [(c, [n]) | c <- cs, let a = g |? c, a == Just (Mark '*')])
      )
    & foldl1' (unionWith (<>))
    & mapFilter ((== 2) . length)
    & fmap product
    & unMap
    & fmap snd
    & sum