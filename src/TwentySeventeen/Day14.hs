module TwentySeventeen.Day14 where

import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.Sequence as SQ
import qualified Data.Set as S
import Text.Printf (printf)
import TwentySeventeen.Day10 (knotHash)

input :: String
input = "ugkiagan"

getGrid :: [String]
getGrid = rows
  where
    knotRows = [knotHash $ input ++ "-" ++ show r | r <- [0 .. 127]]
    expandRow row = (printf "%04b" . digitToInt) =<< row
    rows = expandRow <$> knotRows

part1 :: Int
part1 = length $ filter (== '1') (concat getGrid)

mkGridMap :: [String] -> M.Map (Int, Int) Char
mkGridMap rows =
  M.fromList
    [ ((x, y), c)
      | (y, row) <- zip [0 ..] rows,
        (x, c) <- zip [0 ..] row
    ]

countGroups :: M.Map (Int, Int) Char -> S.Set (Int, Int) -> (Int, Int) -> Int -> Int
countGroups _ _ (127, 127) n = n
countGroups grid seen (x, y) n = countGroups grid nextSeen nextCoord nextN
  where
    current = grid M.! (x, y)
    nextCoord = if x == 127 then (0, y + 1) else (x + 1, y)
    (nextSeen, nextN) = case current of
      '0' -> (seen, n)
      '1' ->
        if (x, y) `S.member` seen
          then (seen, n)
          else (seen `S.union` bfsFrom grid (SQ.singleton (x, y)) S.empty, n + 1)

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) =
  [ (x + 1, y),
    (x - 1, y),
    (x, y + 1),
    (x, y - 1)
  ]

bfsFrom :: M.Map (Int, Int) Char -> SQ.Seq (Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int)
bfsFrom grid queue seen =
  case SQ.viewl queue of
    SQ.EmptyL -> seen
    current SQ.:< rest ->
      let ns =
            SQ.fromList
              [ n
                | n <- neighbours current,
                  not $ n `S.member` seen,
                  M.lookup n grid == Just '1'
              ]
          nextSeen = S.insert current seen
       in bfsFrom grid (rest SQ.>< ns) nextSeen

part2 :: Int
part2 = countGroups (mkGridMap getGrid) S.empty (0, 0) 0
