module Day8 (part1, part2) where

import Data.Bimap qualified as BM
import Data.Char (intToDigit)
import Data.List (foldl1, maximum, minimum, nub)
import Data.Map.Strict qualified as M
import Data.Tuple.Extra (fst3)
import Helper.Coord (Coord2)
import Helper.Grid (Grid, GridCell (charMap), readGrid, rowsCols)
import Helper.TH (input)
import Helper.Util ((<$$>))
import Relude.Unsafe qualified as U

newtype Cell = Cell Int deriving (Eq, Ord, Enum)

instance GridCell Cell where
  charMap = BM.fromList [(Cell x, intToDigit x) | x <- [0 .. 9]]

visibilityFrom :: Grid Cell -> [Coord2] -> [Coord2]
visibilityFrom g (first : rest) =
  fst $
    foldl'
      ( \(visible, tallest) c ->
          if g M.! c > g M.! tallest
            then (c : visible, c)
            else (visible, tallest)
      )
      ([], first)
      rest

visible :: Grid Cell -> [Coord2]
visible g =
  nub $
    perimeter
      ++ ( visibilityFrom g
             =<< concat [rows, cols, reverse <$> rows, reverse <$> cols]
         )
  where
    (rows, cols) = rowsCols g
    perimeter = U.head rows ++ U.head cols ++ U.last rows ++ U.last cols

scoresFrom :: Grid Cell -> [Coord2] -> Map Coord2 Int
scoresFrom g cs =
  fst3 $
    foldl'
      ( \(distances, distanceToLastTree, n) c ->
          let distance =
                case catMaybes [distanceToLastTree M.! t | t <- [g M.! c .. Cell 9]] of
                  [] -> n
                  ds -> 1 + minimum ds
           in ( M.insert c distance distances,
                M.insert (g M.! c) (Just 0) $ (+ 1) <$$> distanceToLastTree,
                n + 1
              )
      )
      (M.empty, M.fromList [(t, Nothing) | t <- [Cell 0 .. Cell 9]], 0)
      cs

scores :: Grid Cell -> Map Coord2 Int
scores g =
  foldl1
    (M.unionWith (*))
    (concat (scoresFrom g <$$> [rows, cols, reverse <$> rows, reverse <$> cols]))
  where
    (rows, cols) = rowsCols g

part1 :: Int
part1 = $(input 8) & readGrid & visible & length

part2 :: Int
part2 = $(input 8) & readGrid & scores & maximum
