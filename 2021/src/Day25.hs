module Day25 where

import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.List (foldl1')
import Data.Map.Strict qualified as M
import Helper.Coord (Coord2, wrap)
import Helper.Grid (Grid, GridCell (charMap), maxXY, readGrid)
import Helper.TH (input)
import Helper.Util (both)

data Cucumber = CEast | CSouth deriving (Eq, Ord)

data Cell = CEmpty | Full Cucumber deriving (Eq, Ord)

instance GridCell Cell where
  charMap = BM.fromList [(CEmpty, '.'), (Full CEast, '>'), (Full CSouth, 'v')]

moveCuc :: Int -> Int -> Cucumber -> (Coord2 -> Coord2) -> Grid Cell -> Grid Cell
moveCuc w h cuc f g = foldl' (\g (p, c) -> M.insert p c g) g (concat [[(n, c), (p, CEmpty)] | (p@(x, y), c) <- M.toList g, c == Full cuc, let n = wrap w h (f p), g M.! n == CEmpty])

step :: Int -> Int -> Grid Cell -> Grid Cell
step w h = moveCuc w h CSouth (second (+ 1)) . moveCuc w h CEast (first (+ 1))

part1 :: Int
part1 =
  let g = readGrid $(input 25) :: Grid Cell
      (w, h) = both (+ 1) (maxXY g)
      gs = iterate (step w h) g
   in length (takeWhile (uncurry (/=)) (zip gs (drop 1 gs))) + 1

part2 :: Text
part2 = "Merry Christmas!"
