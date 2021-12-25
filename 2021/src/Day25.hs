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

moveCuc :: (Int, Int) -> Cucumber -> (Coord2 -> Coord2) -> Grid Cell -> Grid Cell
moveCuc (w, h) cuc f g =
  let moves = concat [[(n, c), (p, CEmpty)] | (p, c) <- M.toList g, c == Full cuc, let n = wrap w h (f p), g M.! n == CEmpty]
   in foldl' (\g (p, c) -> M.insert p c g) g moves

part1 :: Int
part1 =
  let g = readGrid $(input 25) :: Grid Cell
      move = moveCuc $ both (+ 1) (maxXY g)
      gs = iterate (move CSouth (second (+ 1)) . move CEast (first (+ 1))) g
   in length (takeWhile (uncurry (/=)) (zip gs (drop 1 gs))) + 1

part2 :: Text
part2 = "Merry Christmas!"
