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

moveOne :: (Int, Int) -> Cucumber -> (Coord2 -> Coord2) -> Grid Cell -> Grid Cell
moveOne (w, h) cuc f g =
  foldl'
    (\g (p, n, c) -> M.insert n c . M.insert p CEmpty $ g)
    g
    [(p, n, c) | (p, c) <- M.toList g, c == Full cuc, let n = wrap w h (f p), g M.! n == CEmpty]

part1 :: Int
part1 =
  let g = readGrid $(input 25) :: Grid Cell
      move cuc s = moveOne (both (+ 1) (maxXY g)) cuc (s (+ 1))
      gs = iterate (move CSouth second . move CEast first) g
   in length (takeWhile (uncurry (/=)) (zip gs (drop 1 gs))) + 1

part2 :: Text
part2 = "Merry Christmas!"
