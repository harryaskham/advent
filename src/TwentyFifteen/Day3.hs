module TwentyFifteen.Day3 where

import Coord (Coord2, Dir2 (..), move)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Tuple.Extra (first, second)
import Util (adjustWithDefault, input)

toDir2 :: Char -> Dir2
toDir2 '^' = DirUp
toDir2 'v' = DirDown
toDir2 '<' = DirLeft
toDir2 '>' = DirRight

locationCounts :: Coord2 -> [Dir2] -> Map Coord2 Int
locationCounts start dirs = go start dirs M.empty
  where
    go pos [] counts = adjustWithDefault 0 (+ 1) pos counts
    go pos (d : dirs) counts =
      go
        (move d 1 pos)
        dirs
        (adjustWithDefault 0 (+ 1) pos counts)

solve :: (Coord2 -> [Dir2] -> Map Coord2 Int) -> IO Int
solve f = do
  dirs <- fmap toDir2 . head . lines <$> input 2015 3
  return . M.size . M.filter (>= 1) . f (0, 0) $ dirs

part1 :: IO Int
part1 = solve locationCounts

pairLocationCounts :: Coord2 -> [Dir2] -> Map Coord2 Int
pairLocationCounts start dirs =
  go (start, start) dirs (cycle [(fst, first), (snd, second)]) M.empty
  where
    go ps [] ((access, _) : _) counts =
      adjustWithDefault 0 (+ 1) (access ps) counts
    go ps (d : dirs) ((access, update) : turns) counts =
      go
        (update (move d 1) ps)
        dirs
        turns
        (adjustWithDefault 0 (+ 1) (access ps) counts)

part2 :: IO Int
part2 = solve pairLocationCounts
