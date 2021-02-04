module TwentyFifteen.Day2 where

import Data.List.Extra (splitOn)
import Util (input, toTuple3)

paper :: (Int, Int, Int) -> Int
paper (l, w, h) =
  2 * l * w + 2 * w * h + 2 * h * l + minimum [l * w, w * h, h * l]

solve :: ((Int, Int, Int) -> Int) -> IO Int
solve f =
  sum
    . fmap (f . toTuple3 . fmap read . splitOn "x")
    . lines
    <$> input 2015 2

part1 :: IO Int
part1 = solve paper

ribbon :: (Int, Int, Int) -> Int
ribbon (l, w, h) =
  minimum [2 * (l + w), 2 * (w + h), 2 * (h + l)] + l * w * h

part2 :: IO Int
part2 = solve ribbon
