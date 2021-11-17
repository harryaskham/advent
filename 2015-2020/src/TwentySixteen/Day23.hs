module TwentySixteen.Day23 where

import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import TwentySixteen.Day12 (Machine (Machine), instructions, run)
import Util (input, readWithParser)

part1 :: IO Int
part1 = do
  is <- readWithParser instructions <$> input 2016 23
  let (_, Machine _ mem) =
        run
          (V.fromList is)
          (Machine 0 (M.fromList (zip ['a' .. 'd'] [7, 0, 0, 0])))
  return $ mem M.! 'a'

part2 :: Int
part2 = product [2 .. 12] + 79 * 89
