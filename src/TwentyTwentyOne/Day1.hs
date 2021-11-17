module TwentyTwentyOne.Day1 where

import qualified Data.Map.Strict as M
import qualified Data.Sequence as SQ
import qualified Data.Set as S

inputPath :: String
inputPath = "input/2021/1.txt"

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> readFile inputPath
  print xs
