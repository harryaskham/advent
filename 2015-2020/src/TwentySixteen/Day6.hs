module TwentySixteen.Day6 where

import Data.List (sortOn, transpose)
import qualified Data.Map.Strict as M
import Util (countMap)

inputPath :: String
inputPath = "input/2016/6.txt"

part12 :: IO [String]
part12 =
  traverse
    ( \firstLast ->
        fmap
          ( fst
              . firstLast
              . sortOn snd
              . M.toList
              . countMap
          )
          . transpose
          . lines
          <$> readFile inputPath
    )
    [last, head]
