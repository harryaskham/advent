module Day3 (part1, part2) where

import Data.Char (isAsciiLower)
import Data.List (foldl1)
import Data.List.Split (chunk, chunksOf)
import Data.Set qualified as S
import Data.Text qualified as T
import Helper.TH (input)
import Helper.Util (toTuple2)
import Relude.Unsafe qualified as U

priority :: Char -> Int
priority a
  | isAsciiLower a = ord a - 96
  | otherwise = ord a - 38

duplicate1 :: Text -> Char
duplicate1 s =
  U.head
    . S.toList
    . uncurry S.intersection
    . toTuple2
    . fmap S.fromList
    . chunksOf (T.length s `div` 2)
    $ T.unpack s

part1 :: Int
part1 =
  $(input 3)
    & lines
    & fmap (priority . duplicate1)
    & sum

duplicate2 :: [Text] -> Char
duplicate2 =
  U.head
    . S.toList
    . foldl1 S.intersection
    . fmap (S.fromList . T.unpack)

part2 :: Int
part2 =
  $(input 3)
    & lines
    & chunksOf 3
    & fmap (priority . duplicate2)
    & sum
