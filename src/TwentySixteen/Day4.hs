module TwentySixteen.Day4 where

import Data.Char
import Data.List
import Data.List.Extra
import qualified Data.Map.Strict as M
import Data.Ord
import Data.Tuple.Extra
import Text.ParserCombinators.Parsec
import Util

inputPath :: String
inputPath = "input/2016/4.txt"

rooms :: GenParser Char () [([String], Int, String)]
rooms = do
  rs <- many room
  eof
  return rs
  where
    room = do
      segments <- many1 alphaNum `sepBy` char '-'
      ordering <- between (char '[') (char ']') (many1 letter)
      eol
      return (init segments, read (last segments), ordering)

getOrdering :: [String] -> String
getOrdering =
  take 5
    . fmap fst
    . concatMap (sortOn fst)
    . groupOn snd
    . sortOn (Down . snd)
    . M.toList
    . countMap
    . concat

part1 :: IO Int
part1 = do
  rs <- readWithParser rooms <$> readFile inputPath
  return . sum $ snd3 <$> filter (\(ss, _, o) -> getOrdering ss == o) rs

part2 :: IO Int
part2 = do
  rs <- readWithParser rooms <$> readFile inputPath
  let rotate sId c = chr $ ((ord c - ord 'a' + sId) `mod` 26) + ord 'a'
      roomNames = (\(ss, sId, _) -> (sId, unwords $ rotate sId <$$> ss)) <$> rs
      (sId, _) = head $ filter (\(_, n) -> n == "northpole object storage") roomNames
  return sId
