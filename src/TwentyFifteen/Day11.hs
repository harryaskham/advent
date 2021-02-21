module TwentyFifteen.Day11 where

import Data.Char (chr, ord)
import Data.List (group)
import Numeric (readInt, showIntAtBase)

password :: String
password = "vzbxkghb"

toBase26 :: String -> Integer
toBase26 = fst . head . readInt 26 (`elem` "abcdefghijklmnopqrstuvwxyz") (\c -> ord c - 97)

fromBase26 :: Integer -> String
fromBase26 = ($ "") . showIntAtBase 26 (\c -> chr $ c + 97)

increment :: String -> String
increment = fromBase26 . (+ 1) . toBase26

valid :: String -> Bool
valid = all (== True) . ([hasThreeStraight, noIOL, hasTwoPairs] <*>) . pure

hasThreeStraight :: String -> Bool
hasThreeStraight [_, _] = False
hasThreeStraight (a : b : c : xs)
  | ord b == ord a + 1 && ord c == ord b + 1 = True
  | otherwise = hasThreeStraight (b : c : xs)

noIOL :: String -> Bool
noIOL = not . any (== True) . (elem <$> "iol" <*>) . pure

hasTwoPairs :: String -> Bool
hasTwoPairs = (>= 2) . length . filter ((>= 2) . length) . group

part1 :: String
part1 = head . filter valid $ iterate increment password

part2 :: String
part2 = head . drop 1 . filter valid $ iterate increment password
