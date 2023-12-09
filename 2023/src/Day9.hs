module Day9 (part1, part2) where

import Control.Arrow ((***))
import Data.List (foldr1)
import Helper.TH (input)
import Helper.Util (eol, number, parseWith)
import Relude.Unsafe qualified as U
import Text.ParserCombinators.Parsec (Parser, eof, many1, sepBy, string)

differences :: [Int] -> [[Int]]
differences = pure &&& enumFromTo 2 . length >>> uncurry (foldl' (\(ds : dss) _ -> (uncurry (-) <$> zip (drop 1 ds) ds) : ds : dss))

solve :: ([Int] -> Int) -> (Int -> Int -> Int) -> Int
solve get pm =
  $(input 9)
    & parseWith (many1 ((number `sepBy` string " ") <* eol) <* eof)
    & fmap (differences >>> (fmap get >>> ((foldr1 pm . U.tail) &&& U.head) >>> uncurry (flip pm)))
    & sum

part1 :: Int
part1 = solve U.last (+)

part2 :: Int
part2 = solve U.head (-)