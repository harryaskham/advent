module Day9 (part1, part2) where

import Data.List (elem, foldl', last, length, nub, replicate)
import Helper.Coord (Coord2, Dir2, move, neighbors, udlrToDir2)
import Helper.TH (input)
import Helper.Util (clip, eol, number, parseWith)
import Text.ParserCombinators.Parsec (Parser, anyChar, eof, many1, spaces)
import Prelude hiding (elem, last)

parser :: Parser [(Dir2, Int)]
parser = many1 ((,) <$> (udlrToDir2 <$> anyChar <* spaces) <*> number <* eol) <* eof

moveRope :: ([Coord2], [Coord2]) -> (Dir2, Int) -> ([Coord2], [Coord2])
moveRope (rope, path) (_, 0) = (rope, path)
moveRope ([], path) _ = ([], path)
moveRope (h : ts, path) (d, n) = moveRope (rope', last rope' : path) (d, n - 1)
  where
    rope' = moveTail d (move d 1 h : ts)
    moveTail d (a@(x, y) : b@(x', y') : rest)
      | b `elem` a : neighbors a = a : b : rest
      | otherwise = a : moveTail d ((x' + clip (-1) 1 (x - x'), y' + clip (-1) 1 (y - y')) : rest)
    moveTail _ a = a

solve :: Int -> Int
solve n =
  $(input 9)
    & parseWith parser
    & foldl' moveRope (replicate n (0, 0), [])
    & snd
    & nub
    & length

part1 :: Int
part1 = solve 2

part2 :: Int
part2 = solve 10
