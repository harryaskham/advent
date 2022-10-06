module Day22 where

import Helper.TH (input)
import Helper.Util (count, number, parseLinesWith)
import Text.ParserCombinators.Parsec (GenParser, char, string, try)

type Range2 = (Int, Int)

data CuboidState = On | Off deriving (Eq)

type Cuboid = (Range2, Range2, Range2)

data Instruction = Instruction CuboidState Cuboid

instruction :: GenParser Char () Instruction
instruction = do
  mkI <- try (string "on" >> return (Instruction On)) <|> try (string "off" >> return (Instruction Off))
  x <- string " x=" *> range <* char ','
  y <- string "y=" *> range <* char ','
  z <- string "z=" *> range
  return $ mkI (x, y, z)
  where
    range = (,) <$> (number <* string "..") <*> ((+ 1) <$> number)

runInstruction :: Instruction -> (Int, Int, Int) -> CuboidState -> CuboidState
runInstruction (Instruction t ((x0, x1), (y0, y1), (z0, z1))) (x, y, z) c
  | x >= x0 && x < x1 && y >= y0 && y < y1 && z >= z0 && z < z1 = t
  | otherwise = c

part1 :: Int
part1 =
  $(input 22)
    & parseLinesWith instruction
    & foldl'
      (\g i -> (\(p, c) -> (p, runInstruction i p c)) <$> g)
      [((x, y, z), Off) | x <- [-50 .. 50], y <- [-50 .. 50], z <- [-50 .. 50]]
    & count ((== On) . snd)

accumulate :: [(Cuboid, Int)] -> Instruction -> [(Cuboid, Int)]
accumulate seen (Instruction t c@((ax0, ax1), (ay0, ay1), (az0, az1))) =
  seen
    ++ ( case t of
           On -> [(c, 1)]
           Off -> []
       )
    ++ [ (((x0, x1), (y0, y1), (z0, z1)), negate sgn)
         | (((bx0, bx1), (by0, by1), (bz0, bz1)), sgn) <- seen,
           let x0 = max ax0 bx0,
           let x1 = min ax1 bx1,
           let y0 = max ay0 by0,
           let y1 = min ay1 by1,
           let z0 = max az0 bz0,
           let z1 = min az1 bz1,
           x0 <= x1,
           y0 <= y1,
           z0 <= z1
       ]

volume :: Cuboid -> Int
volume ((x0, x1), (y0, y1), (z0, z1)) =
  abs (x1 - x0) * abs (y1 - y0) * abs (z1 - z0)

part2 :: Int
part2 =
  $(input 22)
    & parseLinesWith instruction
    & foldl' accumulate []
    & foldl' (\s (c, sgn) -> s + sgn * volume c) 0
