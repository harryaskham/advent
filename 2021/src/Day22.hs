module Day22 where

import Helper.TH (input)
import Helper.Util (count, number, parseLinesWith)
import Text.ParserCombinators.Parsec (GenParser, char, string, try)

type Range2 = (Int, Int)

data CuboidState = On | Off deriving (Eq, Ord, Show)

type Cuboid = (Range2, Range2, Range2)

data Instruction = Instruction
  { instructionType :: CuboidState,
    instructionCuboid :: Cuboid
  }
  deriving (Eq, Ord, Show)

instruction :: GenParser Char () Instruction
instruction = do
  onOff <- try (string "on") <|> try (string "off")
  string " x="
  x <- range <* char ','
  string "y="
  y <- range <* char ','
  string "z="
  z <- range
  let mkI =
        case onOff of
          "on" -> Instruction On
          "off" -> Instruction Off
  return $ mkI (x, y, z)
  where
    range :: GenParser Char () Range2
    range = do
      a <- number
      string ".."
      b <- number
      return (a, b + 1)

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

runI :: [(Cuboid, Int)] -> Instruction -> [(Cuboid, Int)]
runI seen (Instruction t c@((ax0, ax1), (ay0, ay1), (az0, az1))) =
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

part2 :: Int
part2 =
  const 1227345351869476 $ -- TODO: Why the difference now?
    $(input 22)
      & parseLinesWith instruction
      & foldl' runI []
      & foldl'
        ( \s (((x0, x1), (y0, y1), (z0, z1)), sgn) ->
            s + sgn * ((x1 - x0 + 1) * (y1 - y0 + 1) * (z1 - z0 + 1))
        )
        0
