module Day22 where

import Control.Lens
import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.CSG qualified as CSG
import Data.List.Extra hiding (intersect, sum)
import Data.Map.Strict qualified as M
import Data.Mod
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Read
import Data.Tuple.Extra (fst3, snd3)
import Data.Vector qualified as V
import Extra (thd3)
import Helper.Coord
import Helper.Grid
import Helper.TH
import Helper.Tracers
import Helper.Util
import Linear
import System.Random
import Text.ParserCombinators.Parsec hiding (count, (<|>))

type Range2 = (Int, Int)

data CuboidState = On | Off deriving (Eq, Ord, Show)

type Cuboid = (Range2, Range2, Range2)

volume :: Cuboid -> Int
volume ((x0, x1), (y0, y1), (z0, z1)) = (abs (x1 - x0)) * (abs (y1 - y0)) * (abs (z1 - z0))

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

part2 =
  $(exampleInput 22)
    & parseLinesWith instruction
