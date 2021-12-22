module Day22 where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.CSG qualified as CSG
import Data.List.Extra hiding (sum)
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

{-
For each existing, non-overlapping cuboid:
does it overlap with the new cuboid
  if not, continue
  if so
    The original splits into possibly many cuboids
    The new cuboid also splits
    Any fragment only in the original goes in the left set
    Any fragment belonging to both goes in both sets
    Any fragment only in the right goes in the right set
    Now the left set is the valid existing non-overlapping cuboids
    We now need to do the same with the right set and any other overlaps
    This will give us a left set of split existing, plus possibly shared fragments
    And a right set full of new fragments, plus possibly shared fragments
    Then union or difference will give a valid next state
-}

corners :: Cuboid -> [(Int, Int, Int)]
corners ((x0, x1), (y0, y1), (z0, z1)) =
  [ (x0, y0, z0),
    (x0, y0, z1),
    (x0, y1, z0),
    (x0, y1, z1),
    (x1, y0, z0),
    (x1, y0, z1),
    (x1, y1, z0),
    (x1, y1, z1)
  ]

inside :: Cuboid -> (Int, Int, Int) -> Bool
inside ((x0, x1), (y0, y1), (z0, z1)) (x, y, z) =
  x >= x0 && x < x1 && y >= y0 && y < y1 && z >= z0 && z < z1

-- Bounds are going to be fucked here because we need symmettry
-- We are going to need to spit out the right-split rather than rely on symmetry
-- Problem: in the eight-case we are counting the centre many times
partitionCuboid :: [(Int, Int, Int)] -> Cuboid -> Set Cuboid
partitionCuboid pivots c@((x0, x1), (y0, y1), (z0, z1))
  | not (length pivots `Prelude.elem` [0, 1, 2, 4, 8]) =  error "Invalid number of pivot points"
  | otherwise =
  traceShow pivots $
  case sortPivots pivots of
    -- No pivots == no overlap; just return the original
    [] ->
      --traceShow "no overlap" $
      S.singleton c
    -- A single pivot == divide cuboid into 8 segments
    -- Some segments might be 0d, 1d, 2d or 3d
    [(x, y, z)] ->
      --traceShow "splitting into 8" $
      S.fromList
        [ ((x0, x), (y0, y), (z0, z)),
          ((x0, x), (y0, y), (z, z1)),
          ((x0, x), (y, y1), (z0, z)),
          ((x0, x), (y, y1), (z, z1)),
          ((x, x1), (y0, y), (z0, z)),
          ((x, x1), (y0, y), (z, z1)),
          ((x, x1), (y, y1), (z0, z)),
          ((x, x1), (y, y1), (z, z1))
        ]
    ps -> foldl' (\cs p -> foldl1 S.union (S.map (partitionCuboid [p]) cs)) (S.singleton c) ps
    --[p1, p2] -> foldl1 S.union (S.map (partitionCuboid [p2]) (partitionCuboid [p1] c))
    --[p1, p2, p3, p4] -> foldl1 S.union (S.map (partitionCuboid [p3, p4]) (partitionCuboid [p1, p2] c))
    --[p1, p2, p3, p4, p5, p6, p7, p8] -> foldl1 S.union (S.map (partitionCuboid [p5, p6, p7, p8]) (partitionCuboid [p1, p2, p3, p4] c))

-- the pivots need to be in increasing x, then increasing y, then increasing z
sortPivots :: [(Int, Int, Int)] -> [(Int, Int, Int)]
sortPivots = sortOn fst3 . sortOn snd3 . sortOn thd3

-- Takes the left cuboid and splits it into many according to the right cuboid.
-- We just need to get the locations of the right's corners that are inside the left
-- There are either 0, 1, 2, or 4 points in 2D
-- In 3d.. erm I guess 0, 1, 2, 4, or 8 depending on amount of overlap
-- This fully determines the partition
splitAround :: Cuboid -> Cuboid -> Set Cuboid
splitAround a b =
  let pivots = (filter (inside b) (corners a))
   in traceShow ("about to partition", a, b, pivots)$
      partitionCuboid pivots b

-- Takes the existing, non-overlapping handled cuboids
-- and the new cuboid to split.
-- Returns the existing cuboids split to align with overlaps from the new one, and the new one also split per the alignment
-- Shared segments exist in both
-- So the caller can decide whether to union or delete
splitCuboids :: Set Cuboid -> Cuboid -> (Set Cuboid, Set Cuboid)
splitCuboids existing new
  | null existing = (existing, S.singleton new)
  | otherwise =
  --traceShow ("splitting", S.size existing, new) $
  -- On the left, we take all existing non-overlapping cuboids, and split them using this new one as a guide
  -- Then, on the right, we take this new cuboid as a starting set, and split it up into many non-overlapping pieces
    let existing' = foldl1 S.union (S.map (splitAround new) existing)
        new' = foldl' (\news e -> foldl1 S.union (S.map (splitAround e) news)) (S.singleton new) existing
     in (existing', new')

runFast :: Set Cuboid -> Instruction -> Set Cuboid
runFast onCuboids i@(Instruction t c) =
  traceShow i $
  let (onCuboids', c') = splitCuboids onCuboids c
   in case t of
        On -> onCuboids' `S.union` c'
        Off -> onCuboids' `S.difference` c'

-- 11185745482800 is too low

-- 2758514936282235 is example correct
-- 610171902584244 is what we get for example

part2 :: Int
part2 =
  -- $(input 22)
  $(exampleInput 22)
    & parseLinesWith instruction
    & foldl' runFast S.empty
    & S.toList
    & fmap volume
    & sum

tests :: IO ()
tests = do
  let a = ((10, 20), (30, 40), (50, 60))
      b = ((15, 17), (37, 39), (52, 54))
      c = ((100,200),(300,400),(500,600))
      d = ((19,21), (30, 40), (50, 60))
  print $ volume a
  print $ volume b
  let (lhs, rhs) = splitCuboids (S.singleton a) d
  print (length lhs)
  print (length rhs)
  print lhs
  print rhs
  print (sum (volume <$> S.toList (lhs `S.union` rhs)))
  print (sum (volume <$> S.toList (lhs `S.difference` rhs)))
  print $ sum (volume <$> S.toList lhs)
  print $ sum (volume <$> S.toList rhs)
  let e = ((-10, 10), (-10, 10), (-10,10))
  -- print $ S.filter ((/=0) . volume) $ partitionCuboid [(0,0,0), (0,0,2), (0,2,0),(0,2,2), (2,0,0), (2,0,2), (2,2,0),(2,2,2)] e
  print $ partitionCuboid [(0,0,0), (2,2,2)] e

-- TODO: debug me
overlapN :: Cuboid -> Cuboid -> Int
overlapN (axr, ayr, azr) (bxr, byr, bzr) = overlapR axr bxr * overlapR ayr byr * overlapR azr bzr
  where
    overlapR (a0, a1) (b0, b1)
      | a1 < b0 || b1 < a0 = 0
      | otherwise = min (a1 - b0) (b1 - a0)

-- If this is an On, then we're adding volume-c cells
-- but then we need to take off any cells we share with the last
-- and any cells we share with the one before that
tally :: Instruction -> [Instruction] -> Int -> ([Instruction], Int)
tally i@(Instruction On c) [] s = ([i], s + volume c)
tally i@(Instruction On c) done s = (i:done, s + volume c)
  
