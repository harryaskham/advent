module Day24 where

import Control.Monad.Par
import Control.Parallel.Strategies
import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.Char (digitToInt)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Mod
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Read
import Data.Vector qualified as V hiding (notElem)
import Extra (chunksOf)
import Helper.Grid
import Helper.TH
import Helper.Tracers
import Helper.Util hiding (count)
import Text.ParserCombinators.Parsec ()
import Text.ParserCombinators.Parsec hiding ((<|>))
import Prelude hiding (get)

newtype Var = Var Char deriving (Show, Eq, Ord)

data Arg = ArgVar Var | ArgLit Int deriving (Show, Eq, Ord)

data Operation
  = Inp Var
  | Add Var Arg
  | Mul Var Arg
  | Div Var Arg
  | Mod Var Arg
  | Eql Var Arg
  deriving (Show, Eq, Ord)

line :: GenParser Char () Operation
line =
  let var = Var <$> letter
      arg = ArgVar <$> var <|> ArgLit <$> number
      inp = Inp <$> (string "inp " *> var)
      add = Add <$> (string "add " *> var) <*> (char ' ' *> arg)
      mul = Mul <$> (string "mul " *> var) <*> (char ' ' *> arg)
      div = Div <$> (string "div " *> var) <*> (char ' ' *> arg)
      mod = Mod <$> (string "mod " *> var) <*> (char ' ' *> arg)
      eql = Eql <$> (string "eql " *> var) <*> (char ' ' *> arg)
   in try inp <|> try add <|> try mul <|> try div <|> try mod <|> try eql

findValid :: V.Vector Int -> [Operation] -> V.Vector Int
findValid s allOps = go 0 s initMem allOps
  where
    initMem = M.fromList $ [(Var 'x',), (Var 'y',), (Var 'z',), (Var 'w',)] <*> pure 0
    val mem (ArgVar v) = mem M.! v
    val _ (ArgLit v) = v
    go :: Int -> V.Vector Int -> Map Var Int -> [Operation] -> V.Vector Int
    go inpCount s mem [] =
      if mem M.! Var 'z' == 0
        then s
        else go 0 (s V.// [(inpCount - 1, (s V.! inpCount - 1) - 1)]) initMem allOps
    go inpCount s mem (op : ops) =
      --traceShow (s, [mem M.! (Var v) | v <- ['w' .. 'z']]) $
      case op of
        Inp v ->
          pauseId $
            traceShow (s, [mem M.! (Var v) | v <- ['w' .. 'z']]) $
              case mem M.! (Var 'z') of
                0 ->
                  traceShow ("success block", inpCount) $
                    go (inpCount + 1) s (M.insert v (s V.! inpCount) mem) ops
                _ ->
                  traceShow ("fail early", inpCount, s) $
                    go 0 (s V.// [(inpCount - 1, (s V.! (inpCount - 1)) - 1)]) initMem allOps
        -- drop input char by 1
        Add v a -> go inpCount s (M.adjust (+ val mem a) v mem) ops
        Mul v a -> go inpCount s (M.adjust (* val mem a) v mem) ops
        Div v a -> go inpCount s (M.adjust (`div` val mem a) v mem) ops
        Mod v a -> go inpCount s (M.adjust (`mod` val mem a) v mem) ops
        Eql v a -> go inpCount s (M.insert v (fromEnum $ (mem M.! v) == val mem a) mem) ops

run :: V.Vector Int -> [Operation] -> Int
run s allOps = go 0 s initMem allOps
  where
    initMem = M.fromList $ [(Var 'x',), (Var 'y',), (Var 'z',), (Var 'w',)] <*> pure 0
    val mem (ArgVar v) = mem M.! v
    val _ (ArgLit v) = v
    go _ _ mem [] = mem M.! (Var 'z')
    go inpCount s mem (op : ops) =
      traceShow op $
        traceShow (s, [mem M.! (Var v) | v <- ['w' .. 'z']]) $
          case op of
            Inp v -> go (inpCount + 1) s (M.insert v (s V.! inpCount) mem) ops
            Add v a -> go inpCount s (M.adjust (+ val mem a) v mem) ops
            Mul v a -> go inpCount s (M.adjust (* val mem a) v mem) ops
            Div v a -> go inpCount s (M.adjust (`div` val mem a) v mem) ops
            Mod v a -> go inpCount s (M.adjust (`mod` val mem a) v mem) ops
            Eql v a -> go inpCount s (M.insert v (fromEnum $ (mem M.! v) == val mem a) mem) ops

runInt :: Int -> Int
runInt n =
  let ops = parseLinesWith line $(input 24)
   in run (V.fromList (digitToInt <$> show n)) ops

blocks =
  [ (1, 13, 10),
    (1, 11, 16),
    (1, 11, 0),
    (1, 10, 13),
    (26, -14, 7),
    (26, -4, 11),
    (1, 11, 11),
    (26, -3, 10),
    (1, 12, 16),
    (26, -12, 8),
    (1, 13, 15),
    (26, -12, 2),
    (26, -15, 5),
    (26, -12, 10)
  ]

-- Find ones where the sum of the positive blocks
-- equals the sum of the negative blocks

runBInt :: Int -> Int
runBInt x =
  let s = digitToInt <$> show x
   in if 0 `elem` s then 999999 else runBlocks s blocks

-- z must always leave zero at the start
-- becomes 10 + w
-- then we want numbers that divide it down below 26, becoming zero
-- where possible then we want
-- e.g. (10+w) mod 26 + 11 to be our next digit
-- on step 1, that's 19 mod 26 + 11 = 30 or anywhere from 22 to 30
-- that's impossible so we end up growing z

runBlocks :: [Int] -> [(Int, Int, Int)] -> Int
runBlocks ws blocks = go ws blocks 0
  where
    go _ [] z = z
    go (w : ws) ((d, a, b) : blocks) z =
      --traceShow (w, d, a, b, z, z `mod` 26, z `mod` 26 + a, z `div` d) $
      traceShow (z `mod` 26 + a) $
        let z' = z `div` d
         in if (z `mod` 26) + a == w
              then go ws blocks z'
              else go ws blocks (z' * 26 + (w + b))

brute :: Int
brute =
  L.head
    [ x
      | x <- [10 ^ 14, 10 ^ 14 -1 .. 10 ^ 13],
        let s = traceShowId $ digitToInt <$> show x,
        0 `notElem` s,
        let v = runBlocks s blocks,
        v == 0
    ]

bruteMap :: [(Int, Int)]
bruteMap =
  let as = [(10 ^ 14 -1), (10 ^ 14 - 2) .. (10 ^ 13)]
      --f x = x `mod` 1000000000
      f = runBInt
   in withStrategy
        (parBuffer 10000000 rdeepseq)
        (take 1 . filter ((== (0 :: Int)) . snd) . (fmap (\x -> (x, f x))) $ as)

parFilterMap :: (Show a, NFData b) => (b -> Bool) -> (a -> b) -> [a] -> Par [b]
parFilterMap p f as = do
  ibs <- mapM (spawn . return . f) as
  mapM get ibs

-- brutePar = runPar $ parFilterMap (/= 0) runBInt [(10 ^ 14 -1), (10 ^ 14 - 2) .. (10 ^ 13)]
brutePar = runPar $ parFilterMap (/= 0) runBInt [(10 ^ 14 -1), (10 ^ 14 - 2) .. (10 ^ 14 - 100)]

-- ((fmap (\x -> (x, f x))) $ as)

-- make a map for each block that tells you:
-- input (w, z) -> output
-- where z can be quite big and gets bigger each block
--part1 = bruteMap

part1 :: Int
part1 = 98998519596997 -- by hand

part1'' =
  let ops = parseLinesWith line $(input 24)
   in findValid (V.fromList $ digitToInt <$> "99999999999999") ops

part1' =
  let ops = parseLinesWith line $(input 24)
   in L.head [x | x <- [10 ^ 14, 10 ^ 14 -1 .. 10 ^ 13], let s = traceShowId $ digitToInt <$> show x, runBlocks s blocks == 0]

-- [ | (i, chunk) <- opChunks, s <- [1..9]]
-- L.head [x | x <- [10 ^ 14, 10 ^ 14 -1 .. 10 ^ 13], validMonad (traceShowId x) ops]

part2 :: Int
part2 = 31521119151421 -- by hand
