module Day24 (part1, part2) where

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
import Data.Vector qualified as V
import Helper.Coord
import Helper.Grid
import Helper.TH
import Helper.Tracers
import Helper.Util hiding (count)
import Text.ParserCombinators.Parsec hiding ((<|>))

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

validMonad :: Int -> [Operation] -> Bool
validMonad n ops
  | '0' `elem` s = False
  | otherwise = go s initMem ops
  where
    initMem = M.fromList $ [(Var 'x',), (Var 'y',), (Var 'z',), (Var 'w',)] <*> pure 0
    val mem (ArgVar v) = mem M.! v
    val _ (ArgLit v) = v
    s = show n
    go :: String -> Map Var Int -> [Operation] -> Bool
    go [] mem [] = mem M.! Var 'z' == 0
    go (_ : _) _ [] = error "Did not consume all inputs"
    go ss mem (op : ops) =
      traceShow s $
        let (mem', ss') =
              case op of
                Inp v -> case ss of
                  [] -> error "input without input"
                  (s : ss') -> (M.insert v (digitToInt s) mem, ss')
                Add v a -> (M.adjust (+ val mem a) v mem, ss)
                Mul v a -> (M.adjust (* val mem a) v mem, ss)
                Div v a -> (M.adjust (`div` val mem a) v mem, ss)
                Mod v a -> (M.adjust (`mod` val mem a) v mem, ss)
                Eql v a -> (M.insert v (fromEnum $ (mem M.! v) == val mem a) mem, ss)
         in go ss' mem' ops

part1 :: Int
part1 =
  let ops = parseLinesWith line $(input 24)
   in L.head [x | x <- [10 ^ 14, 10 ^ 14 -1 .. 10 ^ 13], let s = show x, validMonad x ops]

part2 :: Text
part2 = "Part 2"
