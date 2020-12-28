module TwentySeventeen.Day16 where

import Control.Arrow
import Control.Monad
import qualified Data.Foldable as F
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Sequence as SQ
import Debug.Trace
import Text.ParserCombinators.Parsec
import Util

inputPath :: String
inputPath = "input/2017/16.txt"

data Programs = Programs (M.Map Int Char) (M.Map Char Int) Int

parseOps :: GenParser Char () (SQ.Seq Char -> SQ.Seq Char)
parseOps = do
  ops <- op `sepBy` char ','
  char '\n'
  eof
  return $ foldl1 (>>>) ops
  where
    op =
      choice $
        try
          <$> [ do
                  char 'x'
                  posA <- many1 digit
                  char '/'
                  posB <- many1 digit
                  return $ exchange (read posA) (read posB),
                do
                  char 's'
                  x <- many1 digit
                  return $ spin (read x),
                do
                  char 'p'
                  a <- letter
                  char '/'
                  b <- letter
                  return $ partner a b
              ]

spin :: Int -> SQ.Seq Char -> SQ.Seq Char
spin x seq =
  let (a, b) = SQ.splitAt (length seq - x) seq
   in b SQ.>< a

exchange :: Int -> Int -> SQ.Seq Char -> SQ.Seq Char
exchange posA posB seq =
  let (a, b) = (seq `SQ.index` posA, seq `SQ.index` posB)
   in SQ.update posA b . SQ.update posB a $ seq

partner :: Char -> Char -> SQ.Seq Char -> SQ.Seq Char
partner a b seq =
  let (Just posA, Just posB) = (SQ.elemIndexL a seq, SQ.elemIndexL b seq)
   in exchange posA posB seq

part1 :: IO String
part1 = do
  dance <- readWithParser parseOps <$> readFile inputPath
  return $ F.toList $ dance (SQ.fromList ['a' .. 'p'])

run :: SQ.Seq Char -> (Int, (SQ.Seq Char -> SQ.Seq Char)) -> SQ.Seq Char
run seq (i, dance) = trace (show i) $ dance seq

part2 :: IO String
part2 = do
  dance <- readWithParser parseOps <$> readFile inputPath
  let seq = SQ.fromList ['a' .. 'p']
  return $ F.toList $ foldl' run seq (zip [1 .. 100] (repeat dance))
