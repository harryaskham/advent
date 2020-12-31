module TwentySixteen.Day8 where

import Control.Arrow ((>>>))
import qualified Data.Foldable as F
import Data.List (intercalate)
import qualified Data.Sequence as SQ
import Text.ParserCombinators.Parsec
  ( GenParser,
    char,
    digit,
    eof,
    many,
    many1,
    string,
    try,
    (<|>),
  )
import Util (eol, readWithParser)

inputPath :: String
inputPath = "input/2016/8.txt"

data Cell = On | Off deriving (Eq)

type Screen = SQ.Seq (SQ.Seq Cell)

mkScreen :: Screen
mkScreen = SQ.replicate 6 (SQ.replicate 50 Off)

type Operation = Screen -> Screen

operations :: GenParser Char () Operation
operations = do
  os <- many operation
  eof
  return $ foldl1 (>>>) os
  where
    operation = try rectOp <|> try rotateROp <|> try rotateCOp
    rectOp = do
      string "rect "
      w <- many1 digit
      char 'x'
      h <- many1 digit
      eol
      return $ rect (read w) (read h)
    rotateCOp = do
      string "rotate column x="
      (x, n) <- rotateVals
      return $ rotateC x n
    rotateROp = do
      string "rotate row y="
      (y, n) <- rotateVals
      return $ rotateR y n
    rotateVals = do
      v <- many1 digit
      string " by "
      n <- many1 digit
      eol
      return (read v, read n)

rect :: Int -> Int -> Operation
rect w h grid = setRow <$> SQ.zip grid (SQ.fromList [0 .. SQ.length grid - 1])
  where
    setRow (row, y) = if y < h then SQ.replicate w On SQ.>< SQ.drop w row else row

rotateC :: Int -> Int -> Operation
rotateC x n grid = (\(row, v) -> SQ.update x v row) <$> SQ.zip grid col'
  where
    col = (`SQ.index` x) <$> grid
    (a, b) = SQ.splitAt (length col - n) col
    col' = b SQ.>< a

rotateR :: Int -> Int -> Operation
rotateR y n grid = SQ.update y (b SQ.>< a) grid
  where
    row = grid `SQ.index` y
    (a, b) = SQ.splitAt (length row - n) row

part1 :: IO Int
part1 = do
  doOps <- readWithParser operations <$> readFile inputPath
  return . sum . F.toList $ length . SQ.filter (== On) <$> doOps mkScreen

instance Show Cell where
  show On = "#"
  show Off = " "

printScreen :: Screen -> String
printScreen grid = intercalate "\n" $ printRow <$> F.toList grid
  where
    printRow row = concat (F.toList (show <$> row))

part2 :: IO ()
part2 = do
  doOps <- readWithParser operations <$> readFile inputPath
  putStrLn $ printScreen (doOps mkScreen)
