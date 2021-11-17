module TwentyEighteen.Day10 where

import Control.Monad (when)
import Data.List (intercalate)
import qualified Data.Set as S
import Text.ParserCombinators.Parsec
  ( GenParser,
    char,
    eof,
    many,
    many1,
    oneOf,
    string,
  )
import Util (eol, input, readWithParser)

data Particle = Particle (Int, Int) (Int, Int) deriving (Show)

particles :: GenParser Char () [Particle]
particles = do
  ps <- many particle
  eof
  return ps
  where
    number = read <$> many1 (oneOf "-0123456789 ")
    particle = do
      string "position=<"
      x <- number
      char ','
      y <- number
      string "> velocity=<"
      vx <- number
      char ','
      vy <- number
      char '>'
      eol
      return $ Particle (x, y) (vx, vy)

step :: Particle -> Particle
step (Particle (x, y) v@(vx, vy)) = Particle (x + vx, y + vy) v

getX :: Particle -> Int
getX (Particle (x, _) _) = x

getY :: Particle -> Int
getY (Particle (_, y) _) = y

interesting :: [Particle] -> Bool
interesting ps = abs (maxX - minX) < threshold && abs (maxY - minY) < threshold
  where
    threshold = 200
    xs = getX <$> ps
    ys = getY <$> ps
    maxX = maximum xs
    minX = minimum xs
    maxY = maximum ys
    minY = minimum ys

run :: Int -> [Particle] -> IO ()
run n ps = do
  when (interesting ps) (prettyPrint n ps)
  run (n + 1) (step <$> ps)

prettyPrint :: Int -> [Particle] -> IO ()
prettyPrint n ps = do
  let xs = getX <$> ps
      ys = getY <$> ps
      maxX = maximum xs
      minX = minimum xs
      maxY = maximum ys
      minY = minimum ys
      xs' = subtract minX <$> xs
      ys' = subtract minY <$> ys
      xys = S.fromList $ zip xs' ys'
      width = maxX - minX
      height = maxY - minY
      ls = [[if (x, y) `S.member` xys then '#' else ' ' | x <- [0 .. width]] | y <- [0 .. height]]
  print n
  putStrLn $ intercalate "\n" ls

part1 :: IO ()
part1 = do
  ps <- readWithParser particles <$> input 2018 10
  run 0 ps
