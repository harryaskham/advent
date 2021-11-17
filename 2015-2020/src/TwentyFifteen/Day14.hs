module TwentyFifteen.Day14 where

import Data.List (group, sort, transpose)
import Text.ParserCombinators.Parsec
  ( GenParser,
    digit,
    eof,
    letter,
    many,
    many1,
    string,
  )
import Util (eol, input, maxIndices, readWithParser)

reindeer :: GenParser Char () [(Int, Int, Int)]
reindeer = many line <* eof
  where
    line = do
      many1 letter
      string " can fly "
      speed <- read <$> many1 digit
      string " km/s for "
      duration <- read <$> many1 digit
      string " seconds, but then must rest for "
      rest <- read <$> many1 digit
      string " seconds."
      eol
      return (speed, duration, rest)

distanceAtTime :: Int -> (Int, Int, Int) -> Int
distanceAtTime t (speed, duration, rest) =
  sum
    . take t
    . cycle
    $ replicate duration speed ++ replicate rest 0

part1 :: IO Int
part1 = do
  rs <- readWithParser reindeer <$> input 2015 14
  return . maximum $ distanceAtTime 2503 <$> rs

distancesAtTime :: (Int, Int, Int) -> [Int]
distancesAtTime (speed, duration, rest) =
  scanl1 (+)
    . cycle
    $ replicate duration speed ++ replicate rest 0

part2 :: IO Int
part2 = do
  rs <- readWithParser reindeer <$> input 2015 14
  return
    . maximum
    . fmap length
    . group
    . sort
    . concat
    $ maxIndices <$> (take 2503 $ transpose (distancesAtTime <$> rs))
