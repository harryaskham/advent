module TwentyFifteen.Day13 where

import Data.List (foldl', permutations)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Text.ParserCombinators.Parsec
  ( GenParser,
    char,
    digit,
    eof,
    letter,
    many,
    many1,
    spaces,
    string,
  )
import Util (eol, input, readWithParser, (<$$>))

happiness :: GenParser Char () (Map String (Map String Int))
happiness = M.fromList <$$> (M.fromListWith (++) <$> (many line <* eof))
  where
    line = do
      name1 <- many1 letter
      string " would "
      gainLose <- many1 letter
      spaces
      score <- read <$> many1 digit
      string " happiness units by sitting next to "
      name2 <- many1 letter
      char '.'
      eol
      let score' =
            case gainLose of
              "gain" -> score
              "lose" -> negate score
      return (name1, [(name2, score')])

scoreArrangement :: Map String (Map String Int) -> [String] -> Int
scoreArrangement h xs =
  foldl'
    (\acc (a, b) -> acc + h M.! a M.! b + h M.! b M.! a)
    0
    (zip xs (drop 1 $ cycle xs))

part1 :: IO Int
part1 = do
  h <- readWithParser happiness <$> input 2015 13
  return $ maximum (scoreArrangement h <$> permutations (M.keys h))

part2 :: IO Int
part2 = do
  h <- readWithParser happiness <$> input 2015 13
  let me = "harry"
      h' = M.insert me (M.fromList (zip (M.keys h) (repeat 0))) h
      h'' = foldl' (\acc person -> M.adjust (M.insert me 0) person acc) h' (M.keys h')
  return $ maximum (scoreArrangement h'' <$> permutations (M.keys h''))
