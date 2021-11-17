module TwentyFifteen.Day1 where

import Data.List (elemIndex, foldl')
import Util (input)

part1 :: IO Int
part1 =
  foldl' (\acc paren -> if paren == '(' then acc + 1 else acc - 1) 0
    . head
    . lines
    <$> input 2015 1

part2 :: IO (Maybe Int)
part2 =
  elemIndex (-1)
    . scanl (\acc paren -> if paren == '(' then acc + 1 else acc - 1) 0
    . head
    . lines
    <$> input 2015 1
