module TwentySixteen.Day2 where

import Coord (move, udlrToDir2)
import Data.Char (intToDigit)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Grid (Grid)
import Util ((<$$>))

inputPath :: String
inputPath = "input/2016/2.txt"

keypad1 :: Grid Char
keypad1 =
  M.fromList
    [ ((x, y), intToDigit $ 3 * y + x + 1)
      | x <- [0 .. 2],
        y <- [0 .. 2]
    ]

keypad2 :: Grid Char
keypad2 =
  M.fromList
    [ ((2, 0), '1'),
      ((1, 1), '2'),
      ((2, 1), '3'),
      ((3, 1), '4'),
      ((0, 2), '5'),
      ((1, 2), '6'),
      ((2, 2), '7'),
      ((3, 2), '8'),
      ((4, 2), '9'),
      ((1, 3), 'A'),
      ((2, 3), 'B'),
      ((3, 3), 'C'),
      ((2, 4), 'D')
    ]

solve :: Grid Char -> IO String
solve keypad = do
  dss <- (udlrToDir2 <$$>) . lines <$> readFile inputPath
  return $
    (keypad M.!)
      . foldl'
        ( \pos dir ->
            let pos' = move dir 1 pos
             in if pos' `M.member` keypad
                  then pos'
                  else pos
        )
        (1, 1)
      <$> dss

part1 :: IO String
part1 = solve keypad1

part2 :: IO String
part2 = solve keypad2
