module Main where

import Day1 qualified
import Day10 qualified
import Day11 qualified
import Day12 qualified
import Day13 qualified
import Day14 qualified
import Day15 qualified
import Day16 qualified
import Day17 qualified
import Day18 qualified
import Day19 qualified
import Day2 qualified
import Day20 qualified
import Day21 qualified
import Day22 qualified
import Day23 qualified
import Day24 qualified
import Day25 qualified
import Day3 qualified
import Day4 qualified
import Day5 qualified
import Day6 qualified
import Day7 qualified
import Day8 qualified
import Day9 qualified
import Helper.TH (runAllDays)

main :: IO ()
main =
  mapM_
    ( \(d, p, r) ->
        putTextLn $
          "Day "
            <> show (d :: Integer)
            <> " - Part "
            <> show (p :: Integer)
            <> ": "
            <> r
    )
    $runAllDays
