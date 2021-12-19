module Main where

import Data.Text qualified as T
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
import Test.BenchPress (benchMany)

main :: IO ()
--main = benchAllDays
--main = benchOne (Day5.part1, Day5.part2)
--main = benchEachPart
main = print Day19.part1

benchOne :: Show a => a -> IO ()
benchOne a = benchMany 1 [("One Thing", putTextLn (show a))]

benchEachPart :: IO ()
benchEachPart =
  benchMany
    1
    ( fmap
        ( \(d, p, r) ->
            let t = "Day " <> show (d :: Integer) <> " - Part " <> show (p :: Integer)
             in (T.unpack t, putTextLn (t <> ": " <> r))
        )
        $runAllDays
    )

benchAllDays :: IO ()
benchAllDays =
  benchMany
    1
    [ ( "All Days",
        mapM_
          ( \(d, p, r) ->
              let t = "Day " <> show (d :: Integer) <> " - Part " <> show (p :: Integer)
               in putTextLn (t <> ": " <> r)
          )
          $runAllDays
      )
    ]
