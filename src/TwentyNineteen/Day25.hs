{-# LANGUAGE QuasiQuotes #-}

module TwentyNineteen.Day25 where

import Control.Lens (set, view)
import Control.Monad.Loops (iterateM_)
import Data.Char (chr, ord)
import Data.List (intercalate, isInfixOf, isSuffixOf, sortOn)
import Text.RawString.QQ (r)
import TwentyNineteen.Intcode
  ( Machine (Machine),
    outputs,
    readProgram,
    stepProgram,
  )
import Util (powerset)

directions :: String
directions =
  [r|west
take cake
west
take pointer
west
south
take tambourine
east
east
east
take mug
west
west
west
north
east
south
take monolith
north
east
east
south
take coin
east
take mouse
south
south
take hypercube
north
north
west
south
west
north
north
|]

dropAttempts :: [String]
dropAttempts =
  sortOn length $
    [ intercalate "\n" ((("drop " ++) <$> items) ++ ["north"] ++ (("take " ++) <$> items)) ++ "\n"
      | items <-
          powerset
            [ "cake",
              "pointer",
              "tambourine",
              "mug",
              "monolith",
              "coin",
              "mouse",
              "hypercube"
            ]
    ]

runTurn :: Machine -> IO Machine
runTurn m = do
  m' <- stepProgram m
  let out = (chr . fromIntegral <$> view outputs m)
  if "Command?" `isSuffixOf` out
    then putStrLn out >> return (set outputs [] m')
    else
      if "Santa" `isInfixOf` out
        then putStrLn out >> return m'
        else return m'

part1 :: IO ()
part1 = do
  program <- readProgram "input/2019/25.txt"
  let machine =
        Machine
          0
          (fromIntegral . ord <$> (directions ++ concat dropAttempts))
          []
          program
          0
  iterateM_ runTurn machine
