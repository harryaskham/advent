module TwentyNineteen.Day17 where

import Control.Lens ((.~), (^.))
import Data.Foldable (sequenceA_)
import Data.Function ((&))
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import TwentyNineteen.Intcode
  ( Machine (Machine),
    inputs,
    outputs,
    readProgram,
    runProgram,
  )

data Space = Empty | Scaffold | RUp | RDown | RLeft | RRight | RFallen deriving (Eq)

spaceFromChar '.' = Empty
spaceFromChar '#' = Scaffold
spaceFromChar '^' = RUp
spaceFromChar 'v' = RDown
spaceFromChar '>' = RRight
spaceFromChar '<' = RLeft
spaceFromChar 'X' = RFallen
spaceFromChar e = error $ show e

instance Show Space where
  show Empty = "."
  show Scaffold = "#"
  show RUp = "^"
  show RDown = "v"
  show RRight = ">"
  show RLeft = "<"
  show RFallen = "X"

asciiToGrid :: [Integer] -> [[Space]]
asciiToGrid outputs = (fmap . fmap) spaceFromChar (take (length rows - 1) rows)
  where
    ascii = toEnum . fromIntegral <$> outputs
    rows = lines ascii

intersectionCoords :: [[Space]] -> [(Int, Int)]
intersectionCoords grid = filter isIntersection coords
  where
    coords = [(x, y) | x <- [1 .. length (head grid) - 2], y <- [1 .. length grid - 2]]
    isIntersection (x, y) =
      ((grid !! y !! x) == Scaffold)
        && ((grid !! (y + 1) !! x) == Scaffold)
        && ((grid !! (y -1) !! x) == Scaffold)
        && ((grid !! y !! (x + 1)) == Scaffold)
        && ((grid !! y !! (x -1)) == Scaffold)

part1 :: IO ()
part1 = do
  program <- readProgram "input/2019/17.txt"
  machine <- runProgram (Machine 0 [] [] program 0)
  let grid = asciiToGrid $ machine ^. outputs
      intersections = intersectionCoords grid
      alignmentParams = ((*) <$> fst <*> snd) <$> intersections
  sequenceA_ $ print <$> grid
  print $ sum alignmentParams

inputMovements :: String -> String -> String -> String -> Bool -> Machine -> Machine
inputMovements routine funcA funcB funcC cameraOn machine =
  machine & inputs .~ concat [asciiRoutine, asciiFuncA, asciiFuncB, asciiFuncC, asciiCamera]
  where
    asciiRoutine = fromIntegral . fromEnum <$> routine ++ "\n"
    asciiFuncA = fromIntegral . fromEnum <$> funcA ++ "\n"
    asciiFuncB = fromIntegral . fromEnum <$> funcB ++ "\n"
    asciiFuncC = fromIntegral . fromEnum <$> funcC ++ "\n"
    asciiCamera =
      [ if cameraOn
          then (fromIntegral . fromEnum) 'y'
          else (fromIntegral . fromEnum) 'n',
        (fromIntegral . fromEnum) '\n'
      ]

part2 :: IO ()
part2 = do
  program <- readProgram "input/2019/17.txt"
  let machine = Machine 0 [] [] (M.insert 0 2 program) 0
      moved =
        inputMovements
          "A,B,A,C,A,C,B,C,C,B"
          "L,4,L,4,L,10,R,4"
          "R,4,L,4,L,4,R,8,R,10"
          "R,4,L,10,R,10"
          False
          machine
  runMoved <- runProgram moved
  let rows = splitOn "\n" $ toEnum . fromIntegral <$> runMoved ^. outputs
  sequenceA_ $ print <$> rows
  print $ last $ runMoved ^. outputs
