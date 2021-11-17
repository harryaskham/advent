module TwentySixteen.Day25 where

import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Vector (Vector)
import qualified Data.Vector as V
import TwentySixteen.Day12
  ( Instruction,
    Machine (..),
    instructions,
    step,
  )
import Util (input, readWithParser)

outStream :: Vector Instruction -> Machine -> [Maybe Int]
outStream is m@(Machine pc _) =
  case is V.!? pc of
    Nothing -> []
    Just i -> let (is', m', out) = step i is m in out : outStream is' m'

part1 :: IO Int
part1 = do
  is <- readWithParser instructions <$> input 2016 25
  let machines = [Machine 0 (M.fromList (zip ['a' .. 'd'] [x, 0, 0, 0])) | x <- [0 ..]]
      outputs = zip [0 ..] (take 10 . catMaybes . outStream (V.fromList is) <$> machines)
      goodSignal signal = signal == take (length signal) (cycle [0, 1])
      goodOutputs = filter (goodSignal . snd) outputs
  return . fst . head $ goodOutputs
