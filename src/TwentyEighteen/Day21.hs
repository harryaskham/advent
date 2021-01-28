module TwentyEighteen.Day21 where

import Data.Bits (Bits ((.&.), (.|.)))
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import TwentyEighteen.Day16 (Register (Register))
import TwentyEighteen.Day19
  ( Machine (..),
    instructions,
    stepMachine,
  )
import Util (input, readWithParser)

runMachineBreak :: (Machine -> Maybe a) -> Machine -> [a]
runMachineBreak breakpoint m = do
  let stepped =
        case stepMachine m of
          Nothing -> []
          Just m -> runMachineBreak breakpoint m
  case breakpoint m of
    Just v -> v : stepped
    Nothing -> stepped

part1 :: IO Int
part1 = do
  (boundReg, is) <- readWithParser instructions <$> input 2018 21
  let mem = M.fromList (zip (Register <$> [0 .. 5]) (repeat 0))
      machine = Machine (Register boundReg) 0 is mem
      b (Machine _ _ _ mem) =
        if mem M.! Register boundReg == 29
          then Just (mem M.! Register 5)
          else Nothing
  return . head $ runMachineBreak b machine

runFast :: [Int]
runFast = go 65536 13431073
  where
    go r4 r5 =
      if 256 > r4 then r5' : go r4'' r5'' else go r4' r5'
      where
        r5' = (((r5 + (r4 .&. 255)) .&. 16777215) * 65899) .&. 16777215
        r4' = r4 `div` 256
        r4'' = r5' .|. 65536
        r5'' = 13431073

lastBeforeCycle :: Set Int -> [Int] -> [Int] -> Int
lastBeforeCycle seen (x : xs) acc =
  if x `S.member` seen
    then head acc
    else lastBeforeCycle (S.insert x seen) xs (x : acc)

part2 :: Int
part2 = lastBeforeCycle S.empty runFast []
