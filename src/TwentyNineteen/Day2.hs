module TwentyNineteen.Day2 where

import Data.List.Extra (splitOn)
import qualified Data.Vector as V
import Util (input)

runOp :: (Int -> Int -> Int) -> Int -> Int -> Int -> V.Vector Int -> V.Vector Int
runOp op loc1 loc2 locR program = program V.// [(locR, res)]
  where
    x1 = program V.! loc1
    x2 = program V.! loc2
    res = op x1 x2

runProgramD2 :: Int -> ([String], V.Vector Int) -> ([String], V.Vector Int)
runProgramD2 counter (logs, program) =
  case program V.! counter of
    99 -> (l : logs, program)
    1 -> runProgramD2 (counter + 4) (l : logs, runOp (+) (program V.! (counter + 1)) (program V.! (counter + 2)) (program V.! (counter + 3)) program)
    2 -> runProgramD2 (counter + 4) (l : logs, runOp (*) (program V.! (counter + 1)) (program V.! (counter + 2)) (program V.! (counter + 3)) program)
    _ -> error ("invalid opcode " ++ show (program V.! counter))
  where
    l = show (counter, program)

part1 :: IO Int
part1 = do
  program <- V.fromList . fmap read . splitOn "," . head . lines <$> input 2019 2
  let (_, finalProgram) = runProgramD2 0 ([], program V.// [(1, 12), (2, 2)])
  return $ head . V.toList $ finalProgram

part2 :: IO Int
part2 = do
  program <- V.fromList . fmap read . splitOn "," . head . lines <$> input 2019 2
  let variants = [[(1, noun), (2, verb)] | noun <- [0 .. 99], verb <- [0 .. 99]]
      allRuns = zip (runProgramD2 0 <$> [([], program V.// variant) | variant <- variants]) variants
      (_, variant) = head $ filter (\((_, p), _) -> p V.! 0 == 19690720) allRuns
  return $ (100 * snd (head variant)) + snd (variant !! 1)
