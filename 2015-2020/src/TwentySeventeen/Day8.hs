module TwentySeventeen.Day8 where

import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

inputPath :: String
inputPath = "input/2017/8.txt"

data Instruction = Instruction (M.Map String Int -> M.Map String Int) (M.Map String Int -> Bool)

parseInstruction :: String -> Instruction
parseInstruction line =
  let [updateReg, incDec, updateAmount, _, condReg, condOp', condAmount] = splitOn " " line
      updateOp = case incDec of
        "inc" -> (+)
        "dec" -> (-)
      condOp = case condOp' of
        ">" -> (>)
        "<" -> (<)
        ">=" -> (>=)
        "<=" -> (<=)
        "==" -> (==)
        "!=" -> (/=)
   in Instruction
        ( \m ->
            let x = M.findWithDefault 0 updateReg m
             in M.insert updateReg (x `updateOp` read updateAmount) m
        )
        ( \m ->
            let x = M.findWithDefault 0 condReg m
             in x `condOp` read condAmount
        )

runInstruction :: M.Map String Int -> Instruction -> M.Map String Int
runInstruction mem (Instruction update cond) =
  if cond mem then update mem else mem

part1 :: IO Int
part1 = do
  is <- fmap parseInstruction . lines <$> readFile inputPath
  let mem = foldl' runInstruction M.empty is
  return $ maximum (M.elems mem)

part2 :: IO Int
part2 = do
  is <- fmap parseInstruction . lines <$> readFile inputPath
  let mems = scanl runInstruction M.empty is
  return $ maximum (M.elems =<< mems)
