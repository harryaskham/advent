module TwentyFifteen.Day8 where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Util (countMap, input)

escape :: String -> String
escape [] = []
escape [x] = [x]
escape ('\\' : '\\' : xs) = '\\' : escape xs
escape ('\\' : '"' : xs) = '"' : escape xs
escape ('\\' : 'x' : a : b : xs) = 'x' : escape xs
escape (x : xs) = x : escape xs

part1 :: IO Int
part1 = do
  rawStrings <- lines <$> input 2015 8
  let evalStrings = (\s -> escape $ drop 1 $ take (length s - 1) s) <$> rawStrings
  return $ sum (length <$> rawStrings) - sum (length <$> evalStrings)

part2 :: IO Int
part2 = do
  rawStrings <- lines <$> input 2015 8
  let enLen s =
        let cm = countMap s
         in 2 + length s
              + fromMaybe 0 (M.lookup '"' cm)
              + fromMaybe 0 (M.lookup '\\' cm)
  return $ sum (enLen <$> rawStrings) - sum (length <$> rawStrings)
