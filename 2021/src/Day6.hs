module Day6 (part1, part2) where

import Control.Monad.Memo (MonadMemo (memo), startEvalMemo)
import Helper.TH (input)
import Helper.Util (eol, number, parseWith)
import Text.ParserCombinators.Parsec (char, eof, sepBy)

fish :: [Int]
fish = parseWith (number `sepBy` char ',' <* (eol >> eof)) $(input 6)

progeny :: Int -> Int
progeny d' = startEvalMemo (sum <$> mapM (memo go . (d',)) fish)
  where
    go (0, _) = return 1
    go (d, 0) = (+) <$> memo go (d - 1, 6) <*> memo go (d - 1, 8)
    go (d, f) = memo go (d - 1, f - 1)

part1 :: Int
part1 = progeny 80

part2 :: Int
part2 = progeny 256
