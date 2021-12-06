module Day6 (part1, part2, progeny) where

import Control.Monad.Memo (MonadMemo (memo), startEvalMemo)
import Helper.TH (input)
import Helper.Util (eol, number, parseWith)
import Text.ParserCombinators.Parsec (char, eof, sepBy)

fish :: [Int]
fish = $(input 6) & parseWith (number `sepBy` char ',' <* (eol >> eof))

progeny :: Int -> Int -> Int
progeny d' f' = startEvalMemo $ go (d', f')
  where
    go (0, _) = return 1
    go (d, 0) = do
      p1 <- memo go (d - 1, 6)
      p2 <- memo go (d - 1, 8)
      return $ p1 + p2
    go (d, f) = memo go (d - 1, f - 1)

part1 :: Int
part1 = sum (progeny 80 <$> fish)

part2 :: Int
part2 = sum (progeny 256 <$> fish)
