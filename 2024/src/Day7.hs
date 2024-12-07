module Day7 (part1, part2) where

part1 :: Integer
part1 =
  let sat x y z =
        trying [
          lookAhead eol >> guard (y == z) $> x, sat x (y - a) <$> number, sat x (y `div` a) <$> number]
   in $(aocxn 7 1) |-<> (sat <$> (number <* string ": ") <*> wordOf number


part2 :: Text
part2 = "Part 2"
