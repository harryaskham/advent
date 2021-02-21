module TwentyFifteen.Day10 where

import Data.List (group)

step :: String -> String
step = concat . fmap (\g -> show (length g) ++ [head g]) . group

part12 :: [Int]
part12 = let xs = iterate step "3113322113" in length . (xs !!) <$> [40, 50]
