module Day12 (part1, part2) where

data Spring = NoSpring | Spring | Unknown deriving (Show, Eq, Ord)

mkSpring :: Char -> Spring
mkSpring '.' = NoSpring
mkSpring '#' = Spring
mkSpring '?' = Unknown

line :: Parser ([Spring], [Int])
line = (,) <$> ((mkSpring <$$> (many1 (oneOf ".#?"))) <* string " ") <*> (number `sepBy` string ",")

unfold :: ([Spring], [Int]) -> ([Spring], [Int])
unfold (ss, ns) = (intercalate [Unknown] (replicate 5 ss), mconcat (replicate 5 ns))

solve :: (([Spring], [Int]) -> ([Spring], [Int])) -> Int
solve f = $(input 12) & parseLinesWith line & fmap (f >>> first (NoSpring :) >>> ways >>> startEvalMemo) & sum

ways :: ([Spring], [Int]) -> Memo ([Spring], [Int]) Int Int
ways (ss, []) = return (bool 0 1 (all (∈ mkSet [NoSpring, Unknown]) ss))
ways (ss, (c : cs)) =
  treverse
    sum
    [ memo ways ((drop (c + i) ss), cs)
      | i <- [1 .. length ss - c],
        Spring ∉ mkSet (take i ss),
        NoSpring ∉ mkSet (take c (drop i ss))
    ]

part1 :: Int
part1 = solve id

part2 :: Int
part2 = solve unfold
