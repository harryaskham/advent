module Day12 (part1, part2) where

line :: Parser (String, [ℤ'])
line = (,) <$> (many1 (oneOf ".#?") <* string " ") <*> (number `sepBy` string ",")

ways :: (String, [ℤ']) -> Memo (String, [ℤ']) ℤ' ℤ'
ways (ss, []) = return (bool 0 1 (all (∈ (".?" :: String)) ss))
ways (ss, c : cs) =
  treverse
    sum
    [ memo ways (drop (c + i) ss, cs)
      | i <- [1 .. length ss - c],
        '#' ∉ mkSet (take i ss),
        '.' ∉ mkSet (take c (drop i ss))
    ]

solve :: ((String, [ℤ']) -> (String, [ℤ'])) -> ℤ'
solve f = $(input 12) & parseLinesWith line & fmap (f >>> first ('.' :) >>> ways >>> startEvalMemo) & sum

part1 :: ℤ'
part1 = solve id

part2 :: ℤ'
part2 = solve (bimap (intercalate "?" . replicate 5) (mconcat . replicate 5))
