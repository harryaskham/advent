module Day24 where

(initial, circuit) :: (Map String 𝔹, Map String (String, String, String)) =
  $(aoc 24)
    |- ( (,)
           <$> ((mapSep (string ": ") eol abc123 (as @𝔹 <$> number @ℤ)) <* eol)
           <*> ((\m -> mkWith (<>) [(w, op) | (op, ws) <- unMap m, w <- ws]) <$> (mapcat "-> " (tuple @3 (abcABC123 <* spaces)) abc123))
       )

value :: Map String 𝔹 -> String -> 𝔹
value initial w
  | w ∈ initial = initial |! w
  | otherwise =
      case circuit |! w of
        (w, "AND", y) -> value initial w ∧ value initial y
        (w, "OR", y) -> value initial w ∨ value initial y
        (w, "XOR", y) -> value initial w `xor` value initial y
        e -> error $ tshow e

(!+!) :: ℤ -> ℤ -> ℤ
x !+! y = go (to 'x' x <> to 'y' y)
  where
    to c x =
      traceShowId $
        mkMap [(c : (show n), b) | (n, b) <- zip [0 ..] (reverse (intToBits x))]

test = [a !+! b ≡ a + b | a <- [1 .. 10], b <- [1 .. 10]]

go :: Map String 𝔹 -> ℤ
go initial =
  bitsToInt $
    snd
      <$> sortOn
        (Down ∘ fst)
        [ (n |- number @ℤ, value initial w)
          | w@(z : n) <- nub (keys initial <> keys circuit <> ((\x -> [fst3 x, thd3 x]) =<< values circuit)),
            z ≡ 'z'
        ]

part1 :: ℤ
part1 = go initial

part2 = test
