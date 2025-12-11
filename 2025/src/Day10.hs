module Day10 (part1, part2) where

xs :: [(𝕊, [[ℤ]], [ℤ])] =
  $(aoc 10)
    & (⋮⊏) @([TupSep " " ((Between "[" "]" (𝕊 ⭀ ".#")), [Between "(" ")" (CSVAny ℤ)], (Between "{" "}" (CSVAny ℤ)))] ≠ [])

tog :: Char -> Char
tog '#' = '.'
tog '.' = '#'

press1 :: (𝕊, [[ℤ]], [ℤ]) -> ℤ -> 𝕊
press1 (ls, bs, js) bi = ((Ł (\ls li -> ls !. (li, tog (ls !! li))) ls (bs !! bi)) !>)

press2 :: [ℤ] -> [[ℤ]] -> ℤ -> [ℤ]
press2 cs bs bi = ((Ł (\cs ci -> cs !. (ci, (cs !! ci) + 1)) cs (bs !! bi)) !>)

press :: (𝕊, [[ℤ]], [ℤ]) -> ℤ
press (ls', bs, js) =
  let lsE = const '.' <$> ls'
      go :: Set 𝕊 -> Seq (ℤ, 𝕊) -> ℤ
      go seen ((n, ls) :<| q)
        | ls == ls' = n
        | ls ∈ seen = go seen q
        | otherwise = go (ls |-> seen) (foldl' (\q bi -> q |> (n + 1, press1 (ls, bs, js) bi)) q (range 0 ((bs |.|) - 1)))
   in go (∅) (mk₁ (0, lsE))

presso :: (𝕊, [[ℤ]], [ℤ]) -> ℤ
presso (_, bs, js) =
  let loss (n, cs) = n + maximum ((js, cs) ⤊ (-))
      csE = const 0 <$> js
      go :: Maybe ℤ -> Set [ℤ] -> MinQ ℤ (ℤ, [ℤ]) -> ℤ
      go (Just best) _ NullQ = best
      go best seen ((_, (n, cs)) :<! q)
        | cs == js = n
        | cs == js ∧ isJust best ∧ Just n < best = go (Just n) seen q
        | cs == js ∧ isJust best = go best seen q
        | cs == js ∧ isNothing best = go (Just n) seen q
        | cs ∈ seen = go best seen q
        | isJust best ∧ Just n >= best = go best seen q
        | or ((cs, js) ⤊ (>)) = go best seen q
        | otherwise =
            traceShow (size q, n, cs, js) $
              go best (cs |-> seen) (qAppend loss [(n + 1, press2 cs bs bi) | bi <- range 0 ((bs |.|) - 1)] q)
   in go Nothing (∅) (mkQ₁ loss (0, csE))

part1 :: ℤ = sum (press <$> xs)

part2 :: ℤ = sum (traceShowId . presso <$> xs)
