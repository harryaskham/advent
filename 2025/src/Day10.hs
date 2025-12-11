module Day10 where

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
  let -- loss (n, cs) = n * maximum ((js, cs) ⤊ ((\j c -> j `div` (c + 1))))
      -- loss (n, cs) = let ds = (js, cs) ⤊ (-) in n + maximum ds
      -- loss (n, cs) = let ds = (js, cs) ⤊ (-) in n * n + sum ((ds, ds) ⤊ (*))
      -- loss (n, cs) = let ds = (js, cs) ⤊ (-) in (sum ((ds, ds) ⤊ (*)), n + maximum ds)
      loss (n, cs) = let ds = (js, cs) ⤊ (-) in (n + maximum ds, sum ((ds, ds) ⤊ (*)))
      csE = const 0 <$> js
      go :: Maybe ℤ -> Set [ℤ] -> MinQ ℤ² (ℤ, [ℤ]) -> ℤ
      go (Just best) _ NullQ = best
      go best seen ((l, (n, cs)) :<! q)
        | cs == js = n
        -- \| cs == js ∧ isJust best ∧ Just n < best = go (Just n) seen q
        -- \| cs == js ∧ isJust best = go best seen q
        -- \| cs == js ∧ isNothing best = go (Just n) seen q
        -- \| isJust best ∧ Just n >= best = go best seen q
        | or ((cs, js) ⤊ (>)) = go best seen q
        -- TODO: validate i.e. if we need to prexx 20 times, might need to overflow
        -- need to turn it into a multi-dim problem on 6 axes
        | cs ∈ seen = go best seen q
        | otherwise =
            traceShow (l, size seen, size q, n, cs, js) $
              go
                best
                (cs |-> seen)
                ( qAppend
                    loss
                    [ (n + 1, cs')
                    | bi <- range 0 ((bs |.|) - 1),
                      let cs' = press2 cs bs bi,
                      cs' ∉ seen
                    ]
                    q
                )
   in go Nothing (∅) (mkQ₁ loss (0, csE))

part1 :: ℤ = sum (press <$> xs)

slve :: [[ℤ]] -> [ℤ] -> Maybe (ℤ, [ℤ])
slve bs js' = z3 $ minimize "n_min" do
  ns <- sequence [z3IntVar ("n" <> show i) | (i, _) <- enum bs]
  js <- traverse z3Int js'
  preds <-
    forM
      (enum js)
      (\(ji, j) -> z3Eq j =<< z3Add [n | (n, b) <- zip ns bs, ji ∈ b])
  gts <- sequence [z3Ge n =<< z3Int 0 | n <- ns]
  allGts <- z3And gts
  solved <- z3And preds
  -- printAST solved
  z3Assert solved
  z3Assert allGts
  n <- z3Add ns
  pure (n, ns)

part2 :: ℤ
part2 = sum $ sum <$> ([snd <$> (slve bs js) | (_, bs, js) <- xs] <>?)
