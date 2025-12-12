module Day10 where

xs :: [(𝕊, [[ℤ]], [ℤ])] =
  $(aoc 10)
    & (⋮⊏)
      @( [ TupSep
             " "
             ( (Between "[" "]" (𝕊 ⭀ ".#")),
               [Between "(" ")" (CSVAny ℤ)],
               (Between "{" "}" (CSVAny ℤ))
             )
         ]
           ≠ []
       )

part1 :: ℤ =
  let tog '#' = '.'
      tog '.' = '#'
      press ls bs bi = ((Ł (\ls li -> ls !. (li, tog (ls !! li))) ls (bs !! bi)) !>)
      go' (ls', bs, _) =
        let lsE = const '.' <$> ls'
            go seen ((n, ls) :<| q)
              | ls == ls' = n
              | ls ∈ seen = go seen q
              | otherwise = go (ls |-> seen) (foldl' (\q bi -> q |> (n + 1, press ls bs bi)) q (range 0 ((bs |.|) - 1)))
         in go (∅) (mk₁ (0, lsE))
   in sum (go' <$> xs)

part2 :: ℤ =
  let go :: [[ℤ]] -> [ℤ] -> Maybe (ℤ, [ℤ])
      go bs js' = z3 $ minimize "n_min" do
        ns <- sequence [z3IntVar ("n" <> show i) | (i, _) <- enum bs]
        n <- z3Add ns
        js <- traverse z3Int js'
        preds <-
          sequence
            [ (forM ns (\n -> z3Ge n =<< z3Int 0)),
              (forM (enum js) (\(ji, j) -> z3Eq j =<< z3Add [n | (n, b) <- zip ns bs, ji ∈ b]))
            ]
        z3Assert =<< z3And (preds <>!)
        pure (n, ns)
   in sum $ sum <$> ([snd <$> (go bs js) | (_, bs, js) <- xs] <>?)
