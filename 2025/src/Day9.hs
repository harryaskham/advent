module Day9 (part1, part2) where

ps :: [ℤ²] = (($(aocx 9) |- parseVia @([CSV ℤ 2] ≠ [])) ⊏)

part1 :: ℤ = maximum (ds² <$@> triPairs ps)

inter' :: ℤ² × ℤ² -> ℤ² × ℤ² -> 𝔹
inter' r p
  | r == p = False
  | rx0 == rx1 && px0 == px1 && rx0 == px0 && py0 < ry1 && py1 > ry0 = True
  | ry0 == ry1 && py0 == py1 && ry0 == py0 && px0 < rx1 && px1 > rx0 = True
  | rx0 == rx1 && py0 == py1 && py0 >= ry0 && py1 <= ry1 && px0 <= rx0 && px1 >= rx0 = True
  | ry0 == ry1 && px0 == px1 && px0 >= rx0 && px1 <= rx1 && py0 <= ry0 && py1 >= ry0 = True
  | otherwise = False
  where
    ((rx0, ry0), (rx1, ry1)) = r
    ((px0, py0), (px1, py1)) = p

inter :: ℤ² × ℤ² -> ℤ² × ℤ² -> [ℤ²]
inter r p
  -- \| rx0 == rx1 && px0 == px1 && rx0 == px0 && py0 <= ry1 && py1 >= ry0 = [(rx0, y) | y <- [max ry0 py0 .. min ry1 py1]]
  -- \| ry0 == ry1 && py0 == py1 && ry0 == py0 && px0 <= rx1 && px1 >= rx0 = [(x, ry0) | x <- [max rx0 px0 .. min rx1 px1]]
  | rx0 == rx1 && py0 == py1 && py0 >= ry0 && py1 <= ry1 && px0 <= rx0 && px1 >= rx0 =
      -- traceShowId $
      [(rx0, py0)]
  | ry0 == ry1 && px0 == px1 && px0 >= rx0 && px1 <= rx1 && py0 <= ry0 && py1 >= ry0 =
      -- traceShowId $
      [(px0, ry0)]
  | otherwise -- traceShowId $
    =
      []
  where
    ((rx0, ry0), (rx1, ry1)) = r
    ((px0, py0), (px1, py1)) = p

v :: [[ℤ²]] -> 𝔹
v inters = traceShow inters $ (inters <>!) == []

sortLine :: ℤ² × ℤ² -> ℤ² × ℤ²
sortLine (a, b)
  | fst a > fst b || (fst a == fst b && snd a > snd b) = (b, a)
  | otherwise = (a, b)

circuit :: [ℤ²] -> [ℤ² × ℤ²]
circuit ps' = let ps = nub ps' in nub $ sortLine <$> pairs (ps ++ [ps !! 0])

-- 1643752071 too low
-- 158481360 too low
part2 :: ℤ =
  let plines = circuit ps
      n = size (triPairs ps)
   in traceShow ("perim", plines) $
        ( head' $
            [ a
            | (i, (a, r@((rx0, ry0), (rx1, ry1)))) <- enum (reverse $ sort [(ds² $@ r, r) | r <- triPairs ps]),
              let rlines =
                    -- traceShow ("r", r, "a", ds² $@ r) $
                    traceShow (i, n, r) $
                      circuit [(rx0, ry0), (rx1, ry0), (rx1, ry1), (rx0, ry1)],
              let its =
                    -- traceShow ("rlines", rlines) $
                    traceShowId
                      [ -- traceShow (r, p, is) $
                      inter r =<< plines | r <- rlines
                      ],
              -- not (or inters),
              let ns = size <$> its,
              let vns = [n <= 1 | n <- ns],
              let vn2s = [n `mod` 2 == 0 | n <- ns, n > 1],
              or vns && or vn2s && all (all (== 1)) (ds² <$$@> (toTuple2 <$$> (chunksOf 2 <$> its)))
              -- v inters
              -- all (== True) inters
            ]
        )
