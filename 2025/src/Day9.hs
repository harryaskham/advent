module Day9 (part1, part2) where

ps :: [ℤ²] = (($(aoc 9) |- parseVia @([CSV ℤ 2] ≠ [])) ⊏)

pSet :: Set ℤ² = mk ps

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

inR l (px, py) =
  traceShow ("inR", (px, py), ((x0, y0), (x1, y1)), i) $ i
  where
    ((rx0, ry0), (rx1, ry1)) = l
    x0 = min rx0 rx1
    x1 = max rx0 rx1
    y0 = min ry0 ry1
    y1 = max ry0 ry1
    i = x0 < px && px < x1 && y0 < py && py < y1

inter :: ℤ² × ℤ² -> ℤ² × ℤ² -> [ℤ²]
inter r p
  | rx0 == rx1 && py0 == py1 && py0 <= max ry0 ry1 && py0 >= min ry0 ry1 && min px0 px1 <= rx0 && max px0 px1 >= rx0 = [(rx0, py0)]
  | ry0 == ry1 && px0 == px1 && px0 <= max rx0 rx1 && px0 >= min rx0 rx1 && min py0 py1 <= ry0 && max py0 py1 >= ry0 = [(px0, ry0)]
  | rx0 == rx1 && px0 == px1 && rx0 == px0 && min px0 px1 <= max rx0 rx1 && min rx0 rx1 <= max px0 px1 = [(rx0, y) | y <- [max ry0 py0 .. min ry1 py1]]
  | ry0 == ry1 && py0 == py1 && ry0 == py0 && min py0 py1 <= max ry0 ry1 && min ry0 ry1 <= max py0 py1 = [(x, ry0) | x <- [max rx0 px0 .. min rx1 px1]]
  | otherwise = []
  where
    ((rx0, ry0), (rx1, ry1)) = r
    ((px0, py0), (px1, py1)) = p

v :: [[ℤ²]] -> 𝔹
v inters = traceShow inters $ (inters <>!) == []

sortLine :: ℤ² × ℤ² -> ℤ² × ℤ²
sortLine (a, b)
  | fst a > fst b || (fst a == fst b && snd a > snd b) = (b, a)
  | otherwise = (a, b)

sortLines :: [ℤ² × ℤ²] -> [ℤ² × ℤ²]
sortLines = nub . sort . fmap sortLine

circuit :: [ℤ²] -> [ℤ² × ℤ²]
circuit ps' = let ps = nub ps' in sortLines $ pairs (ps ++ [ps !! 0])

rects = reverse $ sort [(ds² $@ r, r) | r <- triPairs ps]

rlines r@((rx0, ry0), (rx1, ry1)) = circuit [(rx0, ry0), (rx1, ry0), (rx1, ry1), (rx0, ry1)]

-- onPerim r = and [(interPerim r |.|) ≡ ds² $@ l | l <- rlines r]

hlines :: [[ℤ × ℤ²]]
hlines = groupOn fst $ sortOn fst [(y0, (min x0 x1, max x0 x1)) | ((x0, y0), (x1, y1)) <- circuit ps, y0 ≡ y1]

hlines' :: [ℤ × [ℤ²]]
hlines' = [(fst (yLs !! 0), snd <$> yLs) | yLs <- hlines]

vlines :: [[ℤ × ℤ²]]
vlines = groupOn fst $ sortOn fst [(x0, (min y0 y1, max y0 y1)) | ((x0, y0), (x1, y1)) <- circuit ps, x0 ≡ x1]

vlines' :: [ℤ × [ℤ²]]
vlines' = [(fst (yLs !! 0), snd <$> yLs) | yLs <- vlines]

addLine (a, b) [] = [(a, b)]
addLine (a, b) ((c, d) : rest)
  | a ≡ d ∨ b ≡ c = (min a c, max b d) : rest
  | a ≡ c ∧ b > d = rest
  | a ≡ c = (b, d) : rest
  | b ≡ d ∧ a < c = rest
  | b ≡ d = (c, a) : rest
  | otherwise = (c, d) : addLine (a, b) rest

addLine' y (a, b) [] = [(y, (a, b))]
addLine' y (a, b) ((y', (c, d)) : rest)
  | a ≡ d ∨ b ≡ c = (y, (min a c, max b d)) : rest
  | a ≡ c ∧ b > d = rest
  | a ≡ c = (y, (b, d)) : rest
  | b ≡ d ∧ a < c = rest
  | b ≡ d = (y, (a, c)) : rest
  | otherwise = (y', (c, d)) : addLine' y (a, b) rest

goo (y, active) [] = 0
goo (y, active) ((y', lines) : groups) =
  traceShow (y, active) $
    let active' = mergeGroup (foldl' (\active (x0, x1) -> addLine (min x0 x1, max x0 x1) active) active lines)
     in max
          ( maximum
              ( 0
                  : [ (max x0 x1 - min x0 x1 + 1) ⋅ (max y y' - min y y' + 1)
                    | (x0, x1) <- lines,
                      (ax0, ax1) <- active,
                      min x0 x1 ≤ max ax0 ax1 ∧ max x0 x1 ≥ min ax0 ax1
                    ]
              )
          )
          (goo (y', active') groups)

goo1 lines' =
  let (y, active) : rest = lines'
   in goo (y, active) rest

gooAll = maximum (goo1 <$> [hlines', vlines', reverse hlines', reverse vlines'])

gooo active [] = 0
gooo active (yLs : groups) =
  traceShow (active) $
    let active' = foldl' (\active (y', (x0, x1)) -> addLine' y' (min x0 x1, max x0 x1) active) active yLs
     in max
          ( maximum
              ( 0
                  : [ (max x0 x1 - min x0 x1 + 1) ⋅ (max y y' - min y y' + 1)
                    | (y', (x0, x1)) <- yLs,
                      let mergedActive = mergeGroup (snd <$> active),
                      let y = minimum ((y' - 1) : [y | (y, (ax0, ax1)) <- active, min x0 x1 ≤ max ax0 ax1 ∧ max x0 x1 ≥ min ax0 ax1]),
                      (ax0, ax1) <- mergedActive,
                      min x0 x1 ≤ max ax0 ax1 ∧ max x0 x1 ≥ min ax0 ax1
                    ]
              )
          )
          (gooo active' groups)

gooo1 lines' =
  let active : rest = lines'
   in gooo active rest

goooAll = maximum (gooo1 <$> [hlines, vlines, reverse hlines, reverse vlines])

mergeGroup [] = []
mergeGroup [x] = [x]
mergeGroup ((a, b) : (c, d) : rest)
  | b >= c ∧ a ≤ d = mergeGroup ((a, d) : rest)
  | otherwise = (a, b) : mergeGroup ((c, d) : rest)

go [] = 0
go (frontier : groups) =
  max
    ( maximum
        ( 0
            : ( fst
                  <$> takeWhile
                    (not ∘ snd)
                    [ (a, inter)
                    | (y, (x0, x1)) <- frontier,
                      group <- groups,
                      (ly, (lx0, lx1)) <- group,
                      let hitMinMin = min x0 x1 ≡ min lx0 lx1,
                      let hitMinMax = min x0 x1 ≡ max lx0 lx1,
                      let hitMaxMin = max x0 x1 ≡ min lx0 lx1,
                      let hitMaxMax = max x0 x1 ≡ max lx0 lx1,
                      let hit = hitMinMax ∨ hitMaxMin ∨ hitMinMin ∨ hitMaxMax,
                      let cont = hitMinMin ∨ hitMaxMax,
                      let inter = max lx0 lx1 > min x0 x1 ∧ max x0 x1 > min lx0 lx1,
                      let a = (max x0 x1 - min x0 x1 + 1) ⋅ (max y ly - min y ly + 1),
                      traceShow ("f", (y, (x0, x1)), "l", (ly, (lx0, lx1)), "hit", hit, "cont", cont, "inter", inter, "a", a) True,
                      hit
                    ]
              )
        )
    )
    (go groups)

goAll = maximum (go <$> [hlines, vlines]) -- , reverse hlines, reverse vlines])

plines = circuit ps

perimPs = mkSet [(x, y) | ((x0, y0), (x1, y1)) <- plines, x <- [min x0 x1 .. max x0 x1], y <- [min y0 y1 .. max y0 y1]]

start = let (((y, (x0, x1)) : _) : _) = hlines in (x0 + 1, y + 1)

flood seen Empty = seen
flood seen (c :<| q)
  | c ∈ seen = flood seen q
  | otherwise =
      traceShow (size seen) $
        let seen' = c |-> seen
            q' = foldl' (\q n -> q |> n) q (neighborsNoDiags c)
         in flood seen' q'

allPs = flood perimPs (mkSeq [start])

validR ((x0, y0), (x1, y1)) = and [(x, y) ∈ allPs | x <- [min x0 x1 .. max x0 x1], y <- [min y0 y1 .. max y0 y1]]

part2 = head' [a | (a, r) <- rects, traceShow (a, r) $ validR r]

-- 4455021870 not it
-- 4557681710 not it
-- 1643752071 too low
-- 158481360 too low
-- 114894852 too low
-- 111538071 not it
part2'' :: ℤ =
  let plines = circuit ps
      n = size (triPairs ps)
   in traceShow ("perim", plines) $
        ( head' $
            [ a
            | (i, (a, r@((rx0, ry0), (rx1, ry1)))) <- enum rects,
              let rlines = circuit [(rx0, ry0), (rx1, ry0), (rx1, ry1), (rx0, ry1)],
              let ins = inR r <$> ps,
              let its = [nub (inter r =<< plines) | r <- rlines],
              traceShow ("r", r, "a", ds² $@ r, "rlines", rlines, "ins", ins, "its", its) $ True
              -- not (or ins)
              -- its == corners
            ]
        )

part2' :: ℤ =
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
