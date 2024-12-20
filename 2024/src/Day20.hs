module Day20 (part1, part2) where

-- cheats :: ".#SE" ▦ ℤ² -> (Maybe ℤ², Maybe ℤ²) :|-> ([ℤ²], ℤ)
-- cheats g = go h cap (mkQ₁ h (start, [start], ø, True, Nothing, Nothing)) (ø :: Set (ℤ², 𝔹)) ø
cheats :: ".#SE" ▦ ℤ² -> [ℤ]
cheats g = allSavings
  where
    (start, end) = both (g |!>) (mkC @'S' @".#SE", mkC @'E' @".#SE")
    ((basepath, _) : _) = values $ go (const 0) ꝏ (mkQ₁ (const 0) (start, [start], ø, False, Just start, Just start)) ø (ø :: (Maybe ℤ², Maybe ℤ²) :|-> ([ℤ²], ℤ))
    cap = size basepath
    c2n = traceShowId $ mkMap (zip basepath [0 ..])
    nsCheat c = [n' | n <- neighs @4 c g, n' <- neighs @4 n g, g |! n' ≢ (mkC @'#' @".#SE"), n' ∈ c2n]
    cheatTimes c = [2 + (c2n |! n) | n <- (traceShow ("cheat", c, nsCheat c) $ nsCheat c)]
    timeSaved c = [(c2n |! c) - ct | ct <- cheatTimes c]
    allSavings = [t | c <- basepath, t <- timeSaved c]
    h (c, p, _, _, _, _) = let d = size @[ℤ²] p + manhattan c end in if d > cap then ꝏ else d
    go _ _ NullQ _ res = res
    go h cap (st@(c, p, seen, canCheat, chStart, chEnd) :<!! q) gseen res
      | c ≡ end = go h cap q gseen (res |. ((chStart, chEnd), (p, cap - size p)))
      | c ∈ seen = go h cap q gseen res
      | (c, canCheat) ∈ gseen = go h cap q gseen res
      | (chStart, chEnd) ∈ res = go h cap q gseen res
      | size p ≥ cap = go h cap q gseen res
      | otherwise =
          -- traceShow (cap, size q, size res) $
          let noCheat = [(n, n : p, c |-> seen, canCheat, chStart, chEnd) | n <- neighs @4 c g, g |! n ≢ (mkC @'#' @".#SE")]
              startCheat = [(n, n : p, c |-> seen, False, Just n, Nothing) | n <- neighs @4 c g, g |! n ≡ (mkC @'#' @".#SE")]
              endCheat = [(n, n : p, c |-> seen, False, chStart, Just n) | n <- neighs @4 c g, g |! n ≢ (mkC @'#' @".#SE")]
              q' =
                qAppend
                  h
                  ( case (chStart, chEnd) of
                      (_, Just _) -> noCheat
                      (Nothing, _) -> noCheat <> startCheat
                      (Just _, _) -> endCheat
                  )
                  q
           in go h cap q' ((c, canCheat) |-> gseen) res

part1 =
  let g = readGrid $(aoc 20)
   in -- (g, threshold) = (readGrid $(aocx 20), 1)
      -- ([Σ 1 | (cheat, (time, saved)) <- unMap (cheats g), saved >= threshold] <>!)
      sum [1 | t <- cheats g, t ≥ 100]

part2 :: Text
part2 = "Part 2"
